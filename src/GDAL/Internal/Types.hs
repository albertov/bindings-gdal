{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module GDAL.Internal.Types (
    Value(..)
  , uToStValue
  , stToUValue
  , isNoData
  , fromValue

  , GDAL
  , runGDAL
  , gdalForkIO
  , registerFinalizer
) where

import Control.Applicative
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, newEmptyMVar)
import Control.Exception (evaluate)
import Control.DeepSeq (NFData(rnf), force)
import Control.Monad (liftM, void)
import Control.Monad.Trans.Resource (
    ResourceT
  , runResourceT
  , register
  , resourceForkIO
  )
import Control.Monad.Catch (MonadThrow(..), MonadCatch, MonadMask, finally)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Data.Coerce (coerce)
import Data.Word (Word8)

import qualified Data.Vector.Storable as St
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))

data Value a
  = Value {unValue :: !a}
  | NoData
  deriving (Eq, Show, Read)

instance NFData a => NFData (Value a) where
  rnf (Value a) = rnf a `seq` ()
  rnf NoData    = ()

instance  Functor Value  where
    fmap _ NoData       = NoData
    fmap f (Value a)    = Value (f a)

instance Applicative Value where
    pure = Value

    Value f <*> m       = fmap f m
    NoData  <*> _m      = NoData

    Value _m1 *> m2     = m2
    NoData    *> _m2    = NoData

instance Monad Value  where
    (Value x) >>= k     = k x
    NoData    >>= _     = NoData

    (>>) = (*>)

    return              = Value
    fail _              = NoData

type FlagType = Word8

instance Storable a => Storable (Value a) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: FlagType)
  alignment _ = alignment (undefined :: a)
  peek p = let pm = castPtr p :: Ptr FlagType
               pv = pm `plusPtr` sizeOf (undefined :: FlagType)
           in do t <- peek pm
                 if t/=0
                   then fmap Value (peek (castPtr pv))
                   else return NoData
  poke p x = let pm = castPtr p :: Ptr FlagType
                 pv = pm `plusPtr` sizeOf (undefined :: FlagType)
             in case x of
                  NoData  -> poke pm 0
                  Value a -> poke pm 1 >> poke (castPtr pv) a
  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE peek #-}
  {-# INLINE poke #-}

isNoData :: Value a -> Bool
isNoData NoData = True
isNoData _      = False
{-# INLINE isNoData #-}

fromValue :: a -> Value a -> a
fromValue v NoData    = v
fromValue _ (Value v) = v
{-# INLINE fromValue #-}

newtype instance U.Vector    (Value a) =  V_Value (St.Vector (Value a))
newtype instance U.MVector s (Value a) = MV_Value (St.MVector s (Value a))
instance Storable a => U.Unbox (Value a)

stToUValue :: Storable a => St.Vector (Value a) -> U.Vector (Value a)
stToUValue = coerce
{-# INLINE stToUValue #-}

uToStValue :: Storable a => U.Vector (Value a) -> St.Vector (Value a)
uToStValue = coerce
{-# INLINE uToStValue #-}

instance Storable a => M.MVector U.MVector (Value a) where
  basicLength (MV_Value v ) = M.basicLength v
  basicUnsafeSlice m n (MV_Value v) = MV_Value (M.basicUnsafeSlice m n v)
  basicOverlaps (MV_Value v) (MV_Value u) = M.basicOverlaps v u
  basicUnsafeNew = liftM MV_Value . M.basicUnsafeNew
  basicUnsafeRead (MV_Value v) i = M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Value v) i x = M.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Value v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance Storable a => G.Vector U.Vector (Value a) where
  basicUnsafeFreeze (MV_Value v)   = liftM  V_Value (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_Value v)   = liftM MV_Value (G.basicUnsafeThaw   v)
  basicLength       ( V_Value v)   = G.basicLength v
  basicUnsafeSlice m n (V_Value v) = V_Value (G.basicUnsafeSlice m n v)
  basicUnsafeIndexM (V_Value v) i  = G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}




newtype GDAL s a = GDAL (ResourceT (ReaderT (MVar [MVar ()]) IO) a)

deriving instance Functor (GDAL s)
deriving instance Applicative (GDAL s)
deriving instance Monad (GDAL s)
deriving instance MonadIO (GDAL s)
deriving instance MonadThrow (GDAL s)
deriving instance MonadCatch (GDAL s)
deriving instance MonadMask (GDAL s)

runGDAL :: NFData a => (forall s. GDAL s a) -> IO a
runGDAL (GDAL a) = do
  children <- newMVar []
  runReaderT (runResourceT (finally (a >>= liftIO . evaluate . force)
                                    (liftIO (waitForChildren children))))
             children

  where
    waitForChildren children = do
      cs <- takeMVar children
      case cs of
        []   -> return ()
        m:ms -> do
           putMVar children ms
           _ <- takeMVar m
           waitForChildren children

gdalForkIO :: GDAL s () -> GDAL s ThreadId
gdalForkIO (GDAL a) = GDAL $ do
  children <- ask
  mvar <- liftIO $ do
    childs <- takeMVar children
    mvar <- newEmptyMVar
    putMVar children (mvar:childs)
    return mvar
  resourceForkIO (finally a (liftIO (putMVar mvar ())))


registerFinalizer :: IO () -> GDAL s ()
registerFinalizer = GDAL . void . register
