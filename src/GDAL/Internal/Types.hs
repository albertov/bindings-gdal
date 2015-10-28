{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.Types (
    Value(..)
  , GDAL
  , GDALConduit
  , GDALSource
  , GDALSink
  , Window (..)
  , XY (..)
  , Size
  , BlockIx
  , winSize
  , sizeLen
  , AccessMode
  , ReadWrite
  , ReadOnly
  , uToStValue
  , stToUValue
  , isNoData
  , fromValue
  , runGDAL
  , allocate
  , unprotect
  , release
  , unsafeGDALToIO
) where

import Control.Applicative (Applicative(..), liftA2)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Resource (
    ResourceT
  , MonadResource
  , runResourceT
  , allocate
  , release
  , unprotect
  , runInternalState
  , getInternalState
  )
import Control.Monad.Catch (
    MonadThrow(..)
  , MonadCatch
  , MonadMask
  , catch
  )
import Control.Monad.IO.Class (MonadIO(..))

import Data.Typeable (Typeable)
import Data.Coerce (coerce)
import Data.Conduit (Conduit, Sink, Source)
import Data.Word (Word8)

import qualified Data.Vector.Storable as St
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))

import GDAL.Internal.CPLError


type GDALConduit s i o = Conduit i (GDAL s) o
type GDALSource s o = Source (GDAL s) o
type GDALSink s i = Sink i (GDAL s)

data AccessMode = ReadOnly | ReadWrite

type ReadOnly  = 'ReadOnly
type ReadWrite = 'ReadWrite

data XY a = XY {px :: !a, py :: !a} deriving (Eq, Ord, Show, Read, Typeable)

instance NFData a => NFData (XY a) where
  rnf (XY a b) = rnf a `seq` rnf b `seq` ()

data Window a
  = Window {
      winMin :: !(XY a)
    , winMax :: !(XY a)
    }
  deriving (Eq, Show, Read, Functor, Typeable)

instance NFData a => NFData (Window a) where
  rnf (Window a b) = rnf a `seq` rnf b `seq` ()

winSize :: Num a => Window a -> XY a
winSize w = liftA2 (-) (winMax w) (winMin w)
{-# INLINE winSize #-}

sizeLen :: Size -> Int
sizeLen (XY x y) = x*y
{-# INLINE sizeLen #-}

type Size    = XY Int
type BlockIx = XY Int


instance Functor XY where
  fmap f (XY a b) = XY (f a) (f b)
  {-# INLINE fmap #-}
#if MIN_VERSION_base(4,8,0)
  a <$ _ = XY a a
  {-# INLINE (<$) #-}
#endif

instance Applicative XY where
  pure a = XY a a
  {-# INLINE pure #-}
  XY a b <*> XY d e = XY (a d) (b e)
  {-# INLINE (<*>) #-}



data Value a
  = Value {unValue :: !a}
  | NoData
  deriving (Eq, Ord, Show, Read)

instance NFData a => NFData (Value a) where
  rnf (Value a) = rnf a `seq` ()
  rnf NoData    = ()
  {-# INLINE rnf #-}

instance Functor Value where
  fmap _ NoData       = NoData
  fmap f (Value a)    = Value (f a)
  {-# INLINE fmap #-}

instance Applicative Value where
  pure = Value
  {-# INLINE pure #-}

  Value f <*> m       = fmap f m
  NoData  <*> _m      = NoData
  {-# INLINE (<*>) #-}

  Value _m1 *> m2     = m2
  NoData    *> _m2    = NoData
  {-# INLINE (*>) #-}

instance Monad Value where
  (Value x) >>= k     = k x
  NoData    >>= _     = NoData
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

  return              = Value
  {-# INLINE return #-}
  fail _              = NoData
  {-# INLINE fail #-}

instance Num a => Num (Value a) where
  Value a + Value b = Value (a+b)
  Value a + NoData  = Value a
  NoData  + Value a = Value a
  NoData  + NoData  = NoData
  {-# INLINE (+) #-}

  Value a - Value b = Value (a-b)
  Value a - NoData  = Value a
  NoData  - Value a = Value a
  NoData  - NoData  = NoData
  {-# INLINE (-) #-}

  Value a * Value b = Value (a*b)
  Value a * NoData  = Value a
  NoData  * Value a = Value a
  NoData  * NoData  = NoData
  {-# INLINE (*) #-}

  negate = fmap negate
  {-# INLINE negate #-}

  abs = fmap abs
  {-# INLINE abs #-}

  signum = fmap signum
  {-# INLINE signum #-}

  fromInteger = Value . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Value a) where
  Value a / Value b = Value (a/b)
  Value a / NoData  = Value a
  NoData  / Value a = Value a
  NoData  / NoData  = NoData
  {-# INLINE (/) #-}

  recip = fmap recip
  {-# INLINE recip #-}

  fromRational = Value . fromRational
  {-# INLINE fromRational #-}

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




newtype GDAL s a = GDAL (ResourceT IO a)

deriving instance Functor (GDAL s)
deriving instance Applicative (GDAL s)
deriving instance Monad (GDAL s)
deriving instance MonadIO (GDAL s)
deriving instance MonadThrow (GDAL s)
deriving instance MonadCatch (GDAL s)
deriving instance MonadMask (GDAL s)
deriving instance MonadBase IO (GDAL s)
deriving instance MonadResource (GDAL s)

runGDAL :: (forall s. GDAL s a) -> IO (Either GDALException a)
runGDAL (GDAL a) = withErrorHandler $ runResourceT $
    (liftM Right a) `catch` (return . Left)

unsafeGDALToIO :: GDAL s a -> GDAL s (IO a)
unsafeGDALToIO (GDAL act) = do
  state <- GDAL getInternalState
  return (runInternalState act state)
