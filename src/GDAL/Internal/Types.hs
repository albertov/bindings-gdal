{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.Types (
    GDAL
  , GDALInternalState
  , Pair (..)
  , pFst
  , pSnd
  , Size
  , BlockIx
  , ReleaseKey
  , AccessMode
  , ReadWrite
  , ReadOnly
  , runGDAL
  , unsafeRunGDAL
  , runGDAL_
  , allocate
  , allocateGDAL
  , unprotect
  , release
  , sizeLen
  , createGDALInternalState
  , closeGDALInternalState
  , getInternalState
  , runWithInternalState
) where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.DeepSeq (NFData(rnf), force)
import Control.Exception (evaluate, bracket, try, throw)
import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT(runReaderT), ask)
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Trans.Resource (
    MonadResource(liftResourceT)
  , ReleaseKey
  , InternalState
  , allocate
  , release
  , unprotect
  , runInternalState
  , createInternalState
  , closeInternalState
  )
import Control.Monad.Catch (
    MonadThrow
  , MonadCatch
  , MonadMask
  )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed.Base as U

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

import GDAL.Internal.CPLError
import GDAL.Internal.Util (runBounded)


data AccessMode = ReadOnly | ReadWrite

type ReadOnly  = 'ReadOnly
type ReadWrite = 'ReadWrite

infixl 2 :+:

data Pair a = !a :+: !a
  deriving (Eq, Ord, Show, Read, Typeable)

pFst, pSnd :: Pair a -> a
pFst (a :+: _) = a
pSnd (_ :+: a) = a
{-# INLINE pFst #-}
{-# INLINE pSnd #-}

instance NFData (Pair a) where
  rnf (_ :+: _) = ()

type Size    = Pair Int

sizeLen :: Size -> Int
sizeLen (x :+: y) = x*y
{-# INLINE sizeLen #-}

type BlockIx = Pair Int


instance Functor Pair where
  fmap f (a :+: b) = f a :+: f b
  {-# INLINE fmap #-}
#if MIN_VERSION_base(4,8,0)
  a <$ _ = (a :+: a)
  {-# INLINE (<$) #-}
#endif

instance Applicative Pair where
  pure a = a :+: a
  {-# INLINE pure #-}
  (a :+: b) <*> (d :+: e) = a d :+: b e
  {-# INLINE (<*>) #-}

instance Foldable Pair where
  foldMap f (a :+: b) = f a `mappend` f b
  {-# INLINE foldMap #-}

instance Traversable Pair where
  traverse f (a :+: b) = (:+:) <$> f a <*> f b
  {-# INLINE traverse #-}

instance Num a => Num (Pair a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Pair a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (Pair a) where
    pi = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    log = fmap log
    {-# INLINE log #-}
    (**) = liftA2 (**)
    {-# INLINE (**) #-}
    logBase = liftA2 logBase
    {-# INLINE logBase #-}
    sin = fmap sin
    {-# INLINE sin #-}
    tan = fmap tan
    {-# INLINE tan #-}
    cos = fmap cos
    {-# INLINE cos #-}
    asin = fmap asin
    {-# INLINE asin #-}
    atan = fmap atan
    {-# INLINE atan #-}
    acos = fmap acos
    {-# INLINE acos #-}
    sinh = fmap sinh
    {-# INLINE sinh #-}
    tanh = fmap tanh
    {-# INLINE tanh #-}
    cosh = fmap cosh
    {-# INLINE cosh #-}
    asinh = fmap asinh
    {-# INLINE asinh #-}
    atanh = fmap atanh
    {-# INLINE atanh #-}
    acosh = fmap acosh
    {-# INLINE acosh #-}

instance Storable a => Storable (Pair a) where
  sizeOf _ = 2 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (x :+: y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = (:+:) <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

newtype GDAL s a = GDAL {unGDAL :: ReaderT InternalState IO a}

deriving instance Functor (GDAL s)
deriving instance Applicative (GDAL s)
deriving instance Monad (GDAL s)
deriving instance MonadIO (GDAL s)
deriving instance MonadThrow (GDAL s)
deriving instance MonadCatch (GDAL s)
deriving instance MonadMask (GDAL s)
deriving instance MonadFix (GDAL s)

instance MonadResource (GDAL s) where
  liftResourceT act = liftIO . runInternalState act . unState =<< getInternalState

instance MonadUnliftIO (GDAL s) where
  withRunInIO inner = liftIO (inner unsafeRunGDAL)
  {-# INLINE withRunInIO #-}

runGDAL :: NFData a => (forall s. GDAL s a) -> IO (Either GDALException a)
runGDAL a = runBounded $
  bracket createGDALInternalState closeGDALInternalState
  (try . (evaluate . force <=< runWithInternalState a))

unsafeRunGDAL :: GDAL s a -> IO a
unsafeRunGDAL
  = runBounded
  . bracket createGDALInternalState closeGDALInternalState
  . runWithInternalState

createGDALInternalState :: IO (GDALInternalState s)
createGDALInternalState = GDALInternalState <$> createInternalState

closeGDALInternalState :: GDALInternalState s -> IO ()
closeGDALInternalState = closeInternalState . unState

runGDAL_ :: NFData a => (forall s. GDAL s a) -> IO a
runGDAL_ a = either throw return =<< runGDAL a

newtype GDALInternalState s =
  GDALInternalState { unState :: InternalState }

runWithInternalState
  :: GDAL s a -> GDALInternalState s -> IO a
runWithInternalState (GDAL a) = runReaderT a . unState

allocateGDAL
  :: GDAL s a
  -> (a -> GDAL s ())
  -> GDAL s (ReleaseKey, a)
allocateGDAL (GDAL alloc) free = do
  GDALInternalState state <- getInternalState
  allocate (runReaderT alloc state) (flip runReaderT state . unGDAL . free)

getInternalState :: GDAL s (GDALInternalState s)
getInternalState = GDALInternalState <$> GDAL ask


newtype instance U.Vector    (Pair a) =
    V_Value (U.Vector a, U.Vector a)
newtype instance U.MVector s (Pair a) =
  MV_Value (U.MVector s a, U.MVector s a)

instance U.Unbox a => U.Unbox (Pair a)

instance U.Unbox a => M.MVector U.MVector (Pair a) where

  basicLength (MV_Value (a,_)) = M.basicLength a
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MV_Value (a, b)) =
    MV_Value ( M.basicUnsafeSlice m n a
             , M.basicUnsafeSlice m n b)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MV_Value (a,b)) (MV_Value (a',b')) =
    M.basicOverlaps a a' && M.basicOverlaps b b'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i =
    curry MV_Value <$> M.basicUnsafeNew i <*> M.basicUnsafeNew i
  {-# INLINE basicUnsafeNew #-}


  basicUnsafeRead (MV_Value (a,b)) i =
    (:+:) <$> M.basicUnsafeRead a i <*> M.basicUnsafeRead b i
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MV_Value (a,b)) i (a':+:b') = do
    M.basicUnsafeWrite a i a'
    M.basicUnsafeWrite b i b'
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Value (a,b)) = do
    M.basicInitialize a
    M.basicInitialize b
  {-# INLINE basicInitialize #-}
#endif

instance U.Unbox a => G.Vector U.Vector (Pair a) where

  basicUnsafeFreeze (MV_Value (a,b)) =
    curry V_Value <$> G.basicUnsafeFreeze a <*> G.basicUnsafeFreeze b
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (V_Value (a,b)) =
    curry MV_Value <$> G.basicUnsafeThaw a <*> G.basicUnsafeThaw b
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (V_Value (a,_)) = G.basicLength a
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (V_Value (a,b)) =
    V_Value (G.basicUnsafeSlice m n a, G.basicUnsafeSlice m n b)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (V_Value (a,b)) i =
    (:+:) <$> G.basicUnsafeIndexM a i <*> G.basicUnsafeIndexM b i
  {-# INLINE basicUnsafeIndexM #-}
