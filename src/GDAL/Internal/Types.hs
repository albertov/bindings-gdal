{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad.Base (MonadBase)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT(runReaderT), ask)
import Control.Monad.Trans.Control (MonadBaseControl(..))
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
  pure a = (a :+: a)
  {-# INLINE pure #-}
  (a :+: b) <*> (d :+: e) = a d :+: b e
  {-# INLINE (<*>) #-}

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
deriving instance MonadBase IO (GDAL s)
deriving instance MonadFix (GDAL s)

instance MonadResource (GDAL s) where
  liftResourceT act = liftIO . runInternalState act . unState =<< getInternalState

instance MonadBaseControl IO (GDAL s) where
  type StM (GDAL s) a = a
  liftBaseWith runInBase = do
    state <- getInternalState
    liftIO $ runInBase (`runWithInternalState` state)
  restoreM = return

runGDAL :: NFData a => (forall s. GDAL s a) -> IO (Either GDALException a)
runGDAL a = runBounded $
  bracket createGDALInternalState closeGDALInternalState
  (try . evaluate . force <=< runWithInternalState a)

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
