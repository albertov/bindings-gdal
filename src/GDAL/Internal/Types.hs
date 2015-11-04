{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , XY (..)
  , Size
  , BlockIx
  , ReleaseKey
  , AccessMode
  , ReadWrite
  , ReadOnly
  , isNoData
  , fromValue
  , runGDAL
  , execGDAL
  , allocate
  , unprotect
  , release
  , sizeLen
  , unsafeGDALToIO
  , mkValueUVector
  , mkMaskedValueUVector
  , mkAllValidValueUVector
  , maskAndValueVectors
) where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.DeepSeq (NFData(rnf), force)
import Control.Exception (evaluate)
import Control.Monad (liftM, liftM2)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Resource (
    ResourceT
  , MonadResource
  , ReleaseKey
  , runResourceT
  , allocate
  , release
  , unprotect
  , runInternalState
  , getInternalState
  )
import Control.Monad.Catch (
    MonadThrow
  , MonadCatch
  , MonadMask
  )
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Typeable (Typeable)
import Data.Word (Word8)

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Unboxed.Base as U

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

import GDAL.Internal.CPLError



data AccessMode = ReadOnly | ReadWrite

type ReadOnly  = 'ReadOnly
type ReadWrite = 'ReadWrite

data XY a =
  XY { px :: {-# UNPACK #-} !a
     , py :: {-# UNPACK #-} !a
     } deriving (Eq, Ord, Show, Read, Typeable)

instance NFData a => NFData (XY a) where
  rnf (XY a b) = rnf a `seq` rnf b `seq` ()

type Size    = XY Int

sizeLen :: Size -> Int
sizeLen (XY x y) = x*y
{-# INLINE sizeLen #-}

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

instance Num a => Num (XY a) where
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

instance Fractional a => Fractional (XY a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (XY a) where
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

instance Storable a => Storable (XY a) where
  sizeOf _ = 2 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (XY x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = XY <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}


data Value a
  = Value {unValue :: {-# UNPACK #-} !a}
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

isNoData :: Value a -> Bool
isNoData NoData = True
isNoData _      = False
{-# INLINE isNoData #-}

fromValue :: a -> Value a -> a
fromValue v NoData    = v
fromValue _ (Value v) = v
{-# INLINE fromValue #-}

maskValid, maskNoData :: Word8
maskValid  = 255
maskNoData = 0

mkMaskedValueUVector
  :: Storable a => St.Vector Word8 -> St.Vector a -> U.Vector (Value a)
mkMaskedValueUVector mask values = V_Value (mask, values)
{-# INLINE mkMaskedValueUVector #-}

mkAllValidValueUVector
  :: Storable a => St.Vector a -> U.Vector (Value a)
mkAllValidValueUVector values =
  V_Value (St.replicate (St.length values) maskValid, values)
{-# INLINE mkAllValidValueUVector #-}

mkValueUVector
  :: Storable a => (a -> Value a) -> St.Vector a -> U.Vector (Value a)
mkValueUVector fun values = V_Value (St.map mkMask values, values)
  where mkMask v = if isNoData (fun v) then maskNoData else maskValid
{-# INLINE mkValueUVector #-}


newtype instance U.Vector    (Value a) =
    V_Value { maskAndValueVectors :: (St.Vector Word8, St.Vector a) }
newtype instance U.MVector s (Value a) =
  MV_Value (St.MVector s Word8, St.MVector s a)
instance Storable a => U.Unbox (Value a)


instance Storable a => M.MVector U.MVector (Value a) where
  basicLength (MV_Value (_,v)) =
    M.basicLength v
  basicUnsafeSlice m n (MV_Value (x,v)) =
    MV_Value ( M.basicUnsafeSlice m n x
             , M.basicUnsafeSlice m n v)
  basicOverlaps (MV_Value (_,v)) (MV_Value (_,v')) =
    M.basicOverlaps v v'
  basicUnsafeNew i =
    liftM2 (\x v -> MV_Value (x,v))
           (M.basicUnsafeNew i)
           (M.basicUnsafeNew i)
  basicUnsafeRead (MV_Value (x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m/=maskNoData
       then liftM Value (M.basicUnsafeRead v i)
       else return NoData
  basicUnsafeWrite (MV_Value (x,v)) i a =
    case a of
      NoData ->
        M.basicUnsafeWrite x i maskNoData
      Value a' -> do
        M.basicUnsafeWrite x i maskValid
        M.basicUnsafeWrite v i a'
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Value (x,v)) = do
    M.basicInitialize x
    M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance Storable a => G.Vector U.Vector (Value a) where
  basicUnsafeFreeze (MV_Value (x,v)) =
    liftM2 (\x' v' -> V_Value (x',v'))
           (G.basicUnsafeFreeze x)
           (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Value (x,v)) =
    liftM2 (\x' v' -> MV_Value (x',v'))
           (G.basicUnsafeThaw x)
           (G.basicUnsafeThaw v)
  basicLength  (V_Value (_,v)) =
    G.basicLength v
  basicUnsafeSlice m n (V_Value (x,v)) =
    V_Value ( G.basicUnsafeSlice m n x
            , G.basicUnsafeSlice m n v)
  basicUnsafeIndexM (V_Value (x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m/=maskNoData
       then liftM Value (G.basicUnsafeIndexM v i)
       else return NoData
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

runGDAL :: NFData a => (forall s. GDAL s a) -> IO (a, [GDALException])
runGDAL (GDAL a) = withErrorHandler $ do
  ret <- runResourceT (a >>= liftIO . evaluate . force)
  errs <- getErrors
  return (ret, errs)

execGDAL :: NFData a => (forall s. GDAL s a) -> IO a
execGDAL a = liftM fst (runGDAL a)

unsafeGDALToIO :: GDAL s a -> GDAL s (IO a)
unsafeGDALToIO (GDAL act) = do
  state <- GDAL getInternalState
  return (runInternalState act state)
