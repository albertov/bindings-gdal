{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.Types.Value (
    Value(..)
  , isNoData
  , fromValue

  , mkValueUVector
  , mkMaskedValueUVector
  , mkAllValidValueUVector
  , mkValueUMVector
  , mkMaskedValueUMVector
  , mkAllValidValueUMVector
  , toGVec
  , toGVecWithNodata
  , toGVecWithMask
) where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM, liftM2)

import Data.Typeable (Typeable)
import Data.Word (Word8)

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U

import GDAL.Internal.DataType
import qualified GDAL.Internal.Types.Vector as GV


data Value a
  = Value {unValue :: {-# UNPACK #-} !a}
  | NoData
  deriving (Eq, Ord, Show, Read, Typeable)

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

data Mask v a
  = AllValid
  | Mask      (v Word8)
  | UseNoData a

maskValid, maskNoData :: Word8
maskValid  = 255
maskNoData = 0

newtype instance U.Vector    (Value a) =
    V_Value (Mask GV.Vector a, GV.Vector a)
newtype instance U.MVector s (Value a) =
  MV_Value (Mask (GV.MVector s) a, GV.MVector s a)
instance GDALType a => U.Unbox (Value a)


mkMaskedValueUVector
  :: GDALType a
  => GV.Vector Word8 -> GV.Vector a
  -> U.Vector (Value a)
mkMaskedValueUVector mask values =
    V_Value (Mask (mask), values)
{-# INLINE mkMaskedValueUVector #-}

mkAllValidValueUVector
  :: GDALType a
  => GV.Vector a -> U.Vector (Value a)
mkAllValidValueUVector values = V_Value (AllValid, values)
{-# INLINE mkAllValidValueUVector #-}

mkValueUVector
  :: GDALType a
  => a -> GV.Vector a -> U.Vector (Value a)
mkValueUVector nd values = V_Value (UseNoData nd, values)
{-# INLINE mkValueUVector #-}

mkMaskedValueUMVector
  :: GDALType a
  => GV.MVector s Word8 -> GV.MVector s a -> U.MVector s (Value a)
mkMaskedValueUMVector mask values = MV_Value (Mask mask, values)
{-# INLINE mkMaskedValueUMVector #-}

mkAllValidValueUMVector
  :: GDALType a
  => GV.MVector s a -> U.MVector s (Value a)
mkAllValidValueUMVector values = MV_Value (AllValid, values)
{-# INLINE mkAllValidValueUMVector #-}

mkValueUMVector
  :: GDALType a => a -> GV.MVector s a -> U.MVector s (Value a)
mkValueUMVector nd values = MV_Value (UseNoData nd, values)
{-# INLINE mkValueUMVector #-}



toGVecWithNodata
  :: GDALType a
  => a -> U.Vector (Value a) -> GV.Vector a
toGVecWithNodata nd v =
   (G.generate (G.length v) (fromValue nd . G.unsafeIndex v))
{-# INLINE toGVecWithNodata #-}

toGVecWithMask
  :: GDALType a
  => U.Vector (Value a)
  -> (GV.Vector Word8, GV.Vector a)
toGVecWithMask (V_Value (Mask m  , vs)) = ( m, vs)
toGVecWithMask (V_Value (AllValid, vs)) =
  ( (G.replicate (G.length vs) maskValid), vs)
toGVecWithMask (V_Value (UseNoData nd, vs)) =
  (  (G.map (\v->if v==nd then maskNoData else maskValid) vs)
  ,  vs)
{-# INLINE toGVecWithMask #-}


toGVec :: GDALType a => U.Vector (Value a) -> Maybe (GV.Vector a)
toGVec (V_Value (AllValid, v)) = Just ( v)
toGVec (V_Value (UseNoData nd, v))
  | G.any (==nd) v = Nothing
  | otherwise       = Just ( v)
toGVec (V_Value (Mask m, v))
  | G.any (==maskNoData) m = Nothing
  | otherwise               = Just ( v)
{-# INLINE toGVec #-}

instance GDALType a => M.MVector U.MVector (Value a) where
  basicLength (MV_Value (_,v)) = M.basicLength v

  basicUnsafeSlice m n (MV_Value (Mask x,v)) =
    MV_Value ( Mask (M.basicUnsafeSlice m n x)
             , M.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (MV_Value (x,v)) =
    MV_Value (x, M.basicUnsafeSlice m n v)

  basicOverlaps (MV_Value (_,v)) (MV_Value (_,v')) = M.basicOverlaps v v'

  basicUnsafeNew i =
    liftM2 (\x v -> MV_Value (Mask x,v))
           (M.basicUnsafeNew i)
           (M.basicUnsafeNew i)

  basicUnsafeRead (MV_Value (Mask x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m/=maskNoData
       then liftM Value (M.basicUnsafeRead v i)
       else return NoData

  basicUnsafeRead (MV_Value (AllValid,v)) i =
    liftM Value (M.basicUnsafeRead v i)

  basicUnsafeRead (MV_Value (UseNoData nd,v)) i = do
    val <- M.basicUnsafeRead v i
    return (if val==nd then NoData else Value val)

  basicUnsafeWrite (MV_Value (Mask x,v)) i a =
    case a of
      NoData ->
        M.basicUnsafeWrite x i maskNoData
      Value a' -> do
        M.basicUnsafeWrite x i maskValid
        M.basicUnsafeWrite v i a'

  basicUnsafeWrite (MV_Value (AllValid,v)) i a =
    case a of
      --TODO log the error or throw an exception in debug mode
      NoData   -> return ()
      Value a' -> M.basicUnsafeWrite v i a'

  basicUnsafeWrite (MV_Value (UseNoData nd,v)) i a =
    case a of
      NoData   -> M.basicUnsafeWrite v i nd
      Value a' -> M.basicUnsafeWrite v i a'

  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Value (Mask x,v)) = do
    M.basicInitialize x
    M.basicInitialize v
  basicInitialize (MV_Value (_,v)) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance GDALType a => G.Vector U.Vector (Value a) where
  basicUnsafeFreeze (MV_Value (Mask x,v)) =
    liftM2 (\x' v' -> V_Value (Mask x',v'))
           (G.basicUnsafeFreeze x)
           (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MV_Value (AllValid,v)) =
    liftM (\v' -> V_Value (AllValid,v')) (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MV_Value (UseNoData nd,v)) =
    liftM (\v' -> V_Value (UseNoData nd,v')) (G.basicUnsafeFreeze v)

  basicUnsafeThaw (V_Value (Mask x,v)) =
    liftM2 (\x' v' -> MV_Value (Mask x',v'))
           (G.basicUnsafeThaw x)
           (G.basicUnsafeThaw v)
  basicUnsafeThaw (V_Value (AllValid,v)) =
    liftM (\v' -> MV_Value (AllValid,v')) (G.basicUnsafeThaw v)
  basicUnsafeThaw (V_Value (UseNoData nd,v)) =
    liftM (\v' -> MV_Value (UseNoData nd,v')) (G.basicUnsafeThaw v)

  basicLength  (V_Value (_,v)) = G.basicLength v

  basicUnsafeSlice m n (V_Value (Mask x,v)) =
    V_Value (Mask (G.basicUnsafeSlice m n x), G.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (V_Value (x,v)) =
    V_Value (x, G.basicUnsafeSlice m n v)

  basicUnsafeIndexM (V_Value (Mask x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m/=maskNoData
       then liftM Value (G.basicUnsafeIndexM v i)
       else return NoData
  basicUnsafeIndexM (V_Value (AllValid,v)) i =
     liftM Value (G.basicUnsafeIndexM v i)
  basicUnsafeIndexM (V_Value (UseNoData nd,v)) i = do
    val <- G.basicUnsafeIndexM v i
    return (if val==nd then NoData else Value val)

  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
