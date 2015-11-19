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
  , Masked (..)
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
import Data.Complex
import Data.Int
import Data.Word

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U


data Value a
  = Value {unValue :: !a}
  | NoData
  deriving (Eq, Ord, Show, Typeable)

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
  Value _ * NoData  = NoData
  NoData  * Value _ = NoData
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
  Value _ / NoData  = NoData
  NoData  / Value _ = NoData
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

class ( G.Vector (BaseVector a) a
      , M.MVector (BaseMVector a) a
      , G.Vector (BaseVector a) Word8
      , M.MVector (BaseMVector a) Word8
      , G.Mutable (BaseVector a) ~ BaseMVector a
      , Eq a
      ) => Masked a where
  type BaseMVector a :: * -> * -> *
  type BaseVector  a :: * -> *


newtype instance U.Vector    (Value a) =
    V_Value (Mask (BaseVector a) a, (BaseVector a) a)
newtype instance U.MVector s (Value a) =
  MV_Value (Mask ((BaseMVector a) s) a, (BaseMVector a) s a)
instance Masked a => U.Unbox (Value a)


mkMaskedValueUVector
  :: Masked a
  => BaseVector a Word8 -> BaseVector a a
  -> U.Vector (Value a)
mkMaskedValueUVector mask values =
    V_Value (Mask (mask), values)

mkAllValidValueUVector
  :: Masked a
  => BaseVector a a -> U.Vector (Value a)
mkAllValidValueUVector values = V_Value (AllValid, values)

mkValueUVector
  :: Masked a
  => a -> BaseVector a a -> U.Vector (Value a)
mkValueUVector nd values = V_Value (UseNoData nd, values)

mkMaskedValueUMVector
  :: Masked a
  => BaseMVector a s Word8 -> BaseMVector a s a -> U.MVector s (Value a)
mkMaskedValueUMVector mask values = MV_Value (Mask mask, values)

mkAllValidValueUMVector
  :: Masked a
  => BaseMVector a s a -> U.MVector s (Value a)
mkAllValidValueUMVector values = MV_Value (AllValid, values)

mkValueUMVector
  :: Masked a => a -> BaseMVector a s a -> U.MVector s (Value a)
mkValueUMVector nd values = MV_Value (UseNoData nd, values)



toGVecWithNodata
  :: Masked a
  => a -> U.Vector (Value a) -> BaseVector a a
toGVecWithNodata nd v =
   (G.generate (G.length v) (fromValue nd . G.unsafeIndex v))

toGVecWithMask
  :: Masked a
  => U.Vector (Value a)
  -> (BaseVector a Word8, BaseVector a a)
toGVecWithMask (V_Value (Mask m  , vs)) = ( m, vs)
toGVecWithMask (V_Value (AllValid, vs)) =
  ( (G.replicate (G.length vs) maskValid), vs)
toGVecWithMask (V_Value (UseNoData nd, vs)) =
  (  (G.map (\v->if v==nd then maskNoData else maskValid) vs)
  ,  vs)


toGVec :: Masked a => U.Vector (Value a) -> Maybe ((BaseVector a) a)
toGVec (V_Value (AllValid, v)) = Just ( v)
toGVec (V_Value (UseNoData nd, v))
  | G.any (==nd) v = Nothing
  | otherwise       = Just ( v)
toGVec (V_Value (Mask m, v))
  | G.any (==maskNoData) m = Nothing
  | otherwise               = Just ( v)

instance Masked a => M.MVector U.MVector (Value a) where

  basicLength (MV_Value (_,v)) = M.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MV_Value (Mask x,v)) =
    MV_Value ( Mask (M.basicUnsafeSlice m n x)
             , M.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (MV_Value (x,v)) =
    MV_Value (x, M.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MV_Value (_,v)) (MV_Value (_,v')) = M.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i =
    liftM2 (\x v -> MV_Value (Mask x,v))
           (M.basicUnsafeNew i)
           (M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}


  basicUnsafeRead (MV_Value (Mask x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m/=maskNoData
       then liftM Value (M.basicUnsafeRead v i)
       else return NoData
  basicUnsafeRead (MV_Value (AllValid,v)) i =
    liftM Value (M.basicUnsafeRead v i)
  basicUnsafeRead (MV_Value (UseNoData nd,v)) i = do
    val <- M.basicUnsafeRead v i
    return $! if val==nd then NoData else Value val
  {-# INLINE basicUnsafeRead #-}

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
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Value (Mask x,v)) = do
    M.basicSet x maskNoData
  basicInitialize (MV_Value (AllValid,v)) = M.basicInitialize v
  basicInitialize (MV_Value (UseNoData nd ,v)) = M.basicSet v nd
  {-# INLINE basicInitialize #-}
#endif

instance Masked a => G.Vector U.Vector (Value a) where

  basicUnsafeFreeze (MV_Value (Mask x,v)) =
    liftM2 (\x' v' -> V_Value (Mask x',v'))
           (G.basicUnsafeFreeze x)
           (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MV_Value (AllValid,v)) =
    liftM (\v' -> V_Value (AllValid,v')) (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MV_Value (UseNoData nd,v)) =
    liftM (\v' -> V_Value (UseNoData nd,v')) (G.basicUnsafeFreeze v)
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (V_Value (Mask x,v)) =
    liftM2 (\x' v' -> MV_Value (Mask x',v'))
           (G.basicUnsafeThaw x)
           (G.basicUnsafeThaw v)
  basicUnsafeThaw (V_Value (AllValid,v)) =
    liftM (\v' -> MV_Value (AllValid,v')) (G.basicUnsafeThaw v)
  basicUnsafeThaw (V_Value (UseNoData nd,v)) =
    liftM (\v' -> MV_Value (UseNoData nd,v')) (G.basicUnsafeThaw v)
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (V_Value (_,v)) = G.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (V_Value (Mask x,v)) =
    V_Value (Mask (G.basicUnsafeSlice m n x), G.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (V_Value (x,v)) =
    V_Value (x, G.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (V_Value (Mask x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m/=maskNoData
       then liftM Value (G.basicUnsafeIndexM v i)
       else return NoData
  basicUnsafeIndexM (V_Value (AllValid,v)) i =
     liftM Value (G.basicUnsafeIndexM v i)
  basicUnsafeIndexM (V_Value (UseNoData nd,v)) i = do
    val <- G.basicUnsafeIndexM v i
    return $! (if val==nd then NoData else Value val)
  {-# INLINE basicUnsafeIndexM #-}
