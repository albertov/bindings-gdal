{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module GDAL.Internal.HSDataset (
  HSDataset (..)
, HSRasterBand (..)
, ReadBlock
, hsBandBlockLen
, toGDALDataset
, toGDALDatasetIO
) where

#include "hsdataset.h"
#include "gdal.h"
#include "cpl_conv.h"

{#import GDAL.Internal.GDAL#}

import GDAL.Internal.DataType ( hsDataType, toCDouble )
import GDAL.Internal.OSR ( SpatialReference, withMaybeSRAsCString )
import GDAL.Internal.Util ( fromEnumC )
--import GDAL.Internal.Types.Value ( toGVec, toGVecWithNodata )
import GDAL.Internal.Types ( GDAL
                           , GDALInternalState
                           , BlockIx
                           , Size
                           , Pair (..)
                           , runWithInternalState
                           , getInternalState
                           , createGDALInternalState
                           , closeGDALInternalState )
import GDAL.Internal.CPLError ( ErrorType (..) )
import Control.Exception ( SomeException, catch, uninterruptibleMask_ )
import Control.DeepSeq (NFData (..) , force)
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Data.Maybe (fromMaybe)
import Data.Proxy ( Proxy(Proxy) )
import qualified Data.Vector.Storable as St
import Foreign.C
import Foreign.Marshal.Alloc ( callocBytes, alloca )
import Foreign.Marshal.Array ( pokeArray, copyArray )
import Foreign.Ptr (Ptr, FunPtr, nullFunPtr, castPtr)
import Foreign.ForeignPtr
import Foreign.StablePtr ( newStablePtr, castStablePtrToPtr
                         , deRefStablePtr, castPtrToStablePtr )
import Foreign.Storable ( Storable (..) )
import System.IO ( stderr, hPutStrLn )


type InternalStateStablePtr = Ptr ()
type ReadBlock s a = BlockIx -> GDAL s (St.Vector a)
type CReadBlock = InternalStateStablePtr -> CInt -> CInt -> Ptr () -> IO CInt

data HSDataset s = HSDataset
  { rasterSize   :: Size
  , bands        :: [HSRasterBand s]
  , srs          :: Maybe SpatialReference
  , geotransform :: Geotransform
  }

instance NFData (HSDataset s) where
  rnf (HSDataset a b c d) = rnf a `seq` rnf b  `seq` rnf c `seq` rnf d

data HSRasterBand s = forall a. (NFData a, GDALType a) =>
  HSRasterBand
    { blockSize :: !Size
    , nodata    :: !(Maybe a)
    , readBlock :: !(ReadBlock s a)
    }

instance NFData (HSRasterBand s) where
  rnf (HSRasterBand a b c) = rnf a `seq` rnf b  `seq` rnf c

hsBandBlockLen :: HSRasterBand s -> Int
hsBandBlockLen = (\(x :+: y) -> x*y) . blockSize

{# pointer HSDatasetImpl #}
{# pointer HSRasterBandImpl #}


pokeHSDataset :: HSDatasetImpl -> HSDataset s -> IO ()
pokeHSDataset p HSDataset{..} = uninterruptibleMask_ $ do
  {#set HSDatasetImpl->nRasterXSize#}  p xsize
  {#set HSDatasetImpl->nRasterYSize#}  p ysize
  {#set HSDatasetImpl->nBands#}        p (fromIntegral (length bands))
  bandsPtr <- callocBytes (length bands * {#sizeof hsRasterBandImpl#})
  -- set the pointer before poking the array so we don't lose the reference
  -- and we can cleanup in case a poke fails midway
  {#set HSDatasetImpl->bands#} p (castPtr bandsPtr)
  pokeArray bandsPtr bands
  pSrs <- withMaybeSRAsCString srs {#call unsafe CPLStrdup as cStrdup #}
  {#set HSDatasetImpl->pszProjection#} p pSrs
  flip poke geotransform . castPtr =<< {#get HSDatasetImpl->adfGeoTransform#} p
  where
    xsize :+: ysize = fmap fromIntegral rasterSize


foreign import ccall safe "wrapper"
  c_wrapReadBlock :: CReadBlock -> IO (FunPtr CReadBlock)

wrapReadBlock
  :: HSRasterBand s -> IO (FunPtr CReadBlock)
wrapReadBlock b@HSRasterBand{..} = c_wrapReadBlock c_readBlock
  where
    c_readBlock :: CReadBlock
    c_readBlock statePtr i j destPtr = handleAllExceptions onExc $ do
      let ix = fmap fromIntegral (i :+: j)
      state <- deRefState statePtr
      v <- runWithInternalState (readBlock ix) state
      St.unsafeWith v $ \srcPtr ->
        copyArray (castPtr destPtr) srcPtr bLen
      return ok

    onExc e = do
      printErr "Unhandled exception in readBlock" e
      return failure

    deRefState :: Ptr () -> IO (GDALInternalState s)
    deRefState = deRefStablePtr . castPtrToStablePtr

    bLen = hsBandBlockLen b

pokeHSRasterBand :: HSRasterBandImpl -> HSRasterBand s -> IO ()
pokeHSRasterBand p b@HSRasterBand{readBlock=(_ :: ReadBlock s a),..} = do
  {#set HSRasterBandImpl->nBlockXSize#} p xsize
  {#set HSRasterBandImpl->nBlockYSize#} p ysize
  {#set HSRasterBandImpl->eDataType#}   p (fromEnumC dtype)
  {#set HSRasterBandImpl->nodata#}      p (toCDouble (fromMaybe 0 nodata))
  {#set HSRasterBandImpl->hasNodata#}   p (maybe 0 (const 1) nodata)
  fPtr <- wrapReadBlock b
  {#set HSRasterBandImpl->readBlock#} p fPtr
  where
    dtype = hsDataType (Proxy :: Proxy a)
    xsize :+: ysize = fmap fromIntegral blockSize

instance Storable (HSRasterBand s) where
  sizeOf _ = {#sizeof hsRasterBandImpl #}
  alignment _ = {#alignof hsRasterBandImpl #}

  peek =
    error "HSRasterBand's Storable's peek has not been implemented yet"
  poke p = pokeHSRasterBand (castPtr p)

printErr :: String -> SomeException -> IO ()
printErr msg e = hPutStrLn stderr ("ERROR: " ++ msg ++ ": " ++ show e)

ok :: CInt
ok = fromEnumC CE_None

failure :: CInt
failure = fromEnumC CE_Failure

handleAllExceptions
  :: NFData a => (SomeException -> IO a) -> IO a -> IO a
handleAllExceptions onError action = catch (fmap force action) onError

toGDALDatasetIO :: GDAL s (HSDataset s) -> IO DatasetH
toGDALDatasetIO mkDataset =
  alloca $ \impl ->
  handleAllExceptions (onExc impl) $ do
    state <- createGDALInternalState
    ds <- runWithInternalState mkDataset state
    pokeHSDataset impl ds
    statePtr <- castStablePtrToPtr <$> newStablePtr state
    {#set HSDatasetImpl->state #} impl statePtr
    destroyState <- c_wrapDestroyState closeInternalState
    {#set HSDatasetImpl->destroyState#} impl destroyState
    {#call unsafe hs_gdal_create_dataset#} impl
  where
    onExc impl e = do
      printErr "Unhandled exception in toGDALDatasetIO" e
      {#call unsafe destroyHSDatasetImpl#} impl
      return nullDatasetH

toGDALDataset :: HSDataset s -> GDAL s DatasetH
toGDALDataset ds = do
  state <- getInternalState
  liftIO $ alloca $ \impl -> handleAllExceptions (onExc impl) $ do
    pokeHSDataset impl ds
    statePtr <- castStablePtrToPtr <$> newStablePtr state
    {#set HSDatasetImpl->state #} impl statePtr
    {#set HSDatasetImpl->destroyState#} impl nullFunPtr
    {#call unsafe hs_gdal_create_dataset#} impl
  where
    onExc impl e = do
      printErr "Unhandled exception in toGDALDatasetIO" e
      {#call unsafe destroyHSDatasetImpl#} impl
      return nullDatasetH


foreign import ccall safe "wrapper"
  c_wrapDestroyState :: (Ptr () -> IO ()) -> IO (FinalizerPtr ())


closeInternalState :: InternalStateStablePtr -> IO ()
closeInternalState
  = handleAllExceptions onExc
  . (closeGDALInternalState <=< deRefStablePtr . castPtrToStablePtr)
  where
    onExc e =
      printErr "Unhandled exception in closeGDALInternalState" e
