{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module OSGeo.GDAL.Warper (
    ResampleAlg (..)
  , WarpOptions (..)
  , reprojectImage
  , autoCreateWarpedVRT
  , createWarpedVRT
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throw, bracket)
import Data.Default (Default(..))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Storable (Storable(..))
import OSGeo.OSR (SpatialReference, toWkt)
import OSGeo.GDAL.Internal
import OSGeo.Util (fromEnumC, toEnumC)
import System.IO.Unsafe (unsafePerformIO)

#include "gdal.h"
#include "gdalwarper.h"

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter} deriving (Eq,Read,Show) #}

data WarpOptions
  = WarpOptions {
      woResampleAlg     :: ResampleAlg
    , woWarpOptions     :: OptionList
    , woMemoryLimit     :: Double
    , woWorkingDatatype :: Datatype
    , woBands           :: [(Int,Int)]
  } deriving (Show)

intListToPtr :: [Int] -> IO (Ptr CInt)
intListToPtr [] = return nullPtr
intListToPtr l = do
  ptr <- mallocArray (length l)
  mapM_ (\(i,v) -> pokeElemOff ptr i (fromIntegral v)) (zip [0..] l)
  return ptr

ptrToIntList :: Int -> Ptr CInt -> IO [Int]
ptrToIntList 0 _   = return []
ptrToIntList n ptr = mapM (\i -> fmap fromIntegral (peekElemOff ptr i)) [0..n]

instance Default WarpOptions where
  def = unsafePerformIO $ bracket c_createWarpOptions c_destroyWarpOptions peek

instance Storable WarpOptions where
  alignment _ = {#alignof GDALWarpOptions#}
  sizeOf _    = {#sizeof GDALWarpOptions#}
  peek p = do
    nBands <- fmap fromIntegral ({#get GDALWarpOptions.nBandCount #} p)
    srcBands <- ptrToIntList nBands =<< ({#get GDALWarpOptions.panSrcBands #} p)
    dstBands <- ptrToIntList nBands =<< ({#get GDALWarpOptions.panDstBands #} p)
    WarpOptions
       <$> fmap toEnumC ({#get GDALWarpOptions.eResampleAlg #} p)
       <*> ({#get GDALWarpOptions.papszWarpOptions #} p >>= fromOptionListPtr)
       <*> fmap realToFrac ({#get GDALWarpOptions.dfWarpMemoryLimit #} p)
       <*> fmap toEnumC ({#get GDALWarpOptions.eWorkingDataType #} p)
       <*> pure (zip srcBands dstBands)

  poke p WarpOptions{..} = do
    {#set GDALWarpOptions.eResampleAlg #} p (fromEnumC woResampleAlg)
    oListPtr <- toOptionListPtr woWarpOptions
    {#set GDALWarpOptions.papszWarpOptions #} p oListPtr
    {#set GDALWarpOptions.dfWarpMemoryLimit #} p (realToFrac woMemoryLimit)
    {#set GDALWarpOptions.eWorkingDataType #} p (fromEnumC woWorkingDatatype)
    {#set GDALWarpOptions.nBandCount #} p (fromIntegral (length woBands))
    {#set GDALWarpOptions.panSrcBands #} p =<< intListToPtr (map fst woBands)
    {#set GDALWarpOptions.panDstBands #} p =<< intListToPtr (map snd woBands)

reprojectImage
  :: GDALType a
  => Dataset s t a
  -> Maybe SpatialReference
  -> RWDataset s a
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Int
  -> Maybe WarpOptions
  -> GDAL s ()
reprojectImage srcDs srcSr dstDs dstSr alg maxError options
  | maxError < 0 = error "reprojectImage: maxError < 0"
  | otherwise
  = liftIO $ throwIfError_ "reprojectImage" $
      withLockedDatasetPtr srcDs $ \srcDsPtr ->
      withLockedDatasetPtr dstDs $ \dstDsPtr ->
      withMaybeSRAsCString srcSr $ \sSr ->
      withMaybeSRAsCString dstSr $ \dSr ->
      withWarpOptionsPtr options $ \opts ->
       c_reprojectImage srcDsPtr sSr dstDsPtr dSr
                        (fromEnumC alg) 0 (fromIntegral maxError)
                        nullPtr nullPtr opts

withMaybeSRAsCString :: Maybe SpatialReference -> (CString -> IO a) -> IO a
withMaybeSRAsCString Nothing f = f nullPtr
withMaybeSRAsCString (Just srs) f = withCString (toWkt srs) f

foreign import ccall safe "gdalwarper.h GDALReprojectImage" c_reprojectImage
  :: Ptr (Dataset s t a) -- ^Source dataset
  -> CString             -- ^Source proj (WKT)
  -> Ptr (RWDataset s a) -- ^Dest dataset
  -> CString             -- ^Dest proj (WKT)
  -> CInt                -- ^Resample alg
  -> CDouble             -- ^Memory limit
  -> CDouble             -- ^Max error
  -> Ptr ()              -- ^Progress func (unused)
  -> Ptr ()              -- ^Progress arg (unused)
  -> Ptr WarpOptions     -- ^warp options
  -> IO CInt

autoCreateWarpedVRT
  :: GDALType a
  => RODataset s a
  -> Maybe SpatialReference
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Int
  -> Maybe WarpOptions
  -> GDAL s (RODataset s a)
autoCreateWarpedVRT srcDs srcSr dstSr alg maxError options = do
  newDsPtr <- liftIO $
    withLockedDatasetPtr srcDs $ \srcDsPtr ->
    withMaybeSRAsCString srcSr $ \sSr ->
    withMaybeSRAsCString dstSr $ \dSr ->
    withWarpOptionsPtr options $ \opts ->
      c_autoCreateWarpedVRT srcDsPtr sSr dSr (fromEnumC alg)
                            (fromIntegral maxError) opts
  newDerivedDatasetHandle srcDs newDsPtr



foreign import ccall safe "gdalwarper.h GDALAutoCreateWarpedVRT" c_autoCreateWarpedVRT
  :: Ptr (RODataset s a) -- ^Source dataset
  -> CString             -- ^Source proj (WKT)
  -> CString             -- ^Dest proj (WKT)
  -> CInt                -- ^Resample alg
  -> CDouble             -- ^Max error
  -> Ptr WarpOptions     -- ^warp options
  -> IO (Ptr (Dataset s t a))

createWarpedVRT
  :: GDALType a
  => RODataset s a
  -> Int
  -> Int
  -> Geotransform
  -> WarpOptions
  -> GDAL s (RODataset s a)
createWarpedVRT srcDs nPixels nLines gt options = do
  let dsPtr      = unDataset srcDs
      nBands     = datasetBandCount srcDs
      options'
        | null (woBands options)
        = options { woBands = map (\i -> (i,i)) [1..nBands]}
        | otherwise
        = options
  newDsPtr <- liftIO $
    withWarpOptionsPtr (Just options') $ \opts -> do
      ptr <- alloca $ \gtPtr -> do
        poke (castPtr gtPtr) gt
        {#set GDALWarpOptions.hSrcDS #} opts (castPtr dsPtr)
        c_createWarpedVRT dsPtr (fromIntegral nPixels) (fromIntegral nLines)
                          gtPtr opts
      throwIfError_ "initializeWarpedVRT" (c_initializeWarpedVRT ptr opts)
      return ptr
  newDerivedDatasetHandle srcDs newDsPtr

foreign import ccall safe "gdalwarper.h GDALInitializeWarpedVRT" c_initializeWarpedVRT
  :: Ptr (RODataset s a)
  -> Ptr WarpOptions
  -> IO CInt

foreign import ccall safe "gdalwarper.h GDALCreateWarpedVRT" c_createWarpedVRT
  :: Ptr (RODataset s a) -- ^Source dataset
  -> CInt                -- ^nPixels
  -> CInt                -- ^nLines
  -> Ptr CDouble         -- ^geotransform
  -> Ptr WarpOptions     -- ^warp options
  -> IO (Ptr (Dataset s t a))


withWarpOptionsPtr :: Maybe WarpOptions -> (Ptr WarpOptions -> IO a) -> IO a
withWarpOptionsPtr Nothing  f = f nullPtr
withWarpOptionsPtr (Just wo) f
  = bracket c_createWarpOptions c_destroyWarpOptions (\p -> poke p wo >> f p)

foreign import ccall unsafe "gdalwarper.h GDALCreateWarpOptions" c_createWarpOptions
  :: IO (Ptr WarpOptions)

foreign import ccall unsafe "gdalwarper.h GDALDestroyWarpOptions" c_destroyWarpOptions
  :: Ptr WarpOptions -> IO ()
