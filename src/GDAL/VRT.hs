module GDAL.VRT (
  VRTDataset
, VRTSourcedRasterBand
, ScaleOffset(..)
, ScaleRatio(..)
, ImageReadFunc
, createVRT
, createVRTBand
, addSimpleSource
, addComplexSource
, addFuncSource
, vrtBandAsBand
, getSrcDstWin
) where

import GDAL.Internal.VRT
import GDAL

-- | Calculates the source an destination envelopes for
--   'addSimpleSource' or 'addComplexSource'
-- Port of GetSrcDstWin from gdalbuildvrt_lib
getSrcDstWin
  :: Georeference
  -- ^ Destination Georeference
  -> Georeference
  -- ^ Source Georeference
  -> Either String (Maybe (Envelope Int, Envelope Int))
  -- ^ (Destination Envelope, Source Envelope)
getSrcDstWin dstGr srcGr
  | gtXRot srcGt /= 0 || gtYRot srcGt /=0 || gtXRot dstGt /= 0 || gtYRot dstGt /= 0
  = Left "Only Geotransforms without rotation are supported"
  | gtYDelta srcGt >= 0 || gtYDelta dstGt >=0
  = Left "Only north-up Geotransforms are supported"
  | srcX1 < dstX0 || srcX0 > dstX1 || srcY1 < dstY0 || srcY0 > dstY1 = Right Nothing
  | otherwise
  = Right $ Just
    ( Envelope (truncate <$> (dstXOff :+: dstYOff))
               (min <$> grSize dstGr
                    <*> (truncate <$> ((dstXOff + dstNx) :+: (dstYOff + dstNy))))
    , Envelope (truncate <$> (srcXOff :+: srcYOff))
               (grSize srcGr)
    )
  where
  srcNx :+: srcNy = fromIntegral <$> grSize srcGr
  srcGt = grTransform srcGr

  srcX0 = gtXOff srcGt
  srcX1 = srcX0 + (gtXDelta srcGt * srcNx)
  srcY0 = srcY1 + (gtYDelta srcGt * srcNy)
  srcY1 = gtYOff srcGt

  dstNx = srcNx * (gtXDelta srcGt / gtXDelta dstGt)
  dstNy = srcNy * (gtYDelta srcGt / gtYDelta dstGt)
  dstGt = grTransform dstGr

  dstX0 = gtXOff dstGt
  dstX1 = dstX0 + (gtXDelta dstGt * dstNx)
  dstY0 = dstY1 + (gtYDelta dstGt * dstNy)
  dstY1 = gtYOff dstGt

  srcXOff | srcX0 < dstX0 = (dstX0 - srcX0) / gtXDelta srcGt
          | otherwise     = 0

  dstXOff | srcX0 < dstX0 = 0
          | otherwise     = (srcX0 - dstX0) / gtXDelta dstGt

  srcYOff | dstY1 < srcY1 = (srcY1 - dstY1) / negate (gtYDelta srcGt)
          | otherwise     = 0

  dstYOff | dstY1 < srcY1 = 0
          | otherwise     = (dstY1 - srcY1) / negate (gtYDelta dstGt)
