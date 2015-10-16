module GDAL.OSR (
    SpatialReference

  , fromWkt
  , fromProj4
  , fromEPSG
  , fromXML

  , toWkt
  , toProj4
  , toXML

  , isGeographic
  , isLocal
  , isProjected
  , isSameGeoCS
  , isSame

  , getAngularUnits
  , getLinearUnits
) where
import GDAL.Internal.OSR
