module GDAL.OSR (
    SpatialReference
  , CoordinateTransformation

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
  , isSameGeogCS
  , isSame

  , getAngularUnits
  , getLinearUnits

  , CoordinateTransformation
) where
import GDAL.Internal.OSR
