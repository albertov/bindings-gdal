module OSR (
    SpatialReference
  , CoordinateTransformation
  , Projectable (..)
  , AxisMappingStrategy (..)

  , srsFromWkt
  , srsFromProj4
  , srsFromEPSG
  , srsFromEPSGIO
  , srsFromXML

  , srsToWkt
  , srsToProj4
  , srsToXML

  , isGeographic
  , isLocal
  , isProjected
  , isSameGeogCS
  , isSame

  , getAngularUnits
  , getLinearUnits

  , coordinateTransformation
  , setAxisMappingStrategy
) where
import GDAL.Internal.OSR
