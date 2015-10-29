module OGR (
    DataSource
  , SQLDialect (..)
  , ApproxOK (..)
  , Layer
  , RODataSource
  , RWDataSource
  , ROLayer
  , RWLayer
  , Driver

  , OGR
  , OGRConduit
  , OGRSource
  , OGRSink

  , OGRError (..)
  , OGRException (..)
  , DriverCapability(..)
  , LayerCapability(..)
  , DataSourceCapability(..)

  , OGRFeature (..)
  , OGRFeatureDef (..)
  , OGRField   (..)
  , OGRTimeZone (..)
  , Fid (..)
  , FieldType (..)
  , Field (..)
  , Feature (..)
  , Justification (..)

  , FeatureDef (..)
  , GeomFieldDef (..)
  , FieldDef (..)

  , GeometryType (..)
  , Geometry (..)
  , WkbByteOrder (..)
  , Envelope (..)

  , runOGR

  , createFromWkt
  , createFromWkb

  , exportToWkt
  , exportToWkb
  , exportToGml
  , exportToKml
  , exportToJson

  , geometrySpatialReference
  , geometryType
  , geometryEnvelope

  , transformWith
  , transformTo

  , fieldTypedAs
  , (.:)
  , (.=)
  , aGeom
  , aNullableGeom
  , theGeom
  , theNullableGeom
  , feature

  , isOGRException

  , openReadOnly
  , openReadWrite
  , create
  , createMem
  , canCreateMultipleGeometryFields

  , datasourceName
  , executeSQL
  , executeSQL_

  , createLayer
  , createLayerWithDef

  , getLayer
  , getLayerByName

  , sourceLayer
  , sourceLayer_
  , conduitInsertLayer
  , conduitInsertLayer_
  , sinkInsertLayer
  , sinkInsertLayer_
  , sinkUpdateLayer

  , syncToDisk
  , syncLayerToDisk

  , getSpatialFilter
  , setSpatialFilter

  , layerCount
  , layerExtent
  , layerName
  , layerFeatureDef

  , createFeature
  , createFeatureWithFid
  , createFeature_
  , getFeature
  , updateFeature
  , deleteFeature
) where

import GDAL.Internal.OGRError as X
import GDAL.Internal.OGRGeometry as X
import GDAL.Internal.OGRFeature as X
import GDAL.Internal.OGR as X
import GDAL.Internal.OGRFieldInstances ()
