module OGR (
    DataSource
  , SQLDialect (..)
  , ApproxOK (..)
  , Layer
  , FeatureSource
  , FeatureConduit
  , FeatureSink
  , RODataSource
  , RWDataSource
  , ROLayer
  , RWLayer
  , Driver

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

  , createFromWkt
  , createFromWkb
  , exportToWkt
  , exportToWkb
  , geometrySpatialReference

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

  , createLayer
  , createLayerWithDef

  , getLayer
  , getLayerByName

  , sourceFeatures
  , sourceFeaturesByName

  , conduitCreateFeatures
  , conduitCreateFeaturesByName

  , sinkUpdateFeatures
  , sinkUpdateFeaturesByName

  , syncToDisk

  , getSpatialFilter
  , setSpatialFilter

  , layerCount
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
