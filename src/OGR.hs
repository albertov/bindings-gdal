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
  , EnvelopeReal

  , runOGR

  , envelopeSize

  , geomFromWkt
  , geomFromWkb
  , geomFromGml

  , geomToWkt
  , geomToWkb
  , geomToGml
  , geomToKml
  , geomToJson

  , geomSpatialReference
  , geomType
  , geomEnvelope

  , geomIntersects
  , geomEquals
  , geomDisjoint
  , geomTouches
  , geomCrosses
  , geomWithin
  , geomContains
  , geomOverlaps
  , geomSimplify
  , geomSimplifyPreserveTopology
  , geomSegmentize
  , geomBoundary
  , geomConvexHull
  , geomBuffer
  , geomIntersection
  , geomUnion
  , geomUnionCascaded
  , geomPointOnSurface
  , geomDifference
  , geomSymDifference
  , geomDistance
  , geomLength
  , geomArea
  , geomCentroid
  , geomIsEmpty
  , geomIsValid
  , geomIsSimple
  , geomIsRing
  , geomPolygonize

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
  , closeLayer
  , create
  , closeDataSource
  , createMem
  , canCreateMultipleGeometryFields

  , dataSourceName
  , layerCount
  , executeSQL

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

  , layerExtent
  , layerName
  , layerFeatureDef
  , layerFeatureCount
  , layerSpatialFilter
  , layerSpatialReference
  , setLayerSpatialFilter
  , setLayerSpatialFilterRect
  , clearLayerSpatialFilter


  , createFeature
  , createFeatureWithFid
  , createFeature_
  , getFeature
  , updateFeature
  , deleteFeature

  , unsafeToReadOnlyLayer
) where

import GDAL.Internal.Common
import GDAL.Internal.OGRError
import GDAL.Internal.OGRGeometry
import GDAL.Internal.OGRFeature
import GDAL.Internal.OGR
import GDAL.Internal.OGRFieldInstances ()
