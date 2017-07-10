#ifndef BINDINGS_H
#define BINDINGS_H

#include "MachDeps.h"

#define SUPPORTS_MULTI_GEOM_FIELDS (((GDAL_VERSION_MAJOR >= 1) && (GDAL_VERSION_MINOR >= 11)) || (GDAL_VERSION_MAJOR >= 2))

#define SUPPORTS_64_BIT_INT_FIELDS (GDAL_VERSION_MAJOR >=2)
#define SUPPORTS_NULLABLE_FIELD_DEFS (GDAL_VERSION_MAJOR >=2)
#define HAVE_ISFIELDSETANDNOTNULL ((GDAL_VERSION_MAJOR >=2) && (GDAL_VERSION_MINOR >= 2))

#define SUPPORTS_WORD_FIELDS ((WORD_SIZE_IN_BITS < 64) || SUPPORTS_64_BIT_INT_FIELDS)

#define SUPPORTS_METADATA_DOMAINS (((GDAL_VERSION_MAJOR >= 1) && (GDAL_VERSION_MINOR >= 11)) || (GDAL_VERSION_MAJOR >= 2))

#define GDT_UNKNOWN   0
#define GDT_BYTE      1
#define GDT_UINT16    2
#define GDT_INT16     3
#define GDT_UINT32    4
#define GDT_INT32     5
#define GDT_FLOAT32   6
#define GDT_FLOAT64   7
#define GDT_CINT16    8
#define GDT_CINT32    9
#define GDT_CFLOAT32  10
#define GDT_CFLOAT64  11

#endif
