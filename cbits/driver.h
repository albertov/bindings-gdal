#ifndef HS_GDAL_DRIVER_H
#define HS_GDAL_DRIVER_H

#include "gdal.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void* GDALOpenInfoH;
typedef int (*HsDriverIdentify)( GDALOpenInfoH );
typedef GDALDatasetH (*HsDriverOpen)( GDALOpenInfoH );

GDALDriverH hs_gdal_new_driver (
  const char *driverName,
  HsDriverIdentify,
  HsDriverOpen
);

void hs_gdal_delete_driver ( GDALDriverH );

const char *hs_gdal_openinfo_filename ( GDALOpenInfoH );

#ifdef __cplusplus
}
#endif

#endif

