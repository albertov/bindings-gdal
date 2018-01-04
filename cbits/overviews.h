#ifndef HS_GDAL_OVERVIEWS_H
#define HS_GDAL_OVERVIEWS_H

#include "gdal.h"

#ifdef __cplusplus
extern "C" {
#endif


int hs_gdal_band_get_best_overview_level (
    GDALRasterBandH band,
    int nXOff,
    int nYOff,
    int nXSize,
    int nYSize,
    int nBufXSize,
    int nBufYSize
    );

#ifdef __cplusplus
}
#endif

#endif
