#ifndef HS_HDAL_SUGGESTEDWARPOUTPUT_H
#define HS_HDAL_SUGGESTEDWARPOUTPUT_H

#include "gdal.h"
#include "gdal_alg.h"

#ifdef __cplusplus
extern "C" {
#endif


CPLErr hs_gdal_suggested_warp_output (
    const int nRasterXSize,
    const int nRasterYSize,
    GDALTransformerFunc pfnTransformer,
    void * 	pTransformArg,
    double * 	padfGeoTransformOut,
    int * 	pnPixels,
    int * 	pnLines 
  );

#ifdef __cplusplus
}
#endif

#endif
