#include "overviews.h"
#include "gdal_priv.h"

int hs_gdal_band_get_best_overview_level (
    GDALRasterBandH band,
    int nXOff,
    int nYOff,
    int nXSize,
    int nYSize,
    int nBufXSize,
    int nBufYSize)
{
#if (GDAL_VERSION_MAJOR >= 2)
  return GDALBandGetBestOverviewLevel2(
      static_cast<GDALRasterBand*>(band)
    , nXOff, nYOff
    , nXSize, nYSize
    , nBufXSize, nBufYSize
    , NULL
    );
#else
  return -1;
#endif
}
