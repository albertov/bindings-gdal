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

GDALDatasetH hs_gdal_create_overview_dataset (
    GDALDatasetH hSrcDs,
    int nOvrLevel,
    int bThisLevelOnly
    )
{
//FIXME: Use GdalOpenEx instead of the GDALCreateOverviewDataset private API
  return static_cast<GDALDatasetH>(
#if ((GDAL_VERSION_MAJOR >= 2) && (GDAL_VERSION_MINOR >=2))
    GDALCreateOverviewDataset(static_cast<GDALDataset*>(hSrcDs), nOvrLevel, bThisLevelOnly)
#elif (GDAL_VERSION_MAJOR >= 2)
    GDALCreateOverviewDataset(static_cast<GDALDataset*>(hSrcDs), nOvrLevel, bThisLevelOnly, 0)
#else
    NULL
#endif
    );
}
