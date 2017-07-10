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
  return GDALBandGetBestOverviewLevel2(
      static_cast<GDALRasterBand*>(band)
    , nXOff, nYOff
    , nXSize, nYSize
    , nBufXSize, nBufYSize
    , NULL
    );
}

GDALDatasetH hs_gdal_create_overview_dataset (
    GDALDatasetH hSrcDs,
    int nOvrLevel,
    int bThisLevelOnly
    )
{
  return static_cast<GDALDatasetH>(
    GDALCreateOverviewDataset(static_cast<GDALDataset*>(hSrcDs), nOvrLevel, bThisLevelOnly)
    );
}
