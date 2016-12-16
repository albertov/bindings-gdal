#include "suggestedwarpoutput.h"
#include "hsdataset.h"

CPLErr hs_gdal_suggested_warp_output (
    const int nRasterXSize,
    const int nRasterYSize,
    GDALTransformerFunc pfnTransformer,
    void * 	pTransformArg,
    double * 	padfGeoTransformOut,
    int * 	pnPixels,
    int * 	pnLines 
  )
{
  // create a dummy HSDataset to pass to GDALSuggestedWarpOutput the x and y
  // size parameters. This is the only thing it needs it for.
  HSDatasetImpl impl = calloc(1, sizeof(struct hsDatasetImpl));
  impl->nRasterXSize = nRasterXSize;
  impl->nRasterYSize = nRasterYSize;
  GDALDatasetH hSrcDs = hs_gdal_create_dataset(impl);
  CPLErr ret = GDALSuggestedWarpOutput (
    hSrcDs, pfnTransformer, pTransformArg, padfGeoTransformOut, pnPixels, pnLines);
  GDALClose(hSrcDs);
  return ret;
}
