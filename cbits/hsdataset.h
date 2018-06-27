#include "gdal.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void* HsStablePtr;

typedef struct hsRasterBandImpl {
  int nBlockXSize;
  int nBlockYSize;
  GDALDataType eDataType;
  double nodata;
  int hasNodata;
  GDALColorInterp colorInterp;
  int (*readBlock)( HsStablePtr, int, int, void* );
}* HSRasterBandImpl;


typedef struct hsDatasetImpl {
  int nRasterXSize;
  int nRasterYSize;
  int nBands;
  HSRasterBandImpl bands;
  char *pszProjection;
  double adfGeoTransform[6];
  HsStablePtr state;
  void (*destroyState)( HsStablePtr );
}* HSDatasetImpl;

GDALDatasetH hs_gdal_create_dataset ( HSDatasetImpl );

void hs_gdal_destroy_HSDatasetImpl (HSDatasetImpl impl);

#ifdef __cplusplus
}
#endif

