#include "gdal.h"
#include "gdal_priv.h"

extern "C" {
  typedef void *GDALRasterBlockH;
  GDALRasterBlockH GDALRasterBandGetLockedBlock(GDALRasterBandH, int, int);
  void GDALRasterBlockDropLock(GDALRasterBlockH, void*);
  void *GDALRasterBlockDataRef(GDALRasterBlockH);
}
