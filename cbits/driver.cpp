#include "gdal_priv.h"
#include "driver.h"
#include "HsFFI.h"

static void hs_gdal_unload_driver ( GDALDriver *driver )
{
  if ( driver->pfnOpen )
    hs_free_fun_ptr ( reinterpret_cast<HsFunPtr>(driver->pfnOpen) );
  if ( driver->pfnIdentify )
    hs_free_fun_ptr ( reinterpret_cast<HsFunPtr>(driver->pfnIdentify) );
}

GDALDriverH hs_gdal_new_driver (
  const char *driverName,
  HsDriverIdentify identify,
  HsDriverOpen open
)
{
  GDALDriver *poDriver = new GDALDriver();
  poDriver->SetDescription( driverName );
  poDriver->pfnOpen =
    reinterpret_cast<GDALDataset* (*)(GDALOpenInfo*)>( open );
  poDriver->pfnIdentify =
    reinterpret_cast<int (*)(GDALOpenInfo*)>( identify );
  poDriver->pfnUnloadDriver = hs_gdal_unload_driver;

  return static_cast<GDALDriverH>( poDriver );
}

void hs_gdal_delete_driver ( GDALDriverH pDriver )
{
  if ( pDriver ) {
    delete static_cast<GDALDriver*>( pDriver );
  }
}

const char *hs_gdal_openinfo_filename ( GDALOpenInfoH info )
{
  return static_cast<GDALOpenInfo *>(info)->pszFilename;
}
