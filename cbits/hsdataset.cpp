#include <stdio.h>
#include <iostream>
#include "gdal_priv.h"
#include "hsdataset.h"
#include "HsFFI.h"


/************************************************************************/
/*                                                                      */
/*                       HSDataset                                      */
/*                                                                      */
/************************************************************************/

class HSRasterBand;

class HSDataset : public GDALDataset
{
friend class HSRasterBand;

public:
  HSDataset();
  HSDataset(const hsDatasetImpl&);
   ~HSDataset();

  static GDALDataset*  Open( GDALOpenInfo * );
  static int           Identify( GDALOpenInfo * );

  CPLErr               GetGeoTransform( double * padfTransform );
  const char*          GetProjectionRef();

private:
  double adfGeoTransform[6];
  char *pszProjection;
  const HsStablePtr state;
  void (*const pfnDestroyState)( HsStablePtr );
};


/************************************************************************/
/*                                                                      */
/*                            HSRasterBand                              */
/*                                                                      */
/************************************************************************/

class HSRasterBand : public GDALRasterBand
{
    friend class HSDataset;

public:
  HSRasterBand( HSDataset *, int, const hsRasterBandImpl& );
  virtual ~HSRasterBand();

  virtual CPLErr IReadBlock( int, int, void * );
  virtual double GetNoDataValue( int *pbSuccess = NULL );

private:
  const HsStablePtr state;
  const double noDataValue;
  const bool hasNodata;
  int (*const pfnReadBlock)(const HsStablePtr, const int, const int, void*);

};

/************************************************************************/
/*                       HSDataset::HSDataset()                         */
/************************************************************************/
HSDataset::HSDataset():
  pszProjection(0),
  state(0),
  pfnDestroyState(0)
{
  this->adfGeoTransform[0] = 0;
  this->adfGeoTransform[1] = 1;
  this->adfGeoTransform[2] = 0;
  this->adfGeoTransform[3] = 0;
  this->adfGeoTransform[4] = 0;
  this->adfGeoTransform[5] = 1;
}

/************************************************************************/
/*                       HSDataset::HSDataset(impl)                     */
/************************************************************************/
HSDataset::HSDataset(const hsDatasetImpl& impl):
  pszProjection(impl.pszProjection),
  state(impl.state),
  pfnDestroyState(impl.destroyState)
{
  for (int i=0; i<6; i++) {
    this->adfGeoTransform[i] = impl.adfGeoTransform[i];
  }
  this->nRasterXSize    = impl.nRasterXSize;
  this->nRasterYSize    = impl.nRasterYSize;
  this->nBands          = impl.nBands;

  for (int i=0; i < this->nBands; i++) {
    HSRasterBand *band = new HSRasterBand ( this, i+1, impl.bands[i] ) ;
    this->SetBand ( i+1, band );
  }
}

/************************************************************************/
/*                       HSDataset::~HSDataset()                         */
/************************************************************************/
HSDataset::~HSDataset()
{
  CPLFree ( this->pszProjection );
  if ( this->pfnDestroyState && this->state ) {
    this->pfnDestroyState ( this->state );
  }
  if ( this->state ) {
    hs_free_stable_ptr ( this->state );
  }
  if ( this->pfnDestroyState ) {
    hs_free_fun_ptr (
      reinterpret_cast<HsFunPtr> ( this->pfnDestroyState ) );
  }
  this->FlushCache();
}



/************************************************************************/
/*                       HSDataset::GetGeoTransform()                   */
/************************************************************************/

CPLErr HSDataset::GetGeoTransform( double * padfTransform ) 
{
    memcpy( padfTransform,  adfGeoTransform, sizeof(double) * 6 );
    return CE_None;
}



/************************************************************************/
/*                      HSDataset::GetProjectionRef()                   */
/************************************************************************/

const char *HSDataset::GetProjectionRef() 
{
    return this->pszProjection;
}



/************************************************************************/
/*                       HSRasterBand::HSRasterBand()                         */
/************************************************************************/
HSRasterBand::HSRasterBand( HSDataset *poDS, int nBand,
                            const hsRasterBandImpl &impl ):
  state ( poDS->state ),
  noDataValue ( impl.nodata ),
  hasNodata ( impl.hasNodata ),
  pfnReadBlock ( impl.readBlock )
{
  this->poDS = poDS;
  this->nBand = nBand;
  this->nBlockXSize  = impl.nBlockXSize;
  this->nBlockYSize  = impl.nBlockYSize;
  this->eDataType    = impl.eDataType;
}

/************************************************************************/
/*                       HSRasterBand::~HSRasterBand()                  */
/************************************************************************/
HSRasterBand::~HSRasterBand()
{
  hs_free_fun_ptr ( reinterpret_cast<HsFunPtr>( this->pfnReadBlock ) );
}

/************************************************************************/
/*                      HSRasterBand::IReadBlock()                      */
/************************************************************************/

CPLErr
HSRasterBand::IReadBlock( int nBlockXOff, int nBlockYOff, void *pImage )
{
  return static_cast<CPLErr>(
    this->pfnReadBlock( this->state, nBlockXOff, nBlockYOff, pImage ) );
}

/************************************************************************/
/*                     HSRasterBand::GetNoDataValue()                   */
/************************************************************************/

double HSRasterBand::GetNoDataValue( int *pbSuccess )
{
    if ( pbSuccess )
      *pbSuccess = this->hasNodata;
    return this->noDataValue;
}



/************************************************************************/
/*                         hs_gdal_create_dataset                       */
/************************************************************************/
GDALDatasetH hs_gdal_create_dataset ( HSDatasetImpl impl )
{
  return static_cast<GDALDatasetH>( new HSDataset( *impl ) );
}

/************************************************************************/
/*                         destroyHSDatasetImpl                         */
/*                                                                      */
/* For use in cleanup after haskell exception.                          */
/************************************************************************/
void hs_gdal_destroy_HSDatasetImpl (HSDatasetImpl impl)
{
  if ( impl->bands ) {
    for (int i=0; i<impl->nBands; i++) {
      if ( impl->bands[i].readBlock ) {
        hs_free_fun_ptr (
          reinterpret_cast<HsFunPtr>( impl->bands[i].readBlock ) );
      }
    }
    free ( impl->bands );
  }
  if ( impl->state ) {
    hs_free_stable_ptr ( impl-> state );
  }
  if ( impl->destroyState ) {
    hs_free_fun_ptr ( reinterpret_cast<HsFunPtr>( impl->destroyState ) );
  }
  CPLFree ( impl->pszProjection );
  free ( impl );
}
