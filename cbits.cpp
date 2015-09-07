#include "cbits.h"

GDALRasterBlockH
GDALRasterBandGetLockedBlock (GDALRasterBandH hBand, int iXBlock, int iYBlock)
{
   GDALRasterBand *poBand = static_cast<GDALRasterBand*>(hBand);
   GDALRasterBlock *poBlock = poBand->GetLockedBlockRef(iXBlock, iYBlock);
   return static_cast<GDALRasterBlockH> (poBlock);
};

void
GDALRasterBlockDropLock(GDALRasterBlockH hBlock, void *dummy)
{
   GDALRasterBlock *poBlock = static_cast<GDALRasterBlock*>(hBlock);
   poBlock->DropLock();
};

void *
GDALRasterBlockDataRef(GDALRasterBlockH hBlock)
{
   GDALRasterBlock *poBlock = static_cast<GDALRasterBlock*>(hBlock);
   return poBlock->GetDataRef();
};
