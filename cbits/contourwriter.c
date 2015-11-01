#include "contourwriter.h"
#include <assert.h>

CPLErr
hs_contour_writer(double level, int nPoints, double *xs, double *ys, void *data)
{
  assert(data);

  Contour *contour = malloc(sizeof(Contour));
  if (!contour) {
    CPLError( CE_Fatal
            , CPLE_OutOfMemory
            , "hs_contour_writer: could not allocate Contour");
    return CE_Fatal;
  }

  contour->points = malloc(sizeof(Point)*nPoints);
  if (!contour->points) {
    free(contour);
    CPLError( CE_Fatal
            , CPLE_OutOfMemory
            , "hs_contour_writer: could not allocate Contour point array");
    return CE_Fatal;
  }

  for (int i=0; i<nPoints; i++) {
    contour->points[i].px = xs[i];
    contour->points[i].py = ys[i];
  }
  contour->nPoints = nPoints;
  contour->level = level;
  contour->next = *((contour_list) data);
  *((contour_list) data) = contour;
  return CE_None;
}


Contour *pop_contour(contour_list list)
{
  assert (list);
  Contour *ret = *list;
  if (ret) {
    *list = ret->next;
  }
  return ret;
}

void destroy_contour (Contour *contour)
{
  if (contour) {
    free(contour);
  }
}

void destroy_points (Point *points)
{
  if (points) {
    free(points);
  }
}
