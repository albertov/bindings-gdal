#include "contourwriter.h"
#include <assert.h>


CPLErr
hs_contour_writer(double level, int nPoints, double *xs, double *ys, 
                  ContourList list)
{
  assert(list);

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

  int i;
  for (i=0; i<nPoints; i++) {
    contour->points[i].px = xs[i];
    contour->points[i].py = ys[i];
  }
  contour->nPoints = nPoints;
  contour->level = level;
  contour->next = *list,
  *list = contour;
  return CE_None;
}

Point *pop_contour(ContourList list, double *level, int *nPoints)
{
  assert (list);
  Contour *contour = *list;
  Point *ret = NULL;
  if (contour) {
    *list    = contour->next;
    *level   = contour->level;
    *nPoints = contour->nPoints;
    ret      = contour->points;
    free(contour);
  }
  return ret;
}

void destroy_points (Point *points)
{
  if (points) {
    free(points);
  }
}

void destroy_contours (ContourList list)
{
  if (list) {
    Contour *cur = *list;
    while (cur) {
      destroy_points(cur->points);
      Contour *next = cur->next;
      free(cur);
      cur = next;
    }
  }
}
