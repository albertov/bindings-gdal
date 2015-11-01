#ifndef CONTOURWRITER_H
#define CONTOURWRITER_H

#include "gdal_alg.h"

typedef struct point {
  double px;
  double py;
} Point;


typedef struct contour {
  double level;
  int nPoints;
  Point *points;
  struct contour *next;
} Contour;

typedef Contour **contour_list;

CPLErr hs_contour_writer(double, int, double*, double*, void *);
Contour *pop_contour(contour_list);
void destroy_contour (Contour *);
void destroy_points (Point *);

#endif
