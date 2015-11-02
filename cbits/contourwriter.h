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


typedef Contour** ContourList;

CPLErr hs_contour_writer(double, int, double*, double*, ContourList);
Point *pop_contour(ContourList, double*, int*);
void destroy_points (Point *);
void destroy_contours (ContourList);

#endif
