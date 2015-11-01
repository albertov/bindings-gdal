#ifndef ERROR_HANDLER_H
#define ERROR_HANDLER_H

#include "cpl_error.h"

typedef struct error_cell {
  CPLErr             errClass;
  int                errNo;
  char*              msg;
  struct error_cell* next;
}* ErrorCell;


void push_error_handler();
void pop_error_handler();
void clear_stack();
ErrorCell pop_last();
void destroy_ErrorCell(ErrorCell);

#endif
