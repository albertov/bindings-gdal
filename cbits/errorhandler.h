#ifndef ERROR_HANDLER_H
#define ERROR_HANDLER_H

#include "cpl_error.h"

typedef struct error_cell {
  CPLErr             errClass;
  int                errNo;
  char*              msg;
  struct error_cell* next;
}* ErrorCell;

typedef ErrorCell* ErrorStack;

void push_error_handler(ErrorStack);
void pop_error_handler(ErrorStack);
ErrorCell pop_last(ErrorStack);
void destroy_ErrorCell(ErrorCell);

#endif
