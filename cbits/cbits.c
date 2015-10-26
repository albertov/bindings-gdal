#include "cbits.h"

typedef ErrorCell* error_stack;

static error_stack get_stack()
{
  return CPLGetErrorHandlerUserData();
}

static void error_handler(CPLErr errClass, int errNo, const char *msg);
static void destroy_stack (error_stack stack);
static void clear_stack_ (error_stack stack);
static error_stack new_stack();

//
// Public interface
//

void push_error_handler()
{
  CPLPushErrorHandlerEx(error_handler, (void*)new_stack());
}

void pop_error_handler()
{
  destroy_stack(get_stack());
  CPLPopErrorHandler();
}

void clear_stack()
{
  clear_stack_(get_stack());
}

ErrorCell pop_last()
{
  error_stack stack = get_stack();
  ErrorCell ret = *stack;
  if (ret) {
    *stack = ret->next;
  }
  return ret;
}

void destroy_ErrorCell(ErrorCell cell)
{
  if (cell) {
    free(cell->msg);
    free(cell);
  }
}



//
// Internal implementation
//

static void error_handler(CPLErr errClass, int errNo, const char *msg)
{
  error_stack stack = get_stack();
  if (stack) {
    ErrorCell cell = malloc(sizeof(struct error_cell));
    if (cell) {
      cell->errClass = errClass;
      cell->errNo    = errNo;
      cell->msg      = strdup(msg);
      cell->next     = *stack;
      *stack         = cell;
    }
  }
}

static void destroy_stack (error_stack stack)
{
  if (stack) {
    clear_stack_(stack);
    free(stack);
  }
}

static void clear_stack_ (error_stack stack)
{
  ErrorCell cur = *stack;
  while (cur) {
    ErrorCell next = cur->next;
    destroy_ErrorCell(cur);
    cur = next;
  }
  *stack = NULL;
}


static error_stack new_stack()
{
  error_stack stack = malloc(sizeof(error_stack));
  if (stack) {
    *stack = NULL;
  }
  return stack;
}
