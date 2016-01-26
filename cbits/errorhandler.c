#include "errorhandler.h"
#include "cpl_conv.h"
#include <assert.h>


static void error_handler(CPLErr, int, const char *);
static void destroy_stack (ErrorStack);

//
// Public interface
//

void push_error_handler(ErrorStack stack)
{
  CPLPushErrorHandlerEx(error_handler, (void*)stack);
}

void pop_error_handler(ErrorStack stack)
{
  CPLPopErrorHandler();
  destroy_stack(stack);
}


//
// Internal implementation
//


static void error_handler(CPLErr errClass, int errNo, const char *msg)
{
  ErrorStack stack = CPLGetErrorHandlerUserData();
  assert(stack);
  ErrorCell cell = CPLMalloc(sizeof(struct error_cell));
  cell->errClass = errClass;
  cell->errNo    = errNo;
  cell->msg      = strdup(msg);
  cell->next     = *stack;
  *stack         = cell;
}

static void destroy_stack (ErrorStack stack)
{
  assert(stack);
  ErrorCell cur = *stack;
  while (cur) {
    ErrorCell next = cur->next;
    if (cur) {
      free(cur->msg);
      free(cur);
    }
    cur = next;
  }
  *stack = NULL;
}
