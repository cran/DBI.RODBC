#include "R.h"
#include "Rinternals.h"

/* return process id */
SEXP 
RS_getpid(void)
{
   SEXP   pid;

   PROTECT(pid = allocVector(INTSXP, 1));
   INTEGER(pid)[0] = (int) getpid();
   UNPROTECT(1);
   return pid;
}
