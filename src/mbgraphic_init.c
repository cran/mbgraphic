#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP mbgraphic_cmasum(SEXP);
extern SEXP mbgraphic_variableflip(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"mbgraphic_cmasum",       (DL_FUNC) &mbgraphic_cmasum,       1},
    {"mbgraphic_variableflip", (DL_FUNC) &mbgraphic_variableflip, 3},
    {NULL, NULL, 0}
};

void R_init_mbgraphic(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
