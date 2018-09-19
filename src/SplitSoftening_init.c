#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void auc_trapezoid(int *ndata, double *predictions, int *labels, double *result);
extern void pred_ss(double *data, int *ndata, int *dim, int *treesize, int *var,
               double *splits, int *ncat, double *lb, double *ub,
               int *childref, double *yval, int *nclass, double *prob);

static const R_CMethodDef CEntries[] = {
    {"auc_trapezoid", (DL_FUNC) &auc_trapezoid,  4},
    {"pred_ss",       (DL_FUNC) &pred_ss,       13},
    {NULL, NULL, 0}
};

void R_init_SplitSoftening(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

