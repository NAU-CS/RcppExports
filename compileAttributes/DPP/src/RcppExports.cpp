// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;


RcppExport SEXP _rcpp_module_boot_DPPmcmc();
RcppExport SEXP _rcpp_module_boot_Distributions();
RcppExport SEXP _rcpp_module_boot_Models();

static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_module_boot_DPPmcmc", (DL_FUNC) &_rcpp_module_boot_DPPmcmc, 0},
    {"_rcpp_module_boot_Distributions", (DL_FUNC) &_rcpp_module_boot_Distributions, 0},
    {"_rcpp_module_boot_Models", (DL_FUNC) &_rcpp_module_boot_Models, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_DPP(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
