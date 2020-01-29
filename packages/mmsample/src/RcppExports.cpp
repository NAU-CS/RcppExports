// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// ruler
NumericVector ruler(NumericMatrix vR, NumericVector uR, NumericMatrix ciR);
RcppExport SEXP _mmsample_ruler(SEXP vRSEXP, SEXP uRSEXP, SEXP ciRSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type vR(vRSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type uR(uRSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ciR(ciRSEXP);
    rcpp_result_gen = Rcpp::wrap(ruler(vR, uR, ciR));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mmsample_ruler", (DL_FUNC) &_mmsample_ruler, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_mmsample(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}