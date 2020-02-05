// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// gamfunk
NumericVector gamfunk(NumericVector epsmat, NumericMatrix gammat);
RcppExport SEXP _pcIRT_gamfunk(SEXP epsmatSEXP, SEXP gammatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type epsmat(epsmatSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type gammat(gammatSEXP);
    rcpp_result_gen = Rcpp::wrap(gamfunk(epsmat, gammat));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_pcIRT_gamfunk", (DL_FUNC) &_pcIRT_gamfunk, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_pcIRT(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}