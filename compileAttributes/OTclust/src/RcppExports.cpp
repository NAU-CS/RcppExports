// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// ACPS
List ACPS(IntegerVector x, int nbs);
RcppExport SEXP _OTclust_ACPS(SEXP xSEXP, SEXP nbsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nbs(nbsSEXP);
    rcpp_result_gen = Rcpp::wrap(ACPS(x, nbs));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_OTclust_ACPS", (DL_FUNC) &_OTclust_ACPS, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_OTclust(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
