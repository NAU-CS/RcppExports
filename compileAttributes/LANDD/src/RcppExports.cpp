// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// stl_sort
NumericVector stl_sort(NumericVector x);
RcppExport SEXP _LANDD_stl_sort(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(stl_sort(x));
    return rcpp_result_gen;
END_RCPP
}
// normalizeInput
void normalizeInput(NumericVector& x);
RcppExport SEXP _LANDD_normalizeInput(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type x(xSEXP);
    normalizeInput(x);
    return R_NilValue;
END_RCPP
}
// normalizeInputMatrix
NumericMatrix normalizeInputMatrix(NumericMatrix& x);
RcppExport SEXP _LANDD_normalizeInputMatrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(normalizeInputMatrix(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_LANDD_stl_sort", (DL_FUNC) &_LANDD_stl_sort, 1},
    {"_LANDD_normalizeInput", (DL_FUNC) &_LANDD_normalizeInput, 1},
    {"_LANDD_normalizeInputMatrix", (DL_FUNC) &_LANDD_normalizeInputMatrix, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_LANDD(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
