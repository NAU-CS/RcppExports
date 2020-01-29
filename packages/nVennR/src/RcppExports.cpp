// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// drawVenn
StringVector drawVenn(StringVector x);
RcppExport SEXP _nVennR_drawVenn(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(drawVenn(x));
    return rcpp_result_gen;
END_RCPP
}
// makeVenn
List makeVenn(List x, int nCycl);
RcppExport SEXP _nVennR_makeVenn(SEXP xSEXP, SEXP nCyclSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nCycl(nCyclSEXP);
    rcpp_result_gen = Rcpp::wrap(makeVenn(x, nCycl));
    return rcpp_result_gen;
END_RCPP
}
// refineVenn
StringVector refineVenn(List x);
RcppExport SEXP _nVennR_refineVenn(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(refineVenn(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nVennR_drawVenn", (DL_FUNC) &_nVennR_drawVenn, 1},
    {"_nVennR_makeVenn", (DL_FUNC) &_nVennR_makeVenn, 2},
    {"_nVennR_refineVenn", (DL_FUNC) &_nVennR_refineVenn, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_nVennR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}