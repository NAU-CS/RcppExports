// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// ComputeConfIntervals
List ComputeConfIntervals(DataFrame data, NumericVector x, double alpha, NumericVector bw);
RcppExport SEXP _curstatCI_ComputeConfIntervals(SEXP dataSEXP, SEXP xSEXP, SEXP alphaSEXP, SEXP bwSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bw(bwSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeConfIntervals(data, x, alpha, bw));
    return rcpp_result_gen;
END_RCPP
}
// ComputeBW
NumericVector ComputeBW(DataFrame data, NumericVector x);
RcppExport SEXP _curstatCI_ComputeBW(SEXP dataSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeBW(data, x));
    return rcpp_result_gen;
END_RCPP
}
// ComputeMLE
DataFrame ComputeMLE(DataFrame data);
RcppExport SEXP _curstatCI_ComputeMLE(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeMLE(data));
    return rcpp_result_gen;
END_RCPP
}
// ComputeSMLE
NumericVector ComputeSMLE(DataFrame data, NumericVector x, NumericVector bw);
RcppExport SEXP _curstatCI_ComputeSMLE(SEXP dataSEXP, SEXP xSEXP, SEXP bwSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bw(bwSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeSMLE(data, x, bw));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_curstatCI_ComputeConfIntervals", (DL_FUNC) &_curstatCI_ComputeConfIntervals, 4},
    {"_curstatCI_ComputeBW", (DL_FUNC) &_curstatCI_ComputeBW, 2},
    {"_curstatCI_ComputeMLE", (DL_FUNC) &_curstatCI_ComputeMLE, 1},
    {"_curstatCI_ComputeSMLE", (DL_FUNC) &_curstatCI_ComputeSMLE, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_curstatCI(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
