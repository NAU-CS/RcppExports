// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// maxcpp
List maxcpp(NumericVector tau, int x, int y, int j);
RcppExport SEXP _timma_maxcpp(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(maxcpp(tau, x, y, j));
    return rcpp_result_gen;
END_RCPP
}
// mincpp
List mincpp(NumericVector tau, int x, int y, int j);
RcppExport SEXP _timma_mincpp(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(mincpp(tau, x, y, j));
    return rcpp_result_gen;
END_RCPP
}
// sumcpp
arma::mat sumcpp(NumericVector tau, int x, int y, int j);
RcppExport SEXP _timma_sumcpp(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(sumcpp(tau, x, y, j));
    return rcpp_result_gen;
END_RCPP
}
// maxcpp1
List maxcpp1(NumericVector tau, int x, int y);
RcppExport SEXP _timma_maxcpp1(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(maxcpp1(tau, x, y));
    return rcpp_result_gen;
END_RCPP
}
// mincpp1
List mincpp1(NumericVector tau, int x, int y);
RcppExport SEXP _timma_mincpp1(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mincpp1(tau, x, y));
    return rcpp_result_gen;
END_RCPP
}
// sumcpp1
arma::rowvec sumcpp1(NumericVector tau, int x, int y);
RcppExport SEXP _timma_sumcpp1(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(sumcpp1(tau, x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_timma_maxcpp", (DL_FUNC) &_timma_maxcpp, 4},
    {"_timma_mincpp", (DL_FUNC) &_timma_mincpp, 4},
    {"_timma_sumcpp", (DL_FUNC) &_timma_sumcpp, 4},
    {"_timma_maxcpp1", (DL_FUNC) &_timma_maxcpp1, 3},
    {"_timma_mincpp1", (DL_FUNC) &_timma_mincpp1, 3},
    {"_timma_sumcpp1", (DL_FUNC) &_timma_sumcpp1, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_timma(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
