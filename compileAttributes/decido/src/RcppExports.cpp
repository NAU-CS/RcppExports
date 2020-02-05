// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// earcut_cpp
IntegerVector earcut_cpp(NumericVector x, NumericVector y, IntegerVector holes, IntegerVector numholes);
RcppExport SEXP _decido_earcut_cpp(SEXP xSEXP, SEXP ySEXP, SEXP holesSEXP, SEXP numholesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type holes(holesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type numholes(numholesSEXP);
    rcpp_result_gen = Rcpp::wrap(earcut_cpp(x, y, holes, numholes));
    return rcpp_result_gen;
END_RCPP
}