// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// maxcpp
List maxcpp(NumericVector tau, int x, int y, int j);
RcppExport SEXP timma_maxcpp(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP, SEXP jSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP );
        Rcpp::traits::input_parameter< int >::type x(xSEXP );
        Rcpp::traits::input_parameter< int >::type y(ySEXP );
        Rcpp::traits::input_parameter< int >::type j(jSEXP );
        List __result = maxcpp(tau, x, y, j);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// mincpp
List mincpp(NumericVector tau, int x, int y, int j);
RcppExport SEXP timma_mincpp(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP, SEXP jSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP );
        Rcpp::traits::input_parameter< int >::type x(xSEXP );
        Rcpp::traits::input_parameter< int >::type y(ySEXP );
        Rcpp::traits::input_parameter< int >::type j(jSEXP );
        List __result = mincpp(tau, x, y, j);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// sumcpp
arma::mat sumcpp(NumericVector tau, int x, int y, int j);
RcppExport SEXP timma_sumcpp(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP, SEXP jSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP );
        Rcpp::traits::input_parameter< int >::type x(xSEXP );
        Rcpp::traits::input_parameter< int >::type y(ySEXP );
        Rcpp::traits::input_parameter< int >::type j(jSEXP );
        arma::mat __result = sumcpp(tau, x, y, j);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}

// maxcpp1
List maxcpp1(NumericVector tau, int x, int y);
RcppExport SEXP timma_maxcpp1(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP );
        Rcpp::traits::input_parameter< int >::type x(xSEXP );
        Rcpp::traits::input_parameter< int >::type y(ySEXP );
        List __result = maxcpp1(tau, x, y);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// mincpp1
List mincpp1(NumericVector tau, int x, int y);
RcppExport SEXP timma_mincpp1(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP );
        Rcpp::traits::input_parameter< int >::type x(xSEXP );
        Rcpp::traits::input_parameter< int >::type y(ySEXP );
        List __result = mincpp1(tau, x, y);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// sumcpp1
arma::rowvec sumcpp1(NumericVector tau, int x, int y);
RcppExport SEXP timma_sumcpp1(SEXP tauSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP );
        Rcpp::traits::input_parameter< int >::type x(xSEXP );
        Rcpp::traits::input_parameter< int >::type y(ySEXP );
        arma::rowvec __result = sumcpp1(tau, x, y);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}