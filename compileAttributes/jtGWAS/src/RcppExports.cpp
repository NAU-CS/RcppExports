// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// jtGWAS
Rcpp::List jtGWAS(Rcpp::NumericMatrix X, Rcpp::NumericMatrix Y, bool outTopNFlag, int outItemNo, bool standardized);
RcppExport SEXP _jtGWAS_jtGWAS(SEXP XSEXP, SEXP YSEXP, SEXP outTopNFlagSEXP, SEXP outItemNoSEXP, SEXP standardizedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Y(YSEXP);
    Rcpp::traits::input_parameter< bool >::type outTopNFlag(outTopNFlagSEXP);
    Rcpp::traits::input_parameter< int >::type outItemNo(outItemNoSEXP);
    Rcpp::traits::input_parameter< bool >::type standardized(standardizedSEXP);
    rcpp_result_gen = Rcpp::wrap(jtGWAS(X, Y, outTopNFlag, outItemNo, standardized));
    return rcpp_result_gen;
END_RCPP
}
// jtGWASmp
Rcpp::List jtGWASmp(Rcpp::NumericMatrix X, Rcpp::NumericMatrix Y, bool outTopNFlag, int numThreads, int outItemNo, bool standardized);
RcppExport SEXP _jtGWAS_jtGWASmp(SEXP XSEXP, SEXP YSEXP, SEXP outTopNFlagSEXP, SEXP numThreadsSEXP, SEXP outItemNoSEXP, SEXP standardizedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Y(YSEXP);
    Rcpp::traits::input_parameter< bool >::type outTopNFlag(outTopNFlagSEXP);
    Rcpp::traits::input_parameter< int >::type numThreads(numThreadsSEXP);
    Rcpp::traits::input_parameter< int >::type outItemNo(outItemNoSEXP);
    Rcpp::traits::input_parameter< bool >::type standardized(standardizedSEXP);
    rcpp_result_gen = Rcpp::wrap(jtGWASmp(X, Y, outTopNFlag, numThreads, outItemNo, standardized));
    return rcpp_result_gen;
END_RCPP
}
