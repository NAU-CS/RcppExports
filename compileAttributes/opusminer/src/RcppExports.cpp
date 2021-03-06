// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// opus_cpp
Rcpp::GenericVector opus_cpp(Rcpp::GenericVector tidList, int numItems, int numTrans, Rcpp::NumericVector k_, Rcpp::LogicalVector args);
RcppExport SEXP _opusminer_opus_cpp(SEXP tidListSEXP, SEXP numItemsSEXP, SEXP numTransSEXP, SEXP k_SEXP, SEXP argsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::GenericVector >::type tidList(tidListSEXP);
    Rcpp::traits::input_parameter< int >::type numItems(numItemsSEXP);
    Rcpp::traits::input_parameter< int >::type numTrans(numTransSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type k_(k_SEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type args(argsSEXP);
    rcpp_result_gen = Rcpp::wrap(opus_cpp(tidList, numItems, numTrans, k_, args));
    return rcpp_result_gen;
END_RCPP
}
