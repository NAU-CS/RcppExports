// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// strat_cpp
List strat_cpp(arma::vec y, arma::vec r, arma::vec w);
RcppExport SEXP strat_strat_cpp(SEXP ySEXP, SEXP rSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(strat_cpp(y, r, w));
    return rcpp_result_gen;
END_RCPP
}
// strat_cpp_by
List strat_cpp_by(arma::vec y, arma::vec r, arma::vec w, arma::vec c);
RcppExport SEXP strat_strat_cpp_by(SEXP ySEXP, SEXP rSEXP, SEXP wSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type w(wSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(strat_cpp_by(y, r, w, c));
    return rcpp_result_gen;
END_RCPP
}