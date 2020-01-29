// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// RV2cpp
double RV2cpp(arma::mat X, arma::mat Y);
RcppExport SEXP MatrixCorrelation_RV2cpp(SEXP XSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(RV2cpp(X, Y));
    return rcpp_result_gen;
END_RCPP
}
// significantRcpp
NumericVector significantRcpp(SEXP smi1, SEXP T1, SEXP U1, SEXP B1);
RcppExport SEXP MatrixCorrelation_significantRcpp(SEXP smi1SEXP, SEXP T1SEXP, SEXP U1SEXP, SEXP B1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type smi1(smi1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type T1(T1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type U1(U1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type B1(B1SEXP);
    rcpp_result_gen = Rcpp::wrap(significantRcpp(smi1, T1, U1, B1));
    return rcpp_result_gen;
END_RCPP
}
// significantRepRcpp
NumericVector significantRepRcpp(SEXP smi1, SEXP T1, SEXP U1, SEXP B1, SEXP perm, SEXP nrep, SEXP nseg, SEXP inperm);
RcppExport SEXP MatrixCorrelation_significantRepRcpp(SEXP smi1SEXP, SEXP T1SEXP, SEXP U1SEXP, SEXP B1SEXP, SEXP permSEXP, SEXP nrepSEXP, SEXP nsegSEXP, SEXP inpermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type smi1(smi1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type T1(T1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type U1(U1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type B1(B1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type perm(permSEXP);
    Rcpp::traits::input_parameter< SEXP >::type nrep(nrepSEXP);
    Rcpp::traits::input_parameter< SEXP >::type nseg(nsegSEXP);
    Rcpp::traits::input_parameter< SEXP >::type inperm(inpermSEXP);
    rcpp_result_gen = Rcpp::wrap(significantRepRcpp(smi1, T1, U1, B1, perm, nrep, nseg, inperm));
    return rcpp_result_gen;
END_RCPP
}
