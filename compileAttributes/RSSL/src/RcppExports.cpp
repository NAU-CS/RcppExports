// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// harmonic_function_cpp
arma::mat harmonic_function_cpp(const arma::mat& W, const arma::mat& Y);
RcppExport SEXP _RSSL_harmonic_function_cpp(SEXP WSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type W(WSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(harmonic_function_cpp(W, Y));
    return rcpp_result_gen;
END_RCPP
}
// svmlin_rcpp
List svmlin_rcpp(S4 X, NumericVector y, int l, int algorithm, double lambda, double lambda_u, int max_switch, double pos_frac, double Cp, double Cn, NumericVector costs, bool verbose);
RcppExport SEXP _RSSL_svmlin_rcpp(SEXP XSEXP, SEXP ySEXP, SEXP lSEXP, SEXP algorithmSEXP, SEXP lambdaSEXP, SEXP lambda_uSEXP, SEXP max_switchSEXP, SEXP pos_fracSEXP, SEXP CpSEXP, SEXP CnSEXP, SEXP costsSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< S4 >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    Rcpp::traits::input_parameter< int >::type algorithm(algorithmSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< double >::type lambda_u(lambda_uSEXP);
    Rcpp::traits::input_parameter< int >::type max_switch(max_switchSEXP);
    Rcpp::traits::input_parameter< double >::type pos_frac(pos_fracSEXP);
    Rcpp::traits::input_parameter< double >::type Cp(CpSEXP);
    Rcpp::traits::input_parameter< double >::type Cn(CnSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(svmlin_rcpp(X, y, l, algorithm, lambda, lambda_u, max_switch, pos_frac, Cp, Cn, costs, verbose));
    return rcpp_result_gen;
END_RCPP
}
// rowMax
arma::mat rowMax(const arma::mat& X);
RcppExport SEXP _RSSL_rowMax(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMax(X));
    return rcpp_result_gen;
END_RCPP
}
// rowMax2
arma::colvec rowMax2(const arma::mat& X);
RcppExport SEXP _RSSL_rowMax2(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMax2(X));
    return rcpp_result_gen;
END_RCPP
}
// which_rowMax
arma::mat which_rowMax(const arma::mat& X);
RcppExport SEXP _RSSL_which_rowMax(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(which_rowMax(X));
    return rcpp_result_gen;
END_RCPP
}
// which_rowMax2
arma::mat which_rowMax2(const arma::mat& X);
RcppExport SEXP _RSSL_which_rowMax2(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(which_rowMax2(X));
    return rcpp_result_gen;
END_RCPP
}
// sort_matrix
arma::mat sort_matrix(const arma::mat& X);
RcppExport SEXP _RSSL_sort_matrix(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(sort_matrix(X));
    return rcpp_result_gen;
END_RCPP
}
// rowwise_addition
arma::mat rowwise_addition(arma::mat A, arma::rowvec x);
RcppExport SEXP _RSSL_rowwise_addition(SEXP ASEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowwise_addition(A, x));
    return rcpp_result_gen;
END_RCPP
}
// factor_to_dummy_cpp
arma::mat factor_to_dummy_cpp(Rcpp::IntegerVector y, int c);
RcppExport SEXP _RSSL_factor_to_dummy_cpp(SEXP ySEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(factor_to_dummy_cpp(y, c));
    return rcpp_result_gen;
END_RCPP
}