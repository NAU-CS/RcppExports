// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// lagged_variance_c
double lagged_variance_c(NumericVector X, int k, int n);
RcppExport SEXP _partialCI_lagged_variance_c(SEXP XSEXP, SEXP kSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(lagged_variance_c(X, k, n));
    return rcpp_result_gen;
END_RCPP
}
// estimate_rho_par_c
double estimate_rho_par_c(NumericVector X);
RcppExport SEXP _partialCI_estimate_rho_par_c(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(estimate_rho_par_c(X));
    return rcpp_result_gen;
END_RCPP
}
// estimate_par_c
NumericVector estimate_par_c(NumericVector X, double rho_max);
RcppExport SEXP _partialCI_estimate_par_c(SEXP XSEXP, SEXP rho_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type rho_max(rho_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(estimate_par_c(X, rho_max));
    return rcpp_result_gen;
END_RCPP
}
// pvmr_par_c
double pvmr_par_c(double rho, double sigma_M, double sigma_R);
RcppExport SEXP _partialCI_pvmr_par_c(SEXP rhoSEXP, SEXP sigma_MSEXP, SEXP sigma_RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_M(sigma_MSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_R(sigma_RSEXP);
    rcpp_result_gen = Rcpp::wrap(pvmr_par_c(rho, sigma_M, sigma_R));
    return rcpp_result_gen;
END_RCPP
}
// kalman_gain_par_mr
double kalman_gain_par_mr(double rho, double sigma_M, double sigma_R);
RcppExport SEXP _partialCI_kalman_gain_par_mr(SEXP rhoSEXP, SEXP sigma_MSEXP, SEXP sigma_RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_M(sigma_MSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_R(sigma_RSEXP);
    rcpp_result_gen = Rcpp::wrap(kalman_gain_par_mr(rho, sigma_M, sigma_R));
    return rcpp_result_gen;
END_RCPP
}
// loglik_par_c
double loglik_par_c(NumericVector Y, double rho, double sigma_M, double sigma_R, double M0, double R0);
RcppExport SEXP _partialCI_loglik_par_c(SEXP YSEXP, SEXP rhoSEXP, SEXP sigma_MSEXP, SEXP sigma_RSEXP, SEXP M0SEXP, SEXP R0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Y(YSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_M(sigma_MSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_R(sigma_RSEXP);
    Rcpp::traits::input_parameter< double >::type M0(M0SEXP);
    Rcpp::traits::input_parameter< double >::type R0(R0SEXP);
    rcpp_result_gen = Rcpp::wrap(loglik_par_c(Y, rho, sigma_M, sigma_R, M0, R0));
    return rcpp_result_gen;
END_RCPP
}
// loglik_par_t_c
double loglik_par_t_c(NumericVector Y, double rho, double sigma_M, double sigma_R, double M0, double R0, double nu);
RcppExport SEXP _partialCI_loglik_par_t_c(SEXP YSEXP, SEXP rhoSEXP, SEXP sigma_MSEXP, SEXP sigma_RSEXP, SEXP M0SEXP, SEXP R0SEXP, SEXP nuSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Y(YSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_M(sigma_MSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_R(sigma_RSEXP);
    Rcpp::traits::input_parameter< double >::type M0(M0SEXP);
    Rcpp::traits::input_parameter< double >::type R0(R0SEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    rcpp_result_gen = Rcpp::wrap(loglik_par_t_c(Y, rho, sigma_M, sigma_R, M0, R0, nu));
    return rcpp_result_gen;
END_RCPP
}
