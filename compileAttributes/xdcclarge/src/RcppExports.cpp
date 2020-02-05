// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// cdcc_compositelik
double cdcc_compositelik(double alpha, double beta, arma::mat ht, arma::mat residuals, arma::mat stdresids, arma::mat uncR, int nobs, int ndim);
RcppExport SEXP _xdcclarge_cdcc_compositelik(SEXP alphaSEXP, SEXP betaSEXP, SEXP htSEXP, SEXP residualsSEXP, SEXP stdresidsSEXP, SEXP uncRSEXP, SEXP nobsSEXP, SEXP ndimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ht(htSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type residuals(residualsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type stdresids(stdresidsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type uncR(uncRSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    rcpp_result_gen = Rcpp::wrap(cdcc_compositelik(alpha, beta, ht, residuals, stdresids, uncR, nobs, ndim));
    return rcpp_result_gen;
END_RCPP
}
// cdcc_construct
arma::mat cdcc_construct(double alpha, double beta, arma::mat stdresids, arma::mat uncR, int nobs, int ndim, int ts);
RcppExport SEXP _xdcclarge_cdcc_construct(SEXP alphaSEXP, SEXP betaSEXP, SEXP stdresidsSEXP, SEXP uncRSEXP, SEXP nobsSEXP, SEXP ndimSEXP, SEXP tsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type stdresids(stdresidsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type uncR(uncRSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type ts(tsSEXP);
    rcpp_result_gen = Rcpp::wrap(cdcc_construct(alpha, beta, stdresids, uncR, nobs, ndim, ts));
    return rcpp_result_gen;
END_RCPP
}
// dcc_compositelik
double dcc_compositelik(double alpha, double beta, arma::mat ht, arma::mat residuals, arma::mat stdresids, arma::mat uncR, int nobs, int ndim);
RcppExport SEXP _xdcclarge_dcc_compositelik(SEXP alphaSEXP, SEXP betaSEXP, SEXP htSEXP, SEXP residualsSEXP, SEXP stdresidsSEXP, SEXP uncRSEXP, SEXP nobsSEXP, SEXP ndimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ht(htSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type residuals(residualsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type stdresids(stdresidsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type uncR(uncRSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    rcpp_result_gen = Rcpp::wrap(dcc_compositelik(alpha, beta, ht, residuals, stdresids, uncR, nobs, ndim));
    return rcpp_result_gen;
END_RCPP
}
// dcc_construct
arma::mat dcc_construct(double alpha, double beta, arma::mat stdresids, arma::mat uncR, int nobs, int ndim, int ts);
RcppExport SEXP _xdcclarge_dcc_construct(SEXP alphaSEXP, SEXP betaSEXP, SEXP stdresidsSEXP, SEXP uncRSEXP, SEXP nobsSEXP, SEXP ndimSEXP, SEXP tsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type stdresids(stdresidsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type uncR(uncRSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type ts(tsSEXP);
    rcpp_result_gen = Rcpp::wrap(dcc_construct(alpha, beta, stdresids, uncR, nobs, ndim, ts));
    return rcpp_result_gen;
END_RCPP
}
