// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// get_chronotime
double get_chronotime();
RcppExport SEXP _anMC_get_chronotime() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_chronotime());
    return rcpp_result_gen;
END_RCPP
}
// mvrnormArma
arma::mat mvrnormArma(int n, arma::vec mu, arma::mat sigma, int chol);
RcppExport SEXP _anMC_mvrnormArma(SEXP nSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP cholSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< int >::type chol(cholSEXP);
    rcpp_result_gen = Rcpp::wrap(mvrnormArma(n, mu, sigma, chol));
    return rcpp_result_gen;
END_RCPP
}
// trmvrnorm_rej_cpp
arma::mat trmvrnorm_rej_cpp(int n, arma::vec mu, arma::mat sigma, arma::vec lower, arma::vec upper, int verb);
RcppExport SEXP _anMC_trmvrnorm_rej_cpp(SEXP nSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP verbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type upper(upperSEXP);
    Rcpp::traits::input_parameter< int >::type verb(verbSEXP);
    rcpp_result_gen = Rcpp::wrap(trmvrnorm_rej_cpp(n, mu, sigma, lower, upper, verb));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_anMC_get_chronotime", (DL_FUNC) &_anMC_get_chronotime, 0},
    {"_anMC_mvrnormArma", (DL_FUNC) &_anMC_mvrnormArma, 4},
    {"_anMC_trmvrnorm_rej_cpp", (DL_FUNC) &_anMC_trmvrnorm_rej_cpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_anMC(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}