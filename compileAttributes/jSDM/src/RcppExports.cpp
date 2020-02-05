// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// Rcpp_jSDM_binomial
Rcpp::List Rcpp_jSDM_binomial(const int ngibbs, int nthin, int nburn, arma::uvec Y, arma::uvec T, arma::mat X, arma::vec beta_start, arma::vec mubeta, arma::vec Vbeta, const int seed, const double ropt, const int verbose);
RcppExport SEXP _jSDM_Rcpp_jSDM_binomial(SEXP ngibbsSEXP, SEXP nthinSEXP, SEXP nburnSEXP, SEXP YSEXP, SEXP TSEXP, SEXP XSEXP, SEXP beta_startSEXP, SEXP mubetaSEXP, SEXP VbetaSEXP, SEXP seedSEXP, SEXP roptSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type ngibbs(ngibbsSEXP);
    Rcpp::traits::input_parameter< int >::type nthin(nthinSEXP);
    Rcpp::traits::input_parameter< int >::type nburn(nburnSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type T(TSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta_start(beta_startSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mubeta(mubetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Vbeta(VbetaSEXP);
    Rcpp::traits::input_parameter< const int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< const double >::type ropt(roptSEXP);
    Rcpp::traits::input_parameter< const int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_jSDM_binomial(ngibbs, nthin, nburn, Y, T, X, beta_start, mubeta, Vbeta, seed, ropt, verbose));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_jSDM_probit_block
Rcpp::List Rcpp_jSDM_probit_block(const int ngibbs, int nthin, int nburn, arma::umat Y, arma::umat T, arma::mat X, arma::mat param_start, arma::mat Vparam, arma::vec muparam, arma::mat VW, arma::mat W_start, arma::vec alpha_start, double Valpha_start, double shape, double rate, const int seed, const int verbose);
RcppExport SEXP _jSDM_Rcpp_jSDM_probit_block(SEXP ngibbsSEXP, SEXP nthinSEXP, SEXP nburnSEXP, SEXP YSEXP, SEXP TSEXP, SEXP XSEXP, SEXP param_startSEXP, SEXP VparamSEXP, SEXP muparamSEXP, SEXP VWSEXP, SEXP W_startSEXP, SEXP alpha_startSEXP, SEXP Valpha_startSEXP, SEXP shapeSEXP, SEXP rateSEXP, SEXP seedSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type ngibbs(ngibbsSEXP);
    Rcpp::traits::input_parameter< int >::type nthin(nthinSEXP);
    Rcpp::traits::input_parameter< int >::type nburn(nburnSEXP);
    Rcpp::traits::input_parameter< arma::umat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::umat >::type T(TSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type param_start(param_startSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Vparam(VparamSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type muparam(muparamSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type VW(VWSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W_start(W_startSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type alpha_start(alpha_startSEXP);
    Rcpp::traits::input_parameter< double >::type Valpha_start(Valpha_startSEXP);
    Rcpp::traits::input_parameter< double >::type shape(shapeSEXP);
    Rcpp::traits::input_parameter< double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< const int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_jSDM_probit_block(ngibbs, nthin, nburn, Y, T, X, param_start, Vparam, muparam, VW, W_start, alpha_start, Valpha_start, shape, rate, seed, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_jSDM_Rcpp_jSDM_binomial", (DL_FUNC) &_jSDM_Rcpp_jSDM_binomial, 12},
    {"_jSDM_Rcpp_jSDM_probit_block", (DL_FUNC) &_jSDM_Rcpp_jSDM_probit_block, 17},
    {NULL, NULL, 0}
};

RcppExport void R_init_jSDM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
