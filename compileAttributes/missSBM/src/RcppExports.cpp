// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// roundProduct
Rcpp::NumericMatrix roundProduct(arma::cube phi, arma::vec beta);
RcppExport SEXP _missSBM_roundProduct(SEXP phiSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(roundProduct(phi, beta));
    return rcpp_result_gen;
END_RCPP
}
// vExpec_covariates
double vExpec_covariates(Rcpp::NumericMatrix Y, Rcpp::NumericMatrix roundProd, Rcpp::NumericMatrix gamma, Rcpp::NumericMatrix Tau, Rcpp::NumericVector alpha);
RcppExport SEXP _missSBM_vExpec_covariates(SEXP YSEXP, SEXP roundProdSEXP, SEXP gammaSEXP, SEXP TauSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Y(YSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type roundProd(roundProdSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Tau(TauSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(vExpec_covariates(Y, roundProd, gamma, Tau, alpha));
    return rcpp_result_gen;
END_RCPP
}
// E_step_covariates
Rcpp::NumericMatrix E_step_covariates(Rcpp::NumericMatrix Y, Rcpp::NumericMatrix roundProd, Rcpp::NumericMatrix gamma, Rcpp::NumericMatrix Tau, Rcpp::NumericVector alpha);
RcppExport SEXP _missSBM_E_step_covariates(SEXP YSEXP, SEXP roundProdSEXP, SEXP gammaSEXP, SEXP TauSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Y(YSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type roundProd(roundProdSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Tau(TauSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(E_step_covariates(Y, roundProd, gamma, Tau, alpha));
    return rcpp_result_gen;
END_RCPP
}
// Mstep_covariates_undirected
List Mstep_covariates_undirected(arma::vec param, NumericMatrix Y, arma::cube cov, NumericMatrix Tau);
RcppExport SEXP _missSBM_Mstep_covariates_undirected(SEXP paramSEXP, SEXP YSEXP, SEXP covSEXP, SEXP TauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type param(paramSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::cube >::type cov(covSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Tau(TauSEXP);
    rcpp_result_gen = Rcpp::wrap(Mstep_covariates_undirected(param, Y, cov, Tau));
    return rcpp_result_gen;
END_RCPP
}
// Mstep_covariates_directed
List Mstep_covariates_directed(arma::vec param, NumericMatrix Y, arma::cube cov, NumericMatrix Tau);
RcppExport SEXP _missSBM_Mstep_covariates_directed(SEXP paramSEXP, SEXP YSEXP, SEXP covSEXP, SEXP TauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type param(paramSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::cube >::type cov(covSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Tau(TauSEXP);
    rcpp_result_gen = Rcpp::wrap(Mstep_covariates_directed(param, Y, cov, Tau));
    return rcpp_result_gen;
END_RCPP
}
// E_step_nocovariate
Rcpp::NumericMatrix E_step_nocovariate(const arma::mat& Y, const arma::mat& Y_bar, const arma::mat& pi, const arma::mat& Tau, const arma::vec& alpha, const double& log_lambda, int fixPointIter);
RcppExport SEXP _missSBM_E_step_nocovariate(SEXP YSEXP, SEXP Y_barSEXP, SEXP piSEXP, SEXP TauSEXP, SEXP alphaSEXP, SEXP log_lambdaSEXP, SEXP fixPointIterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y_bar(Y_barSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type pi(piSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Tau(TauSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double& >::type log_lambda(log_lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type fixPointIter(fixPointIterSEXP);
    rcpp_result_gen = Rcpp::wrap(E_step_nocovariate(Y, Y_bar, pi, Tau, alpha, log_lambda, fixPointIter));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_missSBM_roundProduct", (DL_FUNC) &_missSBM_roundProduct, 2},
    {"_missSBM_vExpec_covariates", (DL_FUNC) &_missSBM_vExpec_covariates, 5},
    {"_missSBM_E_step_covariates", (DL_FUNC) &_missSBM_E_step_covariates, 5},
    {"_missSBM_Mstep_covariates_undirected", (DL_FUNC) &_missSBM_Mstep_covariates_undirected, 4},
    {"_missSBM_Mstep_covariates_directed", (DL_FUNC) &_missSBM_Mstep_covariates_directed, 4},
    {"_missSBM_E_step_nocovariate", (DL_FUNC) &_missSBM_E_step_nocovariate, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_missSBM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}