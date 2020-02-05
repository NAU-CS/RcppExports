// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// standard
List standard(const arma::mat& x);
RcppExport SEXP _rbridge_standard(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(standard(x));
    return rcpp_result_gen;
END_RCPP
}
// BetaIntial
arma::mat BetaIntial(arma::mat x, arma::colvec y, arma::colvec lambda);
RcppExport SEXP _rbridge_BetaIntial(SEXP xSEXP, SEXP ySEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(BetaIntial(x, y, lambda));
    return rcpp_result_gen;
END_RCPP
}
// Lambdas_Grid
arma::vec Lambdas_Grid(const arma::mat& x, const arma::vec& y, const double& q, const double& lambda_min, const arma::uword& n_lambda);
RcppExport SEXP _rbridge_Lambdas_Grid(SEXP xSEXP, SEXP ySEXP, SEXP qSEXP, SEXP lambda_minSEXP, SEXP n_lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double& >::type q(qSEXP);
    Rcpp::traits::input_parameter< const double& >::type lambda_min(lambda_minSEXP);
    Rcpp::traits::input_parameter< const arma::uword& >::type n_lambda(n_lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(Lambdas_Grid(x, y, q, lambda_min, n_lambda));
    return rcpp_result_gen;
END_RCPP
}
// Bridge
arma::sp_mat Bridge(const arma::mat& x, const arma::colvec& y, const double& q, const arma::colvec& lambda, const double& converge, const double& eta);
RcppExport SEXP _rbridge_Bridge(SEXP xSEXP, SEXP ySEXP, SEXP qSEXP, SEXP lambdaSEXP, SEXP convergeSEXP, SEXP etaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double& >::type q(qSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const double& >::type converge(convergeSEXP);
    Rcpp::traits::input_parameter< const double& >::type eta(etaSEXP);
    rcpp_result_gen = Rcpp::wrap(Bridge(x, y, q, lambda, converge, eta));
    return rcpp_result_gen;
END_RCPP
}
// RBridge
arma::mat RBridge(const arma::mat& x, const arma::colvec& y, const double& q, const arma::colvec& lambda, const arma::mat& R, const arma::mat& r, const double& converge, const double& eta);
RcppExport SEXP _rbridge_RBridge(SEXP xSEXP, SEXP ySEXP, SEXP qSEXP, SEXP lambdaSEXP, SEXP RSEXP, SEXP rSEXP, SEXP convergeSEXP, SEXP etaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double& >::type q(qSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type R(RSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type r(rSEXP);
    Rcpp::traits::input_parameter< const double& >::type converge(convergeSEXP);
    Rcpp::traits::input_parameter< const double& >::type eta(etaSEXP);
    rcpp_result_gen = Rcpp::wrap(RBridge(x, y, q, lambda, R, r, converge, eta));
    return rcpp_result_gen;
END_RCPP
}
// CV_Bridge
arma::mat CV_Bridge(const arma::mat& x, const arma::colvec& y, const double& q, arma::vec& lambda, const double& converge, const double& eta, const arma::uword& num_folds, const arma::uword& num_threads);
RcppExport SEXP _rbridge_CV_Bridge(SEXP xSEXP, SEXP ySEXP, SEXP qSEXP, SEXP lambdaSEXP, SEXP convergeSEXP, SEXP etaSEXP, SEXP num_foldsSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double& >::type q(qSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const double& >::type converge(convergeSEXP);
    Rcpp::traits::input_parameter< const double& >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< const arma::uword& >::type num_folds(num_foldsSEXP);
    Rcpp::traits::input_parameter< const arma::uword& >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(CV_Bridge(x, y, q, lambda, converge, eta, num_folds, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// CV_RBridge
arma::mat CV_RBridge(const arma::mat& x, const arma::colvec& y, const double& q, arma::vec& lambda, const arma::mat& R, const arma::mat& r, const double& converge, const double& eta, const arma::uword& num_folds, const arma::uword& num_threads);
RcppExport SEXP _rbridge_CV_RBridge(SEXP xSEXP, SEXP ySEXP, SEXP qSEXP, SEXP lambdaSEXP, SEXP RSEXP, SEXP rSEXP, SEXP convergeSEXP, SEXP etaSEXP, SEXP num_foldsSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double& >::type q(qSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type R(RSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type r(rSEXP);
    Rcpp::traits::input_parameter< const double& >::type converge(convergeSEXP);
    Rcpp::traits::input_parameter< const double& >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< const arma::uword& >::type num_folds(num_foldsSEXP);
    Rcpp::traits::input_parameter< const arma::uword& >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(CV_RBridge(x, y, q, lambda, R, r, converge, eta, num_folds, num_threads));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rbridge_standard", (DL_FUNC) &_rbridge_standard, 1},
    {"_rbridge_BetaIntial", (DL_FUNC) &_rbridge_BetaIntial, 3},
    {"_rbridge_Lambdas_Grid", (DL_FUNC) &_rbridge_Lambdas_Grid, 5},
    {"_rbridge_Bridge", (DL_FUNC) &_rbridge_Bridge, 6},
    {"_rbridge_RBridge", (DL_FUNC) &_rbridge_RBridge, 8},
    {"_rbridge_CV_Bridge", (DL_FUNC) &_rbridge_CV_Bridge, 8},
    {"_rbridge_CV_RBridge", (DL_FUNC) &_rbridge_CV_RBridge, 10},
    {NULL, NULL, 0}
};

RcppExport void R_init_rbridge(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}