// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rnc_solver_X
extern "C" SEXP rnc_solver_X(SEXP X, SEXP L, SEXP Y, SEXP lambda, SEXP W);
RcppExport SEXP netcoh_rnc_solver_X(SEXP XSEXP, SEXP LSEXP, SEXP YSEXP, SEXP lambdaSEXP, SEXP WSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type W(WSEXP);
    __result = Rcpp::wrap(rnc_solver_X(X, L, Y, lambda, W));
    return __result;
END_RCPP
}
// rnc_solver_noX
extern "C" SEXP rnc_solver_noX(SEXP L, SEXP Y, SEXP lambda, SEXP W);
RcppExport SEXP netcoh_rnc_solver_noX(SEXP LSEXP, SEXP YSEXP, SEXP lambdaSEXP, SEXP WSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type W(WSEXP);
    __result = Rcpp::wrap(rnc_solver_noX(L, Y, lambda, W));
    return __result;
END_RCPP
}
// rnc_solver_naive
extern "C" SEXP rnc_solver_naive(SEXP X, SEXP L, SEXP Y, SEXP lambda, SEXP W);
RcppExport SEXP netcoh_rnc_solver_naive(SEXP XSEXP, SEXP LSEXP, SEXP YSEXP, SEXP lambdaSEXP, SEXP WSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type W(WSEXP);
    __result = Rcpp::wrap(rnc_solver_naive(X, L, Y, lambda, W));
    return __result;
END_RCPP
}
// rnc_logistic_fit
extern "C" SEXP rnc_logistic_fit(SEXP X, SEXP L, SEXP Y, SEXP lambda, SEXP theta_init, SEXP tol, SEXP max_iter, SEXP verbose);
RcppExport SEXP netcoh_rnc_logistic_fit(SEXP XSEXP, SEXP LSEXP, SEXP YSEXP, SEXP lambdaSEXP, SEXP theta_initSEXP, SEXP tolSEXP, SEXP max_iterSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type theta_init(theta_initSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< SEXP >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< SEXP >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(rnc_logistic_fit(X, L, Y, lambda, theta_init, tol, max_iter, verbose));
    return __result;
END_RCPP
}
// rnc_logistic_fit_noX
extern "C" SEXP rnc_logistic_fit_noX(SEXP L, SEXP Y, SEXP lambda, SEXP theta_init, SEXP tol, SEXP max_iter, SEXP verbose);
RcppExport SEXP netcoh_rnc_logistic_fit_noX(SEXP LSEXP, SEXP YSEXP, SEXP lambdaSEXP, SEXP theta_initSEXP, SEXP tolSEXP, SEXP max_iterSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type theta_init(theta_initSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< SEXP >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< SEXP >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(rnc_logistic_fit_noX(L, Y, lambda, theta_init, tol, max_iter, verbose));
    return __result;
END_RCPP
}
// rnc_cox_fit
extern "C" SEXP rnc_cox_fit(SEXP X, SEXP L, SEXP Y, SEXP delta, SEXP lambda, SEXP theta_init, SEXP tol, SEXP max_iter, SEXP verbose);
RcppExport SEXP netcoh_rnc_cox_fit(SEXP XSEXP, SEXP LSEXP, SEXP YSEXP, SEXP deltaSEXP, SEXP lambdaSEXP, SEXP theta_initSEXP, SEXP tolSEXP, SEXP max_iterSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type theta_init(theta_initSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< SEXP >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< SEXP >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(rnc_cox_fit(X, L, Y, delta, lambda, theta_init, tol, max_iter, verbose));
    return __result;
END_RCPP
}
// rnc_cox_fit_noX
extern "C" SEXP rnc_cox_fit_noX(SEXP L, SEXP Y, SEXP delta, SEXP lambda, SEXP theta_init, SEXP tol, SEXP max_iter, SEXP verbose);
RcppExport SEXP netcoh_rnc_cox_fit_noX(SEXP LSEXP, SEXP YSEXP, SEXP deltaSEXP, SEXP lambdaSEXP, SEXP theta_initSEXP, SEXP tolSEXP, SEXP max_iterSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type L(LSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type theta_init(theta_initSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< SEXP >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< SEXP >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(rnc_cox_fit_noX(L, Y, delta, lambda, theta_init, tol, max_iter, verbose));
    return __result;
END_RCPP
}
// cox_pll
extern "C" SEXP cox_pll(SEXP eta, SEXP Y, SEXP delta);
RcppExport SEXP netcoh_cox_pll(SEXP etaSEXP, SEXP YSEXP, SEXP deltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type delta(deltaSEXP);
    __result = Rcpp::wrap(cox_pll(eta, Y, delta));
    return __result;
END_RCPP
}