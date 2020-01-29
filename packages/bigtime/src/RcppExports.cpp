// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// norm2
double norm2(NumericVector x);
RcppExport SEXP _bigtime_norm2(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(norm2(x));
    return rcpp_result_gen;
END_RCPP
}
// prox2HVAR
arma::rowvec prox2HVAR(arma::colvec v, double lambda, int k, int p);
RcppExport SEXP _bigtime_prox2HVAR(SEXP vSEXP, SEXP lambdaSEXP, SEXP kSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec >::type v(vSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(prox2HVAR(v, lambda, k, p));
    return rcpp_result_gen;
END_RCPP
}
// FistaElem
arma::mat FistaElem(const arma::mat& Y, const arma::mat& Z, arma::mat phi, const int p, const int k, double lambda, const double eps, const double tk);
RcppExport SEXP _bigtime_FistaElem(SEXP YSEXP, SEXP ZSEXP, SEXP phiSEXP, SEXP pSEXP, SEXP kSEXP, SEXP lambdaSEXP, SEXP epsSEXP, SEXP tkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< const int >::type p(pSEXP);
    Rcpp::traits::input_parameter< const int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< const double >::type tk(tkSEXP);
    rcpp_result_gen = Rcpp::wrap(FistaElem(Y, Z, phi, p, k, lambda, eps, tk));
    return rcpp_result_gen;
END_RCPP
}
// gamloopElem
arma::cube gamloopElem(NumericVector beta_, const arma::mat& Y, const arma::mat& Z, arma::colvec gammgrid, const double eps, const arma::colvec YMean2, const arma::colvec ZMean2, arma::mat B1, const int k, const int p);
RcppExport SEXP _bigtime_gamloopElem(SEXP beta_SEXP, SEXP YSEXP, SEXP ZSEXP, SEXP gammgridSEXP, SEXP epsSEXP, SEXP YMean2SEXP, SEXP ZMean2SEXP, SEXP B1SEXP, SEXP kSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type beta_(beta_SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type gammgrid(gammgridSEXP);
    Rcpp::traits::input_parameter< const double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< const arma::colvec >::type YMean2(YMean2SEXP);
    Rcpp::traits::input_parameter< const arma::colvec >::type ZMean2(ZMean2SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B1(B1SEXP);
    Rcpp::traits::input_parameter< const int >::type k(kSEXP);
    Rcpp::traits::input_parameter< const int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(gamloopElem(beta_, Y, Z, gammgrid, eps, YMean2, ZMean2, B1, k, p));
    return rcpp_result_gen;
END_RCPP
}
// ST1a
double ST1a(double z, double gam);
RcppExport SEXP _bigtime_ST1a(SEXP zSEXP, SEXP gamSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type gam(gamSEXP);
    rcpp_result_gen = Rcpp::wrap(ST1a(z, gam));
    return rcpp_result_gen;
END_RCPP
}
// ST3a
arma::colvec ST3a(arma::colvec z, double gam);
RcppExport SEXP _bigtime_ST3a(SEXP zSEXP, SEXP gamSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type gam(gamSEXP);
    rcpp_result_gen = Rcpp::wrap(ST3a(z, gam));
    return rcpp_result_gen;
END_RCPP
}
// gamloopFista
arma::cube gamloopFista(NumericVector beta_, const arma::mat& Y, const arma::mat& Z, const arma::colvec gammgrid, const double eps, const arma::colvec& YMean2, const arma::colvec& ZMean2, arma::mat& B1, int k, int p, double tk, int k1, int s);
RcppExport SEXP _bigtime_gamloopFista(SEXP beta_SEXP, SEXP YSEXP, SEXP ZSEXP, SEXP gammgridSEXP, SEXP epsSEXP, SEXP YMean2SEXP, SEXP ZMean2SEXP, SEXP B1SEXP, SEXP kSEXP, SEXP pSEXP, SEXP tkSEXP, SEXP k1SEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type beta_(beta_SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< const arma::colvec >::type gammgrid(gammgridSEXP);
    Rcpp::traits::input_parameter< const double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type YMean2(YMean2SEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type ZMean2(ZMean2SEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type B1(B1SEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type tk(tkSEXP);
    Rcpp::traits::input_parameter< int >::type k1(k1SEXP);
    Rcpp::traits::input_parameter< int >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(gamloopFista(beta_, Y, Z, gammgrid, eps, YMean2, ZMean2, B1, k, p, tk, k1, s));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_bigtime_norm2", (DL_FUNC) &_bigtime_norm2, 1},
    {"_bigtime_prox2HVAR", (DL_FUNC) &_bigtime_prox2HVAR, 4},
    {"_bigtime_FistaElem", (DL_FUNC) &_bigtime_FistaElem, 8},
    {"_bigtime_gamloopElem", (DL_FUNC) &_bigtime_gamloopElem, 10},
    {"_bigtime_ST1a", (DL_FUNC) &_bigtime_ST1a, 2},
    {"_bigtime_ST3a", (DL_FUNC) &_bigtime_ST3a, 2},
    {"_bigtime_gamloopFista", (DL_FUNC) &_bigtime_gamloopFista, 13},
    {NULL, NULL, 0}
};

RcppExport void R_init_bigtime(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
