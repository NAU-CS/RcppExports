// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// getGamma
arma::mat getGamma(const arma::vec expbZ, const arma::vec X, const arma::vec T, const arma::mat Z, const arma::vec wh);
RcppExport SEXP _coxrt_getGamma(SEXP expbZSEXP, SEXP XSEXP, SEXP TSEXP, SEXP ZSEXP, SEXP whSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type expbZ(expbZSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type T(TSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type wh(whSEXP);
    rcpp_result_gen = Rcpp::wrap(getGamma(expbZ, X, T, Z, wh));
    return rcpp_result_gen;
END_RCPP
}
// getSigma_cpp
arma::mat getSigma_cpp(const arma::mat Z, const arma::mat TMP, const arma::vec S0, const arma::mat xi, const arma::mat dMi_Tl);
RcppExport SEXP _coxrt_getSigma_cpp(SEXP ZSEXP, SEXP TMPSEXP, SEXP S0SEXP, SEXP xiSEXP, SEXP dMi_TlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type TMP(TMPSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type S0(S0SEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type dMi_Tl(dMi_TlSEXP);
    rcpp_result_gen = Rcpp::wrap(getSigma_cpp(Z, TMP, S0, xi, dMi_Tl));
    return rcpp_result_gen;
END_RCPP
}
// getVar
arma::mat getVar(const arma::vec exp_bZ, const arma::vec X, const arma::vec T, const arma::mat Z, const arma::vec wh);
RcppExport SEXP _coxrt_getVar(SEXP exp_bZSEXP, SEXP XSEXP, SEXP TSEXP, SEXP ZSEXP, SEXP whSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type exp_bZ(exp_bZSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type T(TSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type wh(whSEXP);
    rcpp_result_gen = Rcpp::wrap(getVar(exp_bZ, X, T, Z, wh));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coxrt_getGamma", (DL_FUNC) &_coxrt_getGamma, 5},
    {"_coxrt_getSigma_cpp", (DL_FUNC) &_coxrt_getSigma_cpp, 5},
    {"_coxrt_getVar", (DL_FUNC) &_coxrt_getVar, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_coxrt(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
