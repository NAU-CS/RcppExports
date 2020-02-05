// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// getEigen
arma::vec getEigen(arma::mat M);
RcppExport SEXP _TAG_getEigen(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(getEigen(M));
    return rcpp_result_gen;
END_RCPP
}
// calcADDR3
Rcpp::NumericMatrix calcADDR3(Rcpp::NumericMatrix x, Rcpp::NumericVector theta, Rcpp::NumericVector omega);
RcppExport SEXP _TAG_calcADDR3(SEXP xSEXP, SEXP thetaSEXP, SEXP omegaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type omega(omegaSEXP);
    rcpp_result_gen = Rcpp::wrap(calcADDR3(x, theta, omega));
    return rcpp_result_gen;
END_RCPP
}
// calcProdR
Rcpp::NumericMatrix calcProdR(Rcpp::NumericMatrix x, Rcpp::NumericVector theta);
RcppExport SEXP _TAG_calcProdR(SEXP xSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(calcProdR(x, theta));
    return rcpp_result_gen;
END_RCPP
}
// calcDR
Rcpp::NumericMatrix calcDR(Rcpp::NumericMatrix x, Rcpp::NumericVector theta, double phiest);
RcppExport SEXP _TAG_calcDR(SEXP xSEXP, SEXP thetaSEXP, SEXP phiestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type phiest(phiestSEXP);
    rcpp_result_gen = Rcpp::wrap(calcDR(x, theta, phiest));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_TAG_getEigen", (DL_FUNC) &_TAG_getEigen, 1},
    {"_TAG_calcADDR3", (DL_FUNC) &_TAG_calcADDR3, 3},
    {"_TAG_calcProdR", (DL_FUNC) &_TAG_calcProdR, 2},
    {"_TAG_calcDR", (DL_FUNC) &_TAG_calcDR, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_TAG(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
