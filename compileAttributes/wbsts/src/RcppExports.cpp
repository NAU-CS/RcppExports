// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cusum
NumericVector cusum(NumericVector x);
RcppExport SEXP _wbsts_cusum(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cusum(x));
    return rcpp_result_gen;
END_RCPP
}
// finner_prod_maxp
NumericVector finner_prod_maxp(NumericVector x, double p);
RcppExport SEXP _wbsts_finner_prod_maxp(SEXP xSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(finner_prod_maxp(x, p));
    return rcpp_result_gen;
END_RCPP
}
// across_fip
NumericVector across_fip(NumericMatrix X, NumericVector tau, double p, NumericVector epp, double p1, double Ts);
RcppExport SEXP _wbsts_across_fip(SEXP XSEXP, SEXP tauSEXP, SEXP pSEXP, SEXP eppSEXP, SEXP p1SEXP, SEXP TsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type epp(eppSEXP);
    Rcpp::traits::input_parameter< double >::type p1(p1SEXP);
    Rcpp::traits::input_parameter< double >::type Ts(TsSEXP);
    rcpp_result_gen = Rcpp::wrap(across_fip(X, tau, p, epp, p1, Ts));
    return rcpp_result_gen;
END_RCPP
}
// multi_across_fip
List multi_across_fip(NumericMatrix X, int M, double min_draw, NumericVector tau, NumericVector p, NumericVector epp, double Ts);
RcppExport SEXP _wbsts_multi_across_fip(SEXP XSEXP, SEXP MSEXP, SEXP min_drawSEXP, SEXP tauSEXP, SEXP pSEXP, SEXP eppSEXP, SEXP TsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< double >::type min_draw(min_drawSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type epp(eppSEXP);
    Rcpp::traits::input_parameter< double >::type Ts(TsSEXP);
    rcpp_result_gen = Rcpp::wrap(multi_across_fip(X, M, min_draw, tau, p, epp, Ts));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_wbsts_cusum", (DL_FUNC) &_wbsts_cusum, 1},
    {"_wbsts_finner_prod_maxp", (DL_FUNC) &_wbsts_finner_prod_maxp, 2},
    {"_wbsts_across_fip", (DL_FUNC) &_wbsts_across_fip, 6},
    {"_wbsts_multi_across_fip", (DL_FUNC) &_wbsts_multi_across_fip, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_wbsts(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}