// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// order_cpp
IntegerVector order_cpp(NumericVector x);
RcppExport SEXP _twosamples_order_cpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(order_cpp(x));
    return rcpp_result_gen;
END_RCPP
}
// ks_stat
double ks_stat(NumericVector a, NumericVector b, double power);
RcppExport SEXP _twosamples_ks_stat(SEXP aSEXP, SEXP bSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(ks_stat(a, b, power));
    return rcpp_result_gen;
END_RCPP
}
// kuiper_stat
double kuiper_stat(NumericVector a, NumericVector b, double power);
RcppExport SEXP _twosamples_kuiper_stat(SEXP aSEXP, SEXP bSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(kuiper_stat(a, b, power));
    return rcpp_result_gen;
END_RCPP
}
// cvm_stat
double cvm_stat(NumericVector a, NumericVector b, double power);
RcppExport SEXP _twosamples_cvm_stat(SEXP aSEXP, SEXP bSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(cvm_stat(a, b, power));
    return rcpp_result_gen;
END_RCPP
}
// ad_stat
double ad_stat(NumericVector a, NumericVector b, double power);
RcppExport SEXP _twosamples_ad_stat(SEXP aSEXP, SEXP bSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(ad_stat(a, b, power));
    return rcpp_result_gen;
END_RCPP
}
// wass_stat
double wass_stat(NumericVector a, NumericVector b, double power);
RcppExport SEXP _twosamples_wass_stat(SEXP aSEXP, SEXP bSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(wass_stat(a, b, power));
    return rcpp_result_gen;
END_RCPP
}
// dts_stat
double dts_stat(NumericVector a, NumericVector b, double power);
RcppExport SEXP _twosamples_dts_stat(SEXP aSEXP, SEXP bSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(dts_stat(a, b, power));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_twosamples_order_cpp", (DL_FUNC) &_twosamples_order_cpp, 1},
    {"_twosamples_ks_stat", (DL_FUNC) &_twosamples_ks_stat, 3},
    {"_twosamples_kuiper_stat", (DL_FUNC) &_twosamples_kuiper_stat, 3},
    {"_twosamples_cvm_stat", (DL_FUNC) &_twosamples_cvm_stat, 3},
    {"_twosamples_ad_stat", (DL_FUNC) &_twosamples_ad_stat, 3},
    {"_twosamples_wass_stat", (DL_FUNC) &_twosamples_wass_stat, 3},
    {"_twosamples_dts_stat", (DL_FUNC) &_twosamples_dts_stat, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_twosamples(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
