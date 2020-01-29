// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// get_honest_C
NumericVector get_honest_C(List x, NumericVector y, NumericMatrix z, NumericMatrix w);
RcppExport SEXP _orf_get_honest_C(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type z(zSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(get_honest_C(x, y, z, w));
    return rcpp_result_gen;
END_RCPP
}
// get_weights_C
NumericMatrix get_weights_C(List x, List y, List z);
RcppExport SEXP _orf_get_weights_C(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< List >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(get_weights_C(x, y, z));
    return rcpp_result_gen;
END_RCPP
}
// pred_honest_C
NumericVector pred_honest_C(List x, NumericVector y, NumericMatrix z, NumericMatrix w);
RcppExport SEXP _orf_pred_honest_C(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type z(zSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(pred_honest_C(x, y, z, w));
    return rcpp_result_gen;
END_RCPP
}
// pred_weights_C
NumericVector pred_weights_C(List x, List y, List z, int w);
RcppExport SEXP _orf_pred_weights_C(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< List >::type z(zSEXP);
    Rcpp::traits::input_parameter< int >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(pred_weights_C(x, y, z, w));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_orf_get_honest_C", (DL_FUNC) &_orf_get_honest_C, 4},
    {"_orf_get_weights_C", (DL_FUNC) &_orf_get_weights_C, 3},
    {"_orf_pred_honest_C", (DL_FUNC) &_orf_pred_honest_C, 4},
    {"_orf_pred_weights_C", (DL_FUNC) &_orf_pred_weights_C, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_orf(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}