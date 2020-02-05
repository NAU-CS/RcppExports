// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// median_filter
NumericMatrix median_filter(NumericMatrix mat, int size, bool na_rm, bool na_count);
RcppExport SEXP _nandb_median_filter(SEXP matSEXP, SEXP sizeSEXP, SEXP na_rmSEXP, SEXP na_countSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_count(na_countSEXP);
    rcpp_result_gen = Rcpp::wrap(median_filter(mat, size, na_rm, na_count));
    return rcpp_result_gen;
END_RCPP
}
// smooth_filter
NumericMatrix smooth_filter(NumericMatrix mat, int size, bool na_rm, bool na_count);
RcppExport SEXP _nandb_smooth_filter(SEXP matSEXP, SEXP sizeSEXP, SEXP na_rmSEXP, SEXP na_countSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_count(na_countSEXP);
    rcpp_result_gen = Rcpp::wrap(smooth_filter(mat, size, na_rm, na_count));
    return rcpp_result_gen;
END_RCPP
}
// cross_var_Cpp
double cross_var_Cpp(NumericVector x, NumericVector y);
RcppExport SEXP _nandb_cross_var_Cpp(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cross_var_Cpp(x, y));
    return rcpp_result_gen;
END_RCPP
}
// cross_var_pillars_Cpp
NumericMatrix cross_var_pillars_Cpp(NumericVector x3d, NumericVector y3d);
RcppExport SEXP _nandb_cross_var_pillars_Cpp(SEXP x3dSEXP, SEXP y3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x3d(x3dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y3d(y3dSEXP);
    rcpp_result_gen = Rcpp::wrap(cross_var_pillars_Cpp(x3d, y3d));
    return rcpp_result_gen;
END_RCPP
}
// float_max
double float_max();
RcppExport SEXP _nandb_float_max() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(float_max());
    return rcpp_result_gen;
END_RCPP
}
// which_interval_
IntegerVector which_interval_(NumericVector numbers, NumericMatrix ranges);
RcppExport SEXP _nandb_which_interval_(SEXP numbersSEXP, SEXP rangesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type numbers(numbersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ranges(rangesSEXP);
    rcpp_result_gen = Rcpp::wrap(which_interval_(numbers, ranges));
    return rcpp_result_gen;
END_RCPP
}
// spread_specific_helper
IntegerVector spread_specific_helper(NumericVector interval_lengths, IntegerVector interval_pops, int m);
RcppExport SEXP _nandb_spread_specific_helper(SEXP interval_lengthsSEXP, SEXP interval_popsSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type interval_lengths(interval_lengthsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type interval_pops(interval_popsSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(spread_specific_helper(interval_lengths, interval_pops, m));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nandb_median_filter", (DL_FUNC) &_nandb_median_filter, 4},
    {"_nandb_smooth_filter", (DL_FUNC) &_nandb_smooth_filter, 4},
    {"_nandb_cross_var_Cpp", (DL_FUNC) &_nandb_cross_var_Cpp, 2},
    {"_nandb_cross_var_pillars_Cpp", (DL_FUNC) &_nandb_cross_var_pillars_Cpp, 2},
    {"_nandb_float_max", (DL_FUNC) &_nandb_float_max, 0},
    {"_nandb_which_interval_", (DL_FUNC) &_nandb_which_interval_, 2},
    {"_nandb_spread_specific_helper", (DL_FUNC) &_nandb_spread_specific_helper, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_nandb(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
