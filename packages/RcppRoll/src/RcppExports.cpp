// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// na_locf
NumericVector na_locf(NumericVector x);
RcppExport SEXP _RcppRoll_na_locf(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(na_locf(x));
    return rcpp_result_gen;
END_RCPP
}
// roll_mean_impl
SEXP roll_mean_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_mean_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_mean_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// roll_median_impl
SEXP roll_median_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_median_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_median_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// roll_min_impl
SEXP roll_min_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_min_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_min_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// roll_max_impl
SEXP roll_max_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_max_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_max_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// roll_prod_impl
SEXP roll_prod_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_prod_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_prod_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// roll_sum_impl
SEXP roll_sum_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_sum_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sum_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// roll_sd_impl
SEXP roll_sd_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_sd_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sd_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// roll_var_impl
SEXP roll_var_impl(SEXP x, int n, NumericVector weights, int by, NumericVector fill_, bool partial, String align, bool normalize, bool na_rm);
RcppExport SEXP _RcppRoll_roll_var_impl(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP fill_SEXP, SEXP partialSEXP, SEXP alignSEXP, SEXP normalizeSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fill_(fill_SEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< String >::type align(alignSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_var_impl(x, n, weights, by, fill_, partial, align, normalize, na_rm));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RcppRoll_na_locf", (DL_FUNC) &_RcppRoll_na_locf, 1},
    {"_RcppRoll_roll_mean_impl", (DL_FUNC) &_RcppRoll_roll_mean_impl, 9},
    {"_RcppRoll_roll_median_impl", (DL_FUNC) &_RcppRoll_roll_median_impl, 9},
    {"_RcppRoll_roll_min_impl", (DL_FUNC) &_RcppRoll_roll_min_impl, 9},
    {"_RcppRoll_roll_max_impl", (DL_FUNC) &_RcppRoll_roll_max_impl, 9},
    {"_RcppRoll_roll_prod_impl", (DL_FUNC) &_RcppRoll_roll_prod_impl, 9},
    {"_RcppRoll_roll_sum_impl", (DL_FUNC) &_RcppRoll_roll_sum_impl, 9},
    {"_RcppRoll_roll_sd_impl", (DL_FUNC) &_RcppRoll_roll_sd_impl, 9},
    {"_RcppRoll_roll_var_impl", (DL_FUNC) &_RcppRoll_roll_var_impl, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_RcppRoll(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}