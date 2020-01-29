// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// timer_error
long double timer_error(std::size_t rounds);
RcppExport SEXP _benchr_timer_error(SEXP roundsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::size_t >::type rounds(roundsSEXP);
    rcpp_result_gen = Rcpp::wrap(timer_error(rounds));
    return rcpp_result_gen;
END_RCPP
}
// timer_precision
double timer_precision();
RcppExport SEXP _benchr_timer_precision() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(timer_precision());
    return rcpp_result_gen;
END_RCPP
}
// do_timing
long double do_timing(const RObject& expr, const Environment& env);
RcppExport SEXP _benchr_do_timing(SEXP exprSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RObject& >::type expr(exprSEXP);
    Rcpp::traits::input_parameter< const Environment& >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(do_timing(expr, env));
    return rcpp_result_gen;
END_RCPP
}
// do_benchmark
NumericVector do_benchmark(const List& exprs, const Environment& env, const IntegerVector& order, bool gc, bool progress);
RcppExport SEXP _benchr_do_benchmark(SEXP exprsSEXP, SEXP envSEXP, SEXP orderSEXP, SEXP gcSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type exprs(exprsSEXP);
    Rcpp::traits::input_parameter< const Environment& >::type env(envSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type order(orderSEXP);
    Rcpp::traits::input_parameter< bool >::type gc(gcSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(do_benchmark(exprs, env, order, gc, progress));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_benchr_timer_error", (DL_FUNC) &_benchr_timer_error, 1},
    {"_benchr_timer_precision", (DL_FUNC) &_benchr_timer_precision, 0},
    {"_benchr_do_timing", (DL_FUNC) &_benchr_do_timing, 2},
    {"_benchr_do_benchmark", (DL_FUNC) &_benchr_do_benchmark, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_benchr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}