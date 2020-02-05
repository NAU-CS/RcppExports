// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// es_raw
double es_raw(IntegerVector S, NumericVector r);
RcppExport SEXP _gsEasy_es_raw(SEXP SSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type S(SSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(es_raw(S, r));
    return rcpp_result_gen;
END_RCPP
}
// gset_raw
double gset_raw(int N, IntegerVector S, NumericVector r, int min_its, int max_its, double signif, double log_dismiss);
RcppExport SEXP _gsEasy_gset_raw(SEXP NSEXP, SEXP SSEXP, SEXP rSEXP, SEXP min_itsSEXP, SEXP max_itsSEXP, SEXP signifSEXP, SEXP log_dismissSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S(SSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< int >::type min_its(min_itsSEXP);
    Rcpp::traits::input_parameter< int >::type max_its(max_itsSEXP);
    Rcpp::traits::input_parameter< double >::type signif(signifSEXP);
    Rcpp::traits::input_parameter< double >::type log_dismiss(log_dismissSEXP);
    rcpp_result_gen = Rcpp::wrap(gset_raw(N, S, r, min_its, max_its, signif, log_dismiss));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_gsEasy_es_raw", (DL_FUNC) &_gsEasy_es_raw, 2},
    {"_gsEasy_gset_raw", (DL_FUNC) &_gsEasy_gset_raw, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_gsEasy(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}