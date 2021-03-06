// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// logloss
double logloss(const NumericMatrix& m, NumericVector outcome, NumericVector rankweights);
RcppExport SEXP _socceR_logloss(SEXP mSEXP, SEXP outcomeSEXP, SEXP rankweightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outcome(outcomeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rankweights(rankweightsSEXP);
    rcpp_result_gen = Rcpp::wrap(logloss(m, outcome, rankweights));
    return rcpp_result_gen;
END_RCPP
}
// trps
double trps(const NumericMatrix& m, NumericVector outcome, NumericVector rankweights);
RcppExport SEXP _socceR_trps(SEXP mSEXP, SEXP outcomeSEXP, SEXP rankweightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outcome(outcomeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rankweights(rankweightsSEXP);
    rcpp_result_gen = Rcpp::wrap(trps(m, outcome, rankweights));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_socceR_logloss", (DL_FUNC) &_socceR_logloss, 3},
    {"_socceR_trps", (DL_FUNC) &_socceR_trps, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_socceR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
