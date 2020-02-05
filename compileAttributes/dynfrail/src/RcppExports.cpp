// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// Estep_id
NumericVector Estep_id(NumericVector events, NumericVector cvec, double aalpha, double ggamma, int dist, double pvfm, NumericVector times, double llambda);
RcppExport SEXP _dynfrail_Estep_id(SEXP eventsSEXP, SEXP cvecSEXP, SEXP aalphaSEXP, SEXP ggammaSEXP, SEXP distSEXP, SEXP pvfmSEXP, SEXP timesSEXP, SEXP llambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type events(eventsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cvec(cvecSEXP);
    Rcpp::traits::input_parameter< double >::type aalpha(aalphaSEXP);
    Rcpp::traits::input_parameter< double >::type ggamma(ggammaSEXP);
    Rcpp::traits::input_parameter< int >::type dist(distSEXP);
    Rcpp::traits::input_parameter< double >::type pvfm(pvfmSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type times(timesSEXP);
    Rcpp::traits::input_parameter< double >::type llambda(llambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(Estep_id(events, cvec, aalpha, ggamma, dist, pvfm, times, llambda));
    return rcpp_result_gen;
END_RCPP
}
// Vcov_adj
List Vcov_adj(List events_l, List cvec_l, double aalpha, double ggamma, int dist, double pvfm, List times_l, double llambda, List elp_l, List xelph_l, List tau_l, List interval_rows_l, List ez_l, int n_times, int n_covs);
RcppExport SEXP _dynfrail_Vcov_adj(SEXP events_lSEXP, SEXP cvec_lSEXP, SEXP aalphaSEXP, SEXP ggammaSEXP, SEXP distSEXP, SEXP pvfmSEXP, SEXP times_lSEXP, SEXP llambdaSEXP, SEXP elp_lSEXP, SEXP xelph_lSEXP, SEXP tau_lSEXP, SEXP interval_rows_lSEXP, SEXP ez_lSEXP, SEXP n_timesSEXP, SEXP n_covsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type events_l(events_lSEXP);
    Rcpp::traits::input_parameter< List >::type cvec_l(cvec_lSEXP);
    Rcpp::traits::input_parameter< double >::type aalpha(aalphaSEXP);
    Rcpp::traits::input_parameter< double >::type ggamma(ggammaSEXP);
    Rcpp::traits::input_parameter< int >::type dist(distSEXP);
    Rcpp::traits::input_parameter< double >::type pvfm(pvfmSEXP);
    Rcpp::traits::input_parameter< List >::type times_l(times_lSEXP);
    Rcpp::traits::input_parameter< double >::type llambda(llambdaSEXP);
    Rcpp::traits::input_parameter< List >::type elp_l(elp_lSEXP);
    Rcpp::traits::input_parameter< List >::type xelph_l(xelph_lSEXP);
    Rcpp::traits::input_parameter< List >::type tau_l(tau_lSEXP);
    Rcpp::traits::input_parameter< List >::type interval_rows_l(interval_rows_lSEXP);
    Rcpp::traits::input_parameter< List >::type ez_l(ez_lSEXP);
    Rcpp::traits::input_parameter< int >::type n_times(n_timesSEXP);
    Rcpp::traits::input_parameter< int >::type n_covs(n_covsSEXP);
    rcpp_result_gen = Rcpp::wrap(Vcov_adj(events_l, cvec_l, aalpha, ggamma, dist, pvfm, times_l, llambda, elp_l, xelph_l, tau_l, interval_rows_l, ez_l, n_times, n_covs));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dynfrail_Estep_id", (DL_FUNC) &_dynfrail_Estep_id, 8},
    {"_dynfrail_Vcov_adj", (DL_FUNC) &_dynfrail_Vcov_adj, 15},
    {NULL, NULL, 0}
};

RcppExport void R_init_dynfrail(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
