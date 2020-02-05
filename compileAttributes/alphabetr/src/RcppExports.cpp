// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// dual_discrim_dual_likelihood
NumericVector dual_discrim_dual_likelihood(double est, double err, NumericVector numb_cells, NumericMatrix numb_wells, List binomials);
RcppExport SEXP _alphabetr_dual_discrim_dual_likelihood(SEXP estSEXP, SEXP errSEXP, SEXP numb_cellsSEXP, SEXP numb_wellsSEXP, SEXP binomialsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type est(estSEXP);
    Rcpp::traits::input_parameter< double >::type err(errSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_cells(numb_cellsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type numb_wells(numb_wellsSEXP);
    Rcpp::traits::input_parameter< List >::type binomials(binomialsSEXP);
    rcpp_result_gen = Rcpp::wrap(dual_discrim_dual_likelihood(est, err, numb_cells, numb_wells, binomials));
    return rcpp_result_gen;
END_RCPP
}
// dual_discrim_shared_likelihood
NumericVector dual_discrim_shared_likelihood(double est1, double est2, double err, NumericVector numb_cells, NumericMatrix numb_wells, List binomials, List multinomials);
RcppExport SEXP _alphabetr_dual_discrim_shared_likelihood(SEXP est1SEXP, SEXP est2SEXP, SEXP errSEXP, SEXP numb_cellsSEXP, SEXP numb_wellsSEXP, SEXP binomialsSEXP, SEXP multinomialsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type est1(est1SEXP);
    Rcpp::traits::input_parameter< double >::type est2(est2SEXP);
    Rcpp::traits::input_parameter< double >::type err(errSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_cells(numb_cellsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type numb_wells(numb_wellsSEXP);
    Rcpp::traits::input_parameter< List >::type binomials(binomialsSEXP);
    Rcpp::traits::input_parameter< List >::type multinomials(multinomialsSEXP);
    rcpp_result_gen = Rcpp::wrap(dual_discrim_shared_likelihood(est1, est2, err, numb_cells, numb_wells, binomials, multinomials));
    return rcpp_result_gen;
END_RCPP
}
// likelihood_dual
double likelihood_dual(double est, double err, NumericVector numb_wells, NumericVector numb_cells, NumericVector numb_sample);
RcppExport SEXP _alphabetr_likelihood_dual(SEXP estSEXP, SEXP errSEXP, SEXP numb_wellsSEXP, SEXP numb_cellsSEXP, SEXP numb_sampleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type est(estSEXP);
    Rcpp::traits::input_parameter< double >::type err(errSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_wells(numb_wellsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_cells(numb_cellsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_sample(numb_sampleSEXP);
    rcpp_result_gen = Rcpp::wrap(likelihood_dual(est, err, numb_wells, numb_cells, numb_sample));
    return rcpp_result_gen;
END_RCPP
}
// likelihood_dualdual
double likelihood_dualdual(double est, double err, NumericVector numb_wells, NumericVector numb_cells, NumericVector numb_sample);
RcppExport SEXP _alphabetr_likelihood_dualdual(SEXP estSEXP, SEXP errSEXP, SEXP numb_wellsSEXP, SEXP numb_cellsSEXP, SEXP numb_sampleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type est(estSEXP);
    Rcpp::traits::input_parameter< double >::type err(errSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_wells(numb_wellsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_cells(numb_cellsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_sample(numb_sampleSEXP);
    rcpp_result_gen = Rcpp::wrap(likelihood_dualdual(est, err, numb_wells, numb_cells, numb_sample));
    return rcpp_result_gen;
END_RCPP
}
// likelihood_single
double likelihood_single(double est, double err, NumericVector numb_wells, NumericVector numb_cells, NumericVector numb_sample);
RcppExport SEXP _alphabetr_likelihood_single(SEXP estSEXP, SEXP errSEXP, SEXP numb_wellsSEXP, SEXP numb_cellsSEXP, SEXP numb_sampleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type est(estSEXP);
    Rcpp::traits::input_parameter< double >::type err(errSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_wells(numb_wellsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_cells(numb_cellsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type numb_sample(numb_sampleSEXP);
    rcpp_result_gen = Rcpp::wrap(likelihood_single(est, err, numb_wells, numb_cells, numb_sample));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_alphabetr_dual_discrim_dual_likelihood", (DL_FUNC) &_alphabetr_dual_discrim_dual_likelihood, 5},
    {"_alphabetr_dual_discrim_shared_likelihood", (DL_FUNC) &_alphabetr_dual_discrim_shared_likelihood, 7},
    {"_alphabetr_likelihood_dual", (DL_FUNC) &_alphabetr_likelihood_dual, 5},
    {"_alphabetr_likelihood_dualdual", (DL_FUNC) &_alphabetr_likelihood_dualdual, 5},
    {"_alphabetr_likelihood_single", (DL_FUNC) &_alphabetr_likelihood_single, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_alphabetr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
