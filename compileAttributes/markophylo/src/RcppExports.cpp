// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// loopC
NumericVector loopC(NumericVector nodelist, int al, IntegerVector x, IntegerVector x2, List pm_lc, NumericMatrix Lix, int finind);
RcppExport SEXP _markophylo_loopC(SEXP nodelistSEXP, SEXP alSEXP, SEXP xSEXP, SEXP x2SEXP, SEXP pm_lcSEXP, SEXP LixSEXP, SEXP finindSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type nodelist(nodelistSEXP);
    Rcpp::traits::input_parameter< int >::type al(alSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< List >::type pm_lc(pm_lcSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Lix(LixSEXP);
    Rcpp::traits::input_parameter< int >::type finind(finindSEXP);
    rcpp_result_gen = Rcpp::wrap(loopC(nodelist, al, x, x2, pm_lc, Lix, finind));
    return rcpp_result_gen;
END_RCPP
}
// colMeansC
NumericVector colMeansC(NumericMatrix x);
RcppExport SEXP _markophylo_colMeansC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(colMeansC(x));
    return rcpp_result_gen;
END_RCPP
}
// cat_loopC
NumericVector cat_loopC(int nocat, NumericVector nodelist, int al, IntegerVector x, IntegerVector x2, List pm_clc, NumericMatrix Lix, int finind);
RcppExport SEXP _markophylo_cat_loopC(SEXP nocatSEXP, SEXP nodelistSEXP, SEXP alSEXP, SEXP xSEXP, SEXP x2SEXP, SEXP pm_clcSEXP, SEXP LixSEXP, SEXP finindSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nocat(nocatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type nodelist(nodelistSEXP);
    Rcpp::traits::input_parameter< int >::type al(alSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< List >::type pm_clc(pm_clcSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Lix(LixSEXP);
    Rcpp::traits::input_parameter< int >::type finind(finindSEXP);
    rcpp_result_gen = Rcpp::wrap(cat_loopC(nocat, nodelist, al, x, x2, pm_clc, Lix, finind));
    return rcpp_result_gen;
END_RCPP
}
// part_loopC
NumericMatrix part_loopC(int nocat, NumericVector nodelist, int al, IntegerVector x, IntegerVector x2, List pm_plc, List Lix, int finind);
RcppExport SEXP _markophylo_part_loopC(SEXP nocatSEXP, SEXP nodelistSEXP, SEXP alSEXP, SEXP xSEXP, SEXP x2SEXP, SEXP pm_plcSEXP, SEXP LixSEXP, SEXP finindSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nocat(nocatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type nodelist(nodelistSEXP);
    Rcpp::traits::input_parameter< int >::type al(alSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< List >::type pm_plc(pm_plcSEXP);
    Rcpp::traits::input_parameter< List >::type Lix(LixSEXP);
    Rcpp::traits::input_parameter< int >::type finind(finindSEXP);
    rcpp_result_gen = Rcpp::wrap(part_loopC(nocat, nodelist, al, x, x2, pm_plc, Lix, finind));
    return rcpp_result_gen;
END_RCPP
}
// expm
arma::mat expm(const arma::mat& M);
RcppExport SEXP _markophylo_expm(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(expm(M));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_markophylo_loopC", (DL_FUNC) &_markophylo_loopC, 7},
    {"_markophylo_colMeansC", (DL_FUNC) &_markophylo_colMeansC, 1},
    {"_markophylo_cat_loopC", (DL_FUNC) &_markophylo_cat_loopC, 8},
    {"_markophylo_part_loopC", (DL_FUNC) &_markophylo_part_loopC, 8},
    {"_markophylo_expm", (DL_FUNC) &_markophylo_expm, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_markophylo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}