// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// COX
double COX(NumericVector tps, NumericVector cureE, NumericVector gene, NumericVector delta);
RcppExport SEXP _iBST_COX(SEXP tpsSEXP, SEXP cureESEXP, SEXP geneSEXP, SEXP deltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tps(tpsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cureE(cureESEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gene(geneSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type delta(deltaSEXP);
    rcpp_result_gen = Rcpp::wrap(COX(tps, cureE, gene, delta));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP iBST_COX(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_iBST_COX", (DL_FUNC) &_iBST_COX, 4},
    {"iBST_COX", (DL_FUNC) &iBST_COX, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_iBST(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
