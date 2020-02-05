// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// markerinsert
NumericMatrix markerinsert(NumericMatrix mp, NumericMatrix geno, NumericMatrix map, int cl, int gg1, int gg2, int gg0, int flagRIL);
RcppExport SEXP _QTL_gCIMapping_markerinsert(SEXP mpSEXP, SEXP genoSEXP, SEXP mapSEXP, SEXP clSEXP, SEXP gg1SEXP, SEXP gg2SEXP, SEXP gg0SEXP, SEXP flagRILSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mp(mpSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type map(mapSEXP);
    Rcpp::traits::input_parameter< int >::type cl(clSEXP);
    Rcpp::traits::input_parameter< int >::type gg1(gg1SEXP);
    Rcpp::traits::input_parameter< int >::type gg2(gg2SEXP);
    Rcpp::traits::input_parameter< int >::type gg0(gg0SEXP);
    Rcpp::traits::input_parameter< int >::type flagRIL(flagRILSEXP);
    rcpp_result_gen = Rcpp::wrap(markerinsert(mp, geno, map, cl, gg1, gg2, gg0, flagRIL));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_QTL_gCIMapping_markerinsert", (DL_FUNC) &_QTL_gCIMapping_markerinsert, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_QTL_gCIMapping(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
