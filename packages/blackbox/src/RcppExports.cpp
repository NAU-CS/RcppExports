// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// newCSmooth
SEXP newCSmooth(SEXP xy, SEXP nrowxy, SEXP ncolxy, SEXP nuniquerows, SEXP GCV, SEXP optimiseBool, SEXP verbosity);
RcppExport SEXP _blackbox_newCSmooth(SEXP xySEXP, SEXP nrowxySEXP, SEXP ncolxySEXP, SEXP nuniquerowsSEXP, SEXP GCVSEXP, SEXP optimiseBoolSEXP, SEXP verbositySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type xy(xySEXP);
    Rcpp::traits::input_parameter< SEXP >::type nrowxy(nrowxySEXP);
    Rcpp::traits::input_parameter< SEXP >::type ncolxy(ncolxySEXP);
    Rcpp::traits::input_parameter< SEXP >::type nuniquerows(nuniquerowsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type GCV(GCVSEXP);
    Rcpp::traits::input_parameter< SEXP >::type optimiseBool(optimiseBoolSEXP);
    Rcpp::traits::input_parameter< SEXP >::type verbosity(verbositySEXP);
    rcpp_result_gen = Rcpp::wrap(newCSmooth(xy, nrowxy, ncolxy, nuniquerows, GCV, optimiseBool, verbosity));
    return rcpp_result_gen;
END_RCPP
}
// deleteCSmooth
int deleteCSmooth();
RcppExport SEXP _blackbox_deleteCSmooth() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(deleteCSmooth());
    return rcpp_result_gen;
END_RCPP
}
// flushCSmoothTable
int flushCSmoothTable();
RcppExport SEXP _blackbox_flushCSmoothTable() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(flushCSmoothTable());
    return rcpp_result_gen;
END_RCPP
}
// GCV_lamVar_covFix_Wrapper
SEXP GCV_lamVar_covFix_Wrapper(SEXP a, SEXP fixedSmoothness, SEXP returnFnvalue);
RcppExport SEXP _blackbox_GCV_lamVar_covFix_Wrapper(SEXP aSEXP, SEXP fixedSmoothnessSEXP, SEXP returnFnvalueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type fixedSmoothness(fixedSmoothnessSEXP);
    Rcpp::traits::input_parameter< SEXP >::type returnFnvalue(returnFnvalueSEXP);
    rcpp_result_gen = Rcpp::wrap(GCV_lamVar_covFix_Wrapper(a, fixedSmoothness, returnFnvalue));
    return rcpp_result_gen;
END_RCPP
}
// Krig_coef_Wrapper
List Krig_coef_Wrapper(SEXP aA, SEXP lambdaP);
RcppExport SEXP _blackbox_Krig_coef_Wrapper(SEXP aASEXP, SEXP lambdaPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type aA(aASEXP);
    Rcpp::traits::input_parameter< SEXP >::type lambdaP(lambdaPSEXP);
    rcpp_result_gen = Rcpp::wrap(Krig_coef_Wrapper(aA, lambdaP));
    return rcpp_result_gen;
END_RCPP
}
// getFnEvalCount
int getFnEvalCount();
RcppExport SEXP _blackbox_getFnEvalCount() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getFnEvalCount());
    return rcpp_result_gen;
END_RCPP
}
// CcovFocal
SEXP CcovFocal(SEXP focal, SEXP CKrigidxP);
RcppExport SEXP _blackbox_CcovFocal(SEXP focalSEXP, SEXP CKrigidxPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type focal(focalSEXP);
    Rcpp::traits::input_parameter< SEXP >::type CKrigidxP(CKrigidxPSEXP);
    rcpp_result_gen = Rcpp::wrap(CcovFocal(focal, CKrigidxP));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_blackbox_newCSmooth", (DL_FUNC) &_blackbox_newCSmooth, 7},
    {"_blackbox_deleteCSmooth", (DL_FUNC) &_blackbox_deleteCSmooth, 0},
    {"_blackbox_flushCSmoothTable", (DL_FUNC) &_blackbox_flushCSmoothTable, 0},
    {"_blackbox_GCV_lamVar_covFix_Wrapper", (DL_FUNC) &_blackbox_GCV_lamVar_covFix_Wrapper, 3},
    {"_blackbox_Krig_coef_Wrapper", (DL_FUNC) &_blackbox_Krig_coef_Wrapper, 2},
    {"_blackbox_getFnEvalCount", (DL_FUNC) &_blackbox_getFnEvalCount, 0},
    {"_blackbox_CcovFocal", (DL_FUNC) &_blackbox_CcovFocal, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_blackbox(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
