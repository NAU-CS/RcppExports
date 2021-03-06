// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// hestMakeModel
SEXP hestMakeModel();
RcppExport SEXP _msde_hestMakeModel() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(hestMakeModel());
    return rcpp_result_gen;
END_RCPP
}
// biouMakeModel
SEXP biouMakeModel();
RcppExport SEXP _msde_biouMakeModel() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(biouMakeModel());
    return rcpp_result_gen;
END_RCPP
}
// pgnetMakeModel
SEXP pgnetMakeModel();
RcppExport SEXP _msde_pgnetMakeModel() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(pgnetMakeModel());
    return rcpp_result_gen;
END_RCPP
}
// lotvolMakeModel
SEXP lotvolMakeModel();
RcppExport SEXP _msde_lotvolMakeModel() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(lotvolMakeModel());
    return rcpp_result_gen;
END_RCPP
}
// eouMakeModel
SEXP eouMakeModel();
RcppExport SEXP _msde_eouMakeModel() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(eouMakeModel());
    return rcpp_result_gen;
END_RCPP
}
// sde_nParams
double sde_nParams(SEXP sdeptr);
RcppExport SEXP _msde_sde_nParams(SEXP sdeptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    rcpp_result_gen = Rcpp::wrap(sde_nParams(sdeptr));
    return rcpp_result_gen;
END_RCPP
}
// sde_nDims
double sde_nDims(SEXP sdeptr);
RcppExport SEXP _msde_sde_nDims(SEXP sdeptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    rcpp_result_gen = Rcpp::wrap(sde_nDims(sdeptr));
    return rcpp_result_gen;
END_RCPP
}
// isData
LogicalVector isData(SEXP sdeptr, NumericVector xIn, NumericVector thetaIn, bool singleX, bool singleTheta, int nReps);
RcppExport SEXP _msde_isData(SEXP sdeptrSEXP, SEXP xInSEXP, SEXP thetaInSEXP, SEXP singleXSEXP, SEXP singleThetaSEXP, SEXP nRepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xIn(xInSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thetaIn(thetaInSEXP);
    Rcpp::traits::input_parameter< bool >::type singleX(singleXSEXP);
    Rcpp::traits::input_parameter< bool >::type singleTheta(singleThetaSEXP);
    Rcpp::traits::input_parameter< int >::type nReps(nRepsSEXP);
    rcpp_result_gen = Rcpp::wrap(isData(sdeptr, xIn, thetaIn, singleX, singleTheta, nReps));
    return rcpp_result_gen;
END_RCPP
}
// isParams
LogicalVector isParams(SEXP sdeptr, NumericVector thetaIn, int nReps);
RcppExport SEXP _msde_isParams(SEXP sdeptrSEXP, SEXP thetaInSEXP, SEXP nRepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thetaIn(thetaInSEXP);
    Rcpp::traits::input_parameter< int >::type nReps(nRepsSEXP);
    rcpp_result_gen = Rcpp::wrap(isParams(sdeptr, thetaIn, nReps));
    return rcpp_result_gen;
END_RCPP
}
// Drift
NumericVector Drift(SEXP sdeptr, NumericVector xIn, NumericVector thetaIn, bool singleX, bool singleTheta, int nReps);
RcppExport SEXP _msde_Drift(SEXP sdeptrSEXP, SEXP xInSEXP, SEXP thetaInSEXP, SEXP singleXSEXP, SEXP singleThetaSEXP, SEXP nRepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xIn(xInSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thetaIn(thetaInSEXP);
    Rcpp::traits::input_parameter< bool >::type singleX(singleXSEXP);
    Rcpp::traits::input_parameter< bool >::type singleTheta(singleThetaSEXP);
    Rcpp::traits::input_parameter< int >::type nReps(nRepsSEXP);
    rcpp_result_gen = Rcpp::wrap(Drift(sdeptr, xIn, thetaIn, singleX, singleTheta, nReps));
    return rcpp_result_gen;
END_RCPP
}
// Diff
NumericVector Diff(SEXP sdeptr, NumericVector xIn, NumericVector thetaIn, bool singleX, bool singleTheta, int nReps);
RcppExport SEXP _msde_Diff(SEXP sdeptrSEXP, SEXP xInSEXP, SEXP thetaInSEXP, SEXP singleXSEXP, SEXP singleThetaSEXP, SEXP nRepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xIn(xInSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thetaIn(thetaInSEXP);
    Rcpp::traits::input_parameter< bool >::type singleX(singleXSEXP);
    Rcpp::traits::input_parameter< bool >::type singleTheta(singleThetaSEXP);
    Rcpp::traits::input_parameter< int >::type nReps(nRepsSEXP);
    rcpp_result_gen = Rcpp::wrap(Diff(sdeptr, xIn, thetaIn, singleX, singleTheta, nReps));
    return rcpp_result_gen;
END_RCPP
}
// LogLik
NumericVector LogLik(SEXP sdeptr, NumericVector xIn, NumericVector dTIn, NumericVector thetaIn, int nComp, int nReps, bool singleX, bool singleTheta, int nCores);
RcppExport SEXP _msde_LogLik(SEXP sdeptrSEXP, SEXP xInSEXP, SEXP dTInSEXP, SEXP thetaInSEXP, SEXP nCompSEXP, SEXP nRepsSEXP, SEXP singleXSEXP, SEXP singleThetaSEXP, SEXP nCoresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xIn(xInSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dTIn(dTInSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thetaIn(thetaInSEXP);
    Rcpp::traits::input_parameter< int >::type nComp(nCompSEXP);
    Rcpp::traits::input_parameter< int >::type nReps(nRepsSEXP);
    Rcpp::traits::input_parameter< bool >::type singleX(singleXSEXP);
    Rcpp::traits::input_parameter< bool >::type singleTheta(singleThetaSEXP);
    Rcpp::traits::input_parameter< int >::type nCores(nCoresSEXP);
    rcpp_result_gen = Rcpp::wrap(LogLik(sdeptr, xIn, dTIn, thetaIn, nComp, nReps, singleX, singleTheta, nCores));
    return rcpp_result_gen;
END_RCPP
}
// Prior
NumericVector Prior(SEXP sdeptr, NumericVector thetaIn, NumericVector xIn, bool singleTheta, bool singleX, int nReps, List phiIn);
RcppExport SEXP _msde_Prior(SEXP sdeptrSEXP, SEXP thetaInSEXP, SEXP xInSEXP, SEXP singleThetaSEXP, SEXP singleXSEXP, SEXP nRepsSEXP, SEXP phiInSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thetaIn(thetaInSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xIn(xInSEXP);
    Rcpp::traits::input_parameter< bool >::type singleTheta(singleThetaSEXP);
    Rcpp::traits::input_parameter< bool >::type singleX(singleXSEXP);
    Rcpp::traits::input_parameter< int >::type nReps(nRepsSEXP);
    Rcpp::traits::input_parameter< List >::type phiIn(phiInSEXP);
    rcpp_result_gen = Rcpp::wrap(Prior(sdeptr, thetaIn, xIn, singleTheta, singleX, nReps, phiIn));
    return rcpp_result_gen;
END_RCPP
}
// Sim
List Sim(SEXP sdeptr, int nDataOut, int N, int burn, int reps, int r, double dT, int MAXBAD, NumericVector initData, NumericVector params, bool singleX, bool singleTheta);
RcppExport SEXP _msde_Sim(SEXP sdeptrSEXP, SEXP nDataOutSEXP, SEXP NSEXP, SEXP burnSEXP, SEXP repsSEXP, SEXP rSEXP, SEXP dTSEXP, SEXP MAXBADSEXP, SEXP initDataSEXP, SEXP paramsSEXP, SEXP singleXSEXP, SEXP singleThetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< int >::type nDataOut(nDataOutSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type burn(burnSEXP);
    Rcpp::traits::input_parameter< int >::type reps(repsSEXP);
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type dT(dTSEXP);
    Rcpp::traits::input_parameter< int >::type MAXBAD(MAXBADSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type initData(initDataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< bool >::type singleX(singleXSEXP);
    Rcpp::traits::input_parameter< bool >::type singleTheta(singleThetaSEXP);
    rcpp_result_gen = Rcpp::wrap(Sim(sdeptr, nDataOut, N, burn, reps, r, dT, MAXBAD, initData, params, singleX, singleTheta));
    return rcpp_result_gen;
END_RCPP
}
// Post
List Post(SEXP sdeptr, NumericVector initParams, NumericVector initData, NumericVector dT, IntegerVector nDimsPerObs, LogicalVector fixedParams, int nSamples, int burn, int nParamsOut, int nDataOut, IntegerVector dataOutSmp, IntegerVector dataOutComp, IntegerVector dataOutDims, double updateParams, double updateData, List priorArgs, List tunePar, int updateLogLik, int nLogLikOut, int updateLastMiss, int nLastMissOut, int nCores, bool displayProgress);
RcppExport SEXP _msde_Post(SEXP sdeptrSEXP, SEXP initParamsSEXP, SEXP initDataSEXP, SEXP dTSEXP, SEXP nDimsPerObsSEXP, SEXP fixedParamsSEXP, SEXP nSamplesSEXP, SEXP burnSEXP, SEXP nParamsOutSEXP, SEXP nDataOutSEXP, SEXP dataOutSmpSEXP, SEXP dataOutCompSEXP, SEXP dataOutDimsSEXP, SEXP updateParamsSEXP, SEXP updateDataSEXP, SEXP priorArgsSEXP, SEXP tuneParSEXP, SEXP updateLogLikSEXP, SEXP nLogLikOutSEXP, SEXP updateLastMissSEXP, SEXP nLastMissOutSEXP, SEXP nCoresSEXP, SEXP displayProgressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sdeptr(sdeptrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type initParams(initParamsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type initData(initDataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dT(dTSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type nDimsPerObs(nDimsPerObsSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type fixedParams(fixedParamsSEXP);
    Rcpp::traits::input_parameter< int >::type nSamples(nSamplesSEXP);
    Rcpp::traits::input_parameter< int >::type burn(burnSEXP);
    Rcpp::traits::input_parameter< int >::type nParamsOut(nParamsOutSEXP);
    Rcpp::traits::input_parameter< int >::type nDataOut(nDataOutSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dataOutSmp(dataOutSmpSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dataOutComp(dataOutCompSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dataOutDims(dataOutDimsSEXP);
    Rcpp::traits::input_parameter< double >::type updateParams(updateParamsSEXP);
    Rcpp::traits::input_parameter< double >::type updateData(updateDataSEXP);
    Rcpp::traits::input_parameter< List >::type priorArgs(priorArgsSEXP);
    Rcpp::traits::input_parameter< List >::type tunePar(tuneParSEXP);
    Rcpp::traits::input_parameter< int >::type updateLogLik(updateLogLikSEXP);
    Rcpp::traits::input_parameter< int >::type nLogLikOut(nLogLikOutSEXP);
    Rcpp::traits::input_parameter< int >::type updateLastMiss(updateLastMissSEXP);
    Rcpp::traits::input_parameter< int >::type nLastMissOut(nLastMissOutSEXP);
    Rcpp::traits::input_parameter< int >::type nCores(nCoresSEXP);
    Rcpp::traits::input_parameter< bool >::type displayProgress(displayProgressSEXP);
    rcpp_result_gen = Rcpp::wrap(Post(sdeptr, initParams, initData, dT, nDimsPerObs, fixedParams, nSamples, burn, nParamsOut, nDataOut, dataOutSmp, dataOutComp, dataOutDims, updateParams, updateData, priorArgs, tunePar, updateLogLik, nLogLikOut, updateLastMiss, nLastMissOut, nCores, displayProgress));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_msde_hestMakeModel", (DL_FUNC) &_msde_hestMakeModel, 0},
    {"_msde_biouMakeModel", (DL_FUNC) &_msde_biouMakeModel, 0},
    {"_msde_pgnetMakeModel", (DL_FUNC) &_msde_pgnetMakeModel, 0},
    {"_msde_lotvolMakeModel", (DL_FUNC) &_msde_lotvolMakeModel, 0},
    {"_msde_eouMakeModel", (DL_FUNC) &_msde_eouMakeModel, 0},
    {"_msde_sde_nParams", (DL_FUNC) &_msde_sde_nParams, 1},
    {"_msde_sde_nDims", (DL_FUNC) &_msde_sde_nDims, 1},
    {"_msde_isData", (DL_FUNC) &_msde_isData, 6},
    {"_msde_isParams", (DL_FUNC) &_msde_isParams, 3},
    {"_msde_Drift", (DL_FUNC) &_msde_Drift, 6},
    {"_msde_Diff", (DL_FUNC) &_msde_Diff, 6},
    {"_msde_LogLik", (DL_FUNC) &_msde_LogLik, 9},
    {"_msde_Prior", (DL_FUNC) &_msde_Prior, 7},
    {"_msde_Sim", (DL_FUNC) &_msde_Sim, 12},
    {"_msde_Post", (DL_FUNC) &_msde_Post, 23},
    {NULL, NULL, 0}
};

RcppExport void R_init_msde(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
