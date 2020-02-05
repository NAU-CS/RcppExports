// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// HARDataCreationC
arma::mat HARDataCreationC(arma::vec vRealizedMeasure, arma::vec vPeriods, int h);
RcppExport SEXP _HARModel_HARDataCreationC(SEXP vRealizedMeasureSEXP, SEXP vPeriodsSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type vRealizedMeasure(vRealizedMeasureSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type vPeriods(vPeriodsSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(HARDataCreationC(vRealizedMeasure, vPeriods, h));
    return rcpp_result_gen;
END_RCPP
}
// HARMatCombine
arma::mat HARMatCombine(arma::mat mA, arma::mat mB);
RcppExport SEXP _HARModel_HARMatCombine(SEXP mASEXP, SEXP mBSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type mA(mASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mB(mBSEXP);
    rcpp_result_gen = Rcpp::wrap(HARMatCombine(mA, mB));
    return rcpp_result_gen;
END_RCPP
}
// HARSimC
arma::mat HARSimC(int iLength, arma::vec vLags, double dConst, arma::vec coef, double dSigma);
RcppExport SEXP _HARModel_HARSimC(SEXP iLengthSEXP, SEXP vLagsSEXP, SEXP dConstSEXP, SEXP coefSEXP, SEXP dSigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type iLength(iLengthSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type vLags(vLagsSEXP);
    Rcpp::traits::input_parameter< double >::type dConst(dConstSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type coef(coefSEXP);
    Rcpp::traits::input_parameter< double >::type dSigma(dSigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(HARSimC(iLength, vLags, dConst, coef, dSigma));
    return rcpp_result_gen;
END_RCPP
}
// fastLMcoef
arma::vec fastLMcoef(arma::mat X, arma::colvec y);
RcppExport SEXP _HARModel_fastLMcoef(SEXP XSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(fastLMcoef(X, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_HARModel_HARDataCreationC", (DL_FUNC) &_HARModel_HARDataCreationC, 3},
    {"_HARModel_HARMatCombine", (DL_FUNC) &_HARModel_HARMatCombine, 2},
    {"_HARModel_HARSimC", (DL_FUNC) &_HARModel_HARSimC, 5},
    {"_HARModel_fastLMcoef", (DL_FUNC) &_HARModel_fastLMcoef, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_HARModel(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}