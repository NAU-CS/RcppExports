// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// main_fastcmh2
Rcpp::List main_fastcmh2(Rcpp::String xfilenameR, Rcpp::String yfilenameR, Rcpp::String cfilenameR, Rcpp::NumericVector alphaR, Rcpp::NumericVector lmaxR, Rcpp::LogicalVector showProcessingR, Rcpp::LogicalVector saveAllPvalsR, Rcpp::LogicalVector doFDR_R, Rcpp::LogicalVector useDependenceFDR_R);
RcppExport SEXP _fastcmh_main_fastcmh2(SEXP xfilenameRSEXP, SEXP yfilenameRSEXP, SEXP cfilenameRSEXP, SEXP alphaRSEXP, SEXP lmaxRSEXP, SEXP showProcessingRSEXP, SEXP saveAllPvalsRSEXP, SEXP doFDR_RSEXP, SEXP useDependenceFDR_RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type xfilenameR(xfilenameRSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type yfilenameR(yfilenameRSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type cfilenameR(cfilenameRSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type alphaR(alphaRSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type lmaxR(lmaxRSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type showProcessingR(showProcessingRSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type saveAllPvalsR(saveAllPvalsRSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type doFDR_R(doFDR_RSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type useDependenceFDR_R(useDependenceFDR_RSEXP);
    rcpp_result_gen = Rcpp::wrap(main_fastcmh2(xfilenameR, yfilenameR, cfilenameR, alphaR, lmaxR, showProcessingR, saveAllPvalsR, doFDR_R, useDependenceFDR_R));
    return rcpp_result_gen;
END_RCPP
}
// cpp_test_filtering
Rcpp::DataFrame cpp_test_filtering(const Rcpp::DataFrame& dfInput);
RcppExport SEXP _fastcmh_cpp_test_filtering(SEXP dfInputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::DataFrame& >::type dfInput(dfInputSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_test_filtering(dfInput));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fastcmh_main_fastcmh2", (DL_FUNC) &_fastcmh_main_fastcmh2, 9},
    {"_fastcmh_cpp_test_filtering", (DL_FUNC) &_fastcmh_cpp_test_filtering, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastcmh(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
