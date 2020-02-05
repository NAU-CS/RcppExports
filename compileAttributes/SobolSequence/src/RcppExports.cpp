// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcppSobolPoints
NumericMatrix rcppSobolPoints(std::string filename, int dimR, int dimF2, int count, NumericVector shiftVector);
RcppExport SEXP _SobolSequence_rcppSobolPoints(SEXP filenameSEXP, SEXP dimRSEXP, SEXP dimF2SEXP, SEXP countSEXP, SEXP shiftVectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type dimR(dimRSEXP);
    Rcpp::traits::input_parameter< int >::type dimF2(dimF2SEXP);
    Rcpp::traits::input_parameter< int >::type count(countSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type shiftVector(shiftVectorSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppSobolPoints(filename, dimR, dimF2, count, shiftVector));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SobolSequence_rcppSobolPoints", (DL_FUNC) &_SobolSequence_rcppSobolPoints, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_SobolSequence(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}