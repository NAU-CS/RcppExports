// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// qrencode_raw
NumericMatrix qrencode_raw(std::string to_encode, int version, int level, int hint, int caseinsensitive);
RcppExport SEXP _qrencoder_qrencode_raw(SEXP to_encodeSEXP, SEXP versionSEXP, SEXP levelSEXP, SEXP hintSEXP, SEXP caseinsensitiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type to_encode(to_encodeSEXP);
    Rcpp::traits::input_parameter< int >::type version(versionSEXP);
    Rcpp::traits::input_parameter< int >::type level(levelSEXP);
    Rcpp::traits::input_parameter< int >::type hint(hintSEXP);
    Rcpp::traits::input_parameter< int >::type caseinsensitive(caseinsensitiveSEXP);
    rcpp_result_gen = Rcpp::wrap(qrencode_raw(to_encode, version, level, hint, caseinsensitive));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_qrencoder_qrencode_raw", (DL_FUNC) &_qrencoder_qrencode_raw, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_qrencoder(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}