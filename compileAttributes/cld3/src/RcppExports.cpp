// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cld3_detect_language
Rcpp::CharacterVector cld3_detect_language(std::vector<std::string> texts);
RcppExport SEXP _cld3_cld3_detect_language(SEXP textsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type texts(textsSEXP);
    rcpp_result_gen = Rcpp::wrap(cld3_detect_language(texts));
    return rcpp_result_gen;
END_RCPP
}
// cld3_detect_language_df
Rcpp::DataFrame cld3_detect_language_df(std::vector<std::string> texts);
RcppExport SEXP _cld3_cld3_detect_language_df(SEXP textsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type texts(textsSEXP);
    rcpp_result_gen = Rcpp::wrap(cld3_detect_language_df(texts));
    return rcpp_result_gen;
END_RCPP
}
// cld3_detect_language_mixed
Rcpp::DataFrame cld3_detect_language_mixed(std::string text, size_t len);
RcppExport SEXP _cld3_cld3_detect_language_mixed(SEXP textSEXP, SEXP lenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type text(textSEXP);
    Rcpp::traits::input_parameter< size_t >::type len(lenSEXP);
    rcpp_result_gen = Rcpp::wrap(cld3_detect_language_mixed(text, len));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cld3_cld3_detect_language", (DL_FUNC) &_cld3_cld3_detect_language, 1},
    {"_cld3_cld3_detect_language_df", (DL_FUNC) &_cld3_cld3_detect_language_df, 1},
    {"_cld3_cld3_detect_language_mixed", (DL_FUNC) &_cld3_cld3_detect_language_mixed, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_cld3(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
