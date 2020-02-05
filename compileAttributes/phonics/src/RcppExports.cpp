// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// metaphone_internal
Rcpp::CharacterVector metaphone_internal(Rcpp::CharacterVector word, int maxCodeLen);
RcppExport SEXP _phonics_metaphone_internal(SEXP wordSEXP, SEXP maxCodeLenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type word(wordSEXP);
    Rcpp::traits::input_parameter< int >::type maxCodeLen(maxCodeLenSEXP);
    rcpp_result_gen = Rcpp::wrap(metaphone_internal(word, maxCodeLen));
    return rcpp_result_gen;
END_RCPP
}
// soundex_internal
Rcpp::CharacterVector soundex_internal(Rcpp::CharacterVector word, int maxCodeLen);
RcppExport SEXP _phonics_soundex_internal(SEXP wordSEXP, SEXP maxCodeLenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type word(wordSEXP);
    Rcpp::traits::input_parameter< int >::type maxCodeLen(maxCodeLenSEXP);
    rcpp_result_gen = Rcpp::wrap(soundex_internal(word, maxCodeLen));
    return rcpp_result_gen;
END_RCPP
}
// refinedSoundex_internal
Rcpp::CharacterVector refinedSoundex_internal(Rcpp::CharacterVector word, int maxCodeLen);
RcppExport SEXP _phonics_refinedSoundex_internal(SEXP wordSEXP, SEXP maxCodeLenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type word(wordSEXP);
    Rcpp::traits::input_parameter< int >::type maxCodeLen(maxCodeLenSEXP);
    rcpp_result_gen = Rcpp::wrap(refinedSoundex_internal(word, maxCodeLen));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_phonics_metaphone_internal", (DL_FUNC) &_phonics_metaphone_internal, 2},
    {"_phonics_soundex_internal", (DL_FUNC) &_phonics_soundex_internal, 2},
    {"_phonics_refinedSoundex_internal", (DL_FUNC) &_phonics_refinedSoundex_internal, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_phonics(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}