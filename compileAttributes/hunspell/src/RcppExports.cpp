// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "hunspell_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// R_hunspell_dict
DictPtr R_hunspell_dict(Rcpp::String affix, CharacterVector dict, StringVector add_words);
RcppExport SEXP _hunspell_R_hunspell_dict(SEXP affixSEXP, SEXP dictSEXP, SEXP add_wordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type affix(affixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dict(dictSEXP);
    Rcpp::traits::input_parameter< StringVector >::type add_words(add_wordsSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_dict(affix, dict, add_words));
    return rcpp_result_gen;
END_RCPP
}
// R_hunspell_info
List R_hunspell_info(DictPtr ptr);
RcppExport SEXP _hunspell_R_hunspell_info(SEXP ptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DictPtr >::type ptr(ptrSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_info(ptr));
    return rcpp_result_gen;
END_RCPP
}
// R_hunspell_check
LogicalVector R_hunspell_check(DictPtr ptr, StringVector words);
RcppExport SEXP _hunspell_R_hunspell_check(SEXP ptrSEXP, SEXP wordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DictPtr >::type ptr(ptrSEXP);
    Rcpp::traits::input_parameter< StringVector >::type words(wordsSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_check(ptr, words));
    return rcpp_result_gen;
END_RCPP
}
// R_hunspell_suggest
List R_hunspell_suggest(DictPtr ptr, StringVector words);
RcppExport SEXP _hunspell_R_hunspell_suggest(SEXP ptrSEXP, SEXP wordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DictPtr >::type ptr(ptrSEXP);
    Rcpp::traits::input_parameter< StringVector >::type words(wordsSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_suggest(ptr, words));
    return rcpp_result_gen;
END_RCPP
}
// R_hunspell_analyze
List R_hunspell_analyze(DictPtr ptr, StringVector words);
RcppExport SEXP _hunspell_R_hunspell_analyze(SEXP ptrSEXP, SEXP wordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DictPtr >::type ptr(ptrSEXP);
    Rcpp::traits::input_parameter< StringVector >::type words(wordsSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_analyze(ptr, words));
    return rcpp_result_gen;
END_RCPP
}
// R_hunspell_stem
List R_hunspell_stem(DictPtr ptr, StringVector words);
RcppExport SEXP _hunspell_R_hunspell_stem(SEXP ptrSEXP, SEXP wordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DictPtr >::type ptr(ptrSEXP);
    Rcpp::traits::input_parameter< StringVector >::type words(wordsSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_stem(ptr, words));
    return rcpp_result_gen;
END_RCPP
}
// R_hunspell_find
List R_hunspell_find(DictPtr ptr, StringVector text, std::string format);
RcppExport SEXP _hunspell_R_hunspell_find(SEXP ptrSEXP, SEXP textSEXP, SEXP formatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DictPtr >::type ptr(ptrSEXP);
    Rcpp::traits::input_parameter< StringVector >::type text(textSEXP);
    Rcpp::traits::input_parameter< std::string >::type format(formatSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_find(ptr, text, format));
    return rcpp_result_gen;
END_RCPP
}
// R_hunspell_parse
List R_hunspell_parse(DictPtr ptr, StringVector text, std::string format);
RcppExport SEXP _hunspell_R_hunspell_parse(SEXP ptrSEXP, SEXP textSEXP, SEXP formatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DictPtr >::type ptr(ptrSEXP);
    Rcpp::traits::input_parameter< StringVector >::type text(textSEXP);
    Rcpp::traits::input_parameter< std::string >::type format(formatSEXP);
    rcpp_result_gen = Rcpp::wrap(R_hunspell_parse(ptr, text, format));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_hunspell_R_hunspell_dict", (DL_FUNC) &_hunspell_R_hunspell_dict, 3},
    {"_hunspell_R_hunspell_info", (DL_FUNC) &_hunspell_R_hunspell_info, 1},
    {"_hunspell_R_hunspell_check", (DL_FUNC) &_hunspell_R_hunspell_check, 2},
    {"_hunspell_R_hunspell_suggest", (DL_FUNC) &_hunspell_R_hunspell_suggest, 2},
    {"_hunspell_R_hunspell_analyze", (DL_FUNC) &_hunspell_R_hunspell_analyze, 2},
    {"_hunspell_R_hunspell_stem", (DL_FUNC) &_hunspell_R_hunspell_stem, 2},
    {"_hunspell_R_hunspell_find", (DL_FUNC) &_hunspell_R_hunspell_find, 3},
    {"_hunspell_R_hunspell_parse", (DL_FUNC) &_hunspell_R_hunspell_parse, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_hunspell(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}