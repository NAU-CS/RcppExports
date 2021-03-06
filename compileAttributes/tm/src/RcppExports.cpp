// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// tdm
List tdm(const StringVector strings, const bool remove_puncts, const bool remove_digits, const std::vector<std::string> stopwords, const std::vector<std::string> dictionary, const unsigned int min_term_freq, const unsigned int max_term_freq, const unsigned int min_word_length, const unsigned int max_word_length);
RcppExport SEXP _tm_tdm(SEXP stringsSEXP, SEXP remove_punctsSEXP, SEXP remove_digitsSEXP, SEXP stopwordsSEXP, SEXP dictionarySEXP, SEXP min_term_freqSEXP, SEXP max_term_freqSEXP, SEXP min_word_lengthSEXP, SEXP max_word_lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const StringVector >::type strings(stringsSEXP);
    Rcpp::traits::input_parameter< const bool >::type remove_puncts(remove_punctsSEXP);
    Rcpp::traits::input_parameter< const bool >::type remove_digits(remove_digitsSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type stopwords(stopwordsSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type dictionary(dictionarySEXP);
    Rcpp::traits::input_parameter< const unsigned int >::type min_term_freq(min_term_freqSEXP);
    Rcpp::traits::input_parameter< const unsigned int >::type max_term_freq(max_term_freqSEXP);
    Rcpp::traits::input_parameter< const unsigned int >::type min_word_length(min_word_lengthSEXP);
    Rcpp::traits::input_parameter< const unsigned int >::type max_word_length(max_word_lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(tdm(strings, remove_puncts, remove_digits, stopwords, dictionary, min_term_freq, max_term_freq, min_word_length, max_word_length));
    return rcpp_result_gen;
END_RCPP
}
// Boost_Tokenizer
StringVector Boost_Tokenizer(const StringVector strings);
RcppExport SEXP _tm_Boost_Tokenizer(SEXP stringsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const StringVector >::type strings(stringsSEXP);
    rcpp_result_gen = Rcpp::wrap(Boost_Tokenizer(strings));
    return rcpp_result_gen;
END_RCPP
}
