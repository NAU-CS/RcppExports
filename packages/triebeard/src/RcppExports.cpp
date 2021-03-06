// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// add_trie_string
void add_trie_string(SEXP trie, CharacterVector keys, CharacterVector values);
RcppExport SEXP triebeard_add_trie_string(SEXP trieSEXP, SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type values(valuesSEXP);
    add_trie_string(trie, keys, values);
    return R_NilValue;
END_RCPP
}
// add_trie_integer
void add_trie_integer(SEXP trie, CharacterVector keys, IntegerVector values);
RcppExport SEXP triebeard_add_trie_integer(SEXP trieSEXP, SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type values(valuesSEXP);
    add_trie_integer(trie, keys, values);
    return R_NilValue;
END_RCPP
}
// add_trie_numeric
void add_trie_numeric(SEXP trie, CharacterVector keys, NumericVector values);
RcppExport SEXP triebeard_add_trie_numeric(SEXP trieSEXP, SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type values(valuesSEXP);
    add_trie_numeric(trie, keys, values);
    return R_NilValue;
END_RCPP
}
// add_trie_logical
void add_trie_logical(SEXP trie, CharacterVector keys, LogicalVector values);
RcppExport SEXP triebeard_add_trie_logical(SEXP trieSEXP, SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type values(valuesSEXP);
    add_trie_logical(trie, keys, values);
    return R_NilValue;
END_RCPP
}
// remove_trie_string
void remove_trie_string(SEXP trie, CharacterVector keys);
RcppExport SEXP triebeard_remove_trie_string(SEXP trieSEXP, SEXP keysSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    remove_trie_string(trie, keys);
    return R_NilValue;
END_RCPP
}
// remove_trie_integer
void remove_trie_integer(SEXP trie, CharacterVector keys);
RcppExport SEXP triebeard_remove_trie_integer(SEXP trieSEXP, SEXP keysSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    remove_trie_integer(trie, keys);
    return R_NilValue;
END_RCPP
}
// remove_trie_numeric
void remove_trie_numeric(SEXP trie, CharacterVector keys);
RcppExport SEXP triebeard_remove_trie_numeric(SEXP trieSEXP, SEXP keysSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    remove_trie_numeric(trie, keys);
    return R_NilValue;
END_RCPP
}
// remove_trie_logical
void remove_trie_logical(SEXP trie, CharacterVector keys);
RcppExport SEXP triebeard_remove_trie_logical(SEXP trieSEXP, SEXP keysSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type trie(trieSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type keys(keysSEXP);
    remove_trie_logical(trie, keys);
    return R_NilValue;
END_RCPP
}
// radix_create_string
SEXP radix_create_string(std::vector < std::string > keys, std::vector < std::string > values);
RcppExport SEXP triebeard_radix_create_string(SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector < std::string > >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< std::vector < std::string > >::type values(valuesSEXP);
    __result = Rcpp::wrap(radix_create_string(keys, values));
    return __result;
END_RCPP
}
// radix_create_integer
SEXP radix_create_integer(std::vector < std::string > keys, std::vector < int > values);
RcppExport SEXP triebeard_radix_create_integer(SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector < std::string > >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< std::vector < int > >::type values(valuesSEXP);
    __result = Rcpp::wrap(radix_create_integer(keys, values));
    return __result;
END_RCPP
}
// radix_create_numeric
SEXP radix_create_numeric(std::vector < std::string > keys, std::vector < double > values);
RcppExport SEXP triebeard_radix_create_numeric(SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector < std::string > >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< std::vector < double > >::type values(valuesSEXP);
    __result = Rcpp::wrap(radix_create_numeric(keys, values));
    return __result;
END_RCPP
}
// radix_create_logical
SEXP radix_create_logical(std::vector < std::string > keys, std::vector < bool > values);
RcppExport SEXP triebeard_radix_create_logical(SEXP keysSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector < std::string > >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< std::vector < bool > >::type values(valuesSEXP);
    __result = Rcpp::wrap(radix_create_logical(keys, values));
    return __result;
END_RCPP
}
// get_keys_string
std::vector < std::string > get_keys_string(SEXP radix);
RcppExport SEXP triebeard_get_keys_string(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_keys_string(radix));
    return __result;
END_RCPP
}
// get_keys_integer
std::vector < std::string > get_keys_integer(SEXP radix);
RcppExport SEXP triebeard_get_keys_integer(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_keys_integer(radix));
    return __result;
END_RCPP
}
// get_keys_numeric
std::vector < std::string > get_keys_numeric(SEXP radix);
RcppExport SEXP triebeard_get_keys_numeric(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_keys_numeric(radix));
    return __result;
END_RCPP
}
// get_keys_logical
std::vector < std::string > get_keys_logical(SEXP radix);
RcppExport SEXP triebeard_get_keys_logical(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_keys_logical(radix));
    return __result;
END_RCPP
}
// get_values_string
std::vector < std::string > get_values_string(SEXP radix);
RcppExport SEXP triebeard_get_values_string(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_values_string(radix));
    return __result;
END_RCPP
}
// get_values_integer
std::vector < int > get_values_integer(SEXP radix);
RcppExport SEXP triebeard_get_values_integer(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_values_integer(radix));
    return __result;
END_RCPP
}
// get_values_numeric
std::vector < double > get_values_numeric(SEXP radix);
RcppExport SEXP triebeard_get_values_numeric(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_values_numeric(radix));
    return __result;
END_RCPP
}
// get_values_logical
std::vector < bool > get_values_logical(SEXP radix);
RcppExport SEXP triebeard_get_values_logical(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(get_values_logical(radix));
    return __result;
END_RCPP
}
// greedy_string
List greedy_string(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_greedy_string(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(greedy_string(radix, to_match));
    return __result;
END_RCPP
}
// greedy_integer
List greedy_integer(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_greedy_integer(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(greedy_integer(radix, to_match));
    return __result;
END_RCPP
}
// greedy_numeric
List greedy_numeric(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_greedy_numeric(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(greedy_numeric(radix, to_match));
    return __result;
END_RCPP
}
// greedy_logical
List greedy_logical(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_greedy_logical(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(greedy_logical(radix, to_match));
    return __result;
END_RCPP
}
// radix_len_string
int radix_len_string(SEXP radix);
RcppExport SEXP triebeard_radix_len_string(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(radix_len_string(radix));
    return __result;
END_RCPP
}
// radix_len_integer
int radix_len_integer(SEXP radix);
RcppExport SEXP triebeard_radix_len_integer(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(radix_len_integer(radix));
    return __result;
END_RCPP
}
// radix_len_numeric
int radix_len_numeric(SEXP radix);
RcppExport SEXP triebeard_radix_len_numeric(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(radix_len_numeric(radix));
    return __result;
END_RCPP
}
// radix_len_logical
int radix_len_logical(SEXP radix);
RcppExport SEXP triebeard_radix_len_logical(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    __result = Rcpp::wrap(radix_len_logical(radix));
    return __result;
END_RCPP
}
// longest_string
CharacterVector longest_string(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_longest_string(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(longest_string(radix, to_match));
    return __result;
END_RCPP
}
// longest_integer
IntegerVector longest_integer(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_longest_integer(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(longest_integer(radix, to_match));
    return __result;
END_RCPP
}
// longest_numeric
NumericVector longest_numeric(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_longest_numeric(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(longest_numeric(radix, to_match));
    return __result;
END_RCPP
}
// longest_logical
LogicalVector longest_logical(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_longest_logical(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(longest_logical(radix, to_match));
    return __result;
END_RCPP
}
// prefix_string
List prefix_string(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_prefix_string(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(prefix_string(radix, to_match));
    return __result;
END_RCPP
}
// prefix_integer
List prefix_integer(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_prefix_integer(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(prefix_integer(radix, to_match));
    return __result;
END_RCPP
}
// prefix_numeric
List prefix_numeric(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_prefix_numeric(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(prefix_numeric(radix, to_match));
    return __result;
END_RCPP
}
// prefix_logical
List prefix_logical(SEXP radix, CharacterVector to_match);
RcppExport SEXP triebeard_prefix_logical(SEXP radixSEXP, SEXP to_matchSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type to_match(to_matchSEXP);
    __result = Rcpp::wrap(prefix_logical(radix, to_match));
    return __result;
END_RCPP
}
// trie_str_string
void trie_str_string(SEXP radix);
RcppExport SEXP triebeard_trie_str_string(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    trie_str_string(radix);
    return R_NilValue;
END_RCPP
}
// trie_str_integer
void trie_str_integer(SEXP radix);
RcppExport SEXP triebeard_trie_str_integer(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    trie_str_integer(radix);
    return R_NilValue;
END_RCPP
}
// trie_str_numeric
void trie_str_numeric(SEXP radix);
RcppExport SEXP triebeard_trie_str_numeric(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    trie_str_numeric(radix);
    return R_NilValue;
END_RCPP
}
// trie_str_logical
void trie_str_logical(SEXP radix);
RcppExport SEXP triebeard_trie_str_logical(SEXP radixSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type radix(radixSEXP);
    trie_str_logical(radix);
    return R_NilValue;
END_RCPP
}
