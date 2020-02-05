// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// lbNumeric
int lbNumeric(Rcpp::NumericVector sortedValues, double valueToSearch);
RcppExport SEXP _bsearchtools_lbNumeric(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< double >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(lbNumeric(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// ubNumeric
int ubNumeric(Rcpp::NumericVector sortedValues, double valueToSearch);
RcppExport SEXP _bsearchtools_ubNumeric(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< double >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(ubNumeric(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// indexesInRangeNumeric
Rcpp::IntegerVector indexesInRangeNumeric(Rcpp::NumericVector sortedValues, double lbInclusive, double ubInclusive, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesInRangeNumeric(SEXP sortedValuesSEXP, SEXP lbInclusiveSEXP, SEXP ubInclusiveSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< double >::type lbInclusive(lbInclusiveSEXP);
    Rcpp::traits::input_parameter< double >::type ubInclusive(ubInclusiveSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesInRangeNumeric(sortedValues, lbInclusive, ubInclusive, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// indexesEqualToNumeric
Rcpp::IntegerVector indexesEqualToNumeric(Rcpp::NumericVector sortedValues, double valueToSearch, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesEqualToNumeric(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< double >::type valueToSearch(valueToSearchSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesEqualToNumeric(sortedValues, valueToSearch, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// lbInteger
int lbInteger(Rcpp::IntegerVector sortedValues, int valueToSearch);
RcppExport SEXP _bsearchtools_lbInteger(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(lbInteger(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// ubInteger
int ubInteger(Rcpp::IntegerVector sortedValues, int valueToSearch);
RcppExport SEXP _bsearchtools_ubInteger(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(ubInteger(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// indexesInRangeInteger
Rcpp::IntegerVector indexesInRangeInteger(Rcpp::IntegerVector sortedValues, int lbInclusive, int ubInclusive, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesInRangeInteger(SEXP sortedValuesSEXP, SEXP lbInclusiveSEXP, SEXP ubInclusiveSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type lbInclusive(lbInclusiveSEXP);
    Rcpp::traits::input_parameter< int >::type ubInclusive(ubInclusiveSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesInRangeInteger(sortedValues, lbInclusive, ubInclusive, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// indexesEqualToInteger
Rcpp::IntegerVector indexesEqualToInteger(Rcpp::IntegerVector sortedValues, int valueToSearch, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesEqualToInteger(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type valueToSearch(valueToSearchSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesEqualToInteger(sortedValues, valueToSearch, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// lbCharacter
int lbCharacter(Rcpp::CharacterVector sortedValues, Rcpp::CharacterVector valueToSearch);
RcppExport SEXP _bsearchtools_lbCharacter(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(lbCharacter(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// ubCharacter
int ubCharacter(Rcpp::CharacterVector sortedValues, Rcpp::CharacterVector valueToSearch);
RcppExport SEXP _bsearchtools_ubCharacter(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(ubCharacter(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// indexesInRangeCharacter
Rcpp::IntegerVector indexesInRangeCharacter(Rcpp::CharacterVector sortedValues, Rcpp::CharacterVector lbInclusive, Rcpp::CharacterVector ubInclusive, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesInRangeCharacter(SEXP sortedValuesSEXP, SEXP lbInclusiveSEXP, SEXP ubInclusiveSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type lbInclusive(lbInclusiveSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type ubInclusive(ubInclusiveSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesInRangeCharacter(sortedValues, lbInclusive, ubInclusive, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// indexesEqualToCharacter
Rcpp::IntegerVector indexesEqualToCharacter(Rcpp::CharacterVector sortedValues, Rcpp::CharacterVector valueToSearch, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesEqualToCharacter(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type valueToSearch(valueToSearchSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesEqualToCharacter(sortedValues, valueToSearch, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// lbLogical
int lbLogical(Rcpp::LogicalVector sortedValues, int valueToSearch);
RcppExport SEXP _bsearchtools_lbLogical(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(lbLogical(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// ubLogical
int ubLogical(Rcpp::LogicalVector sortedValues, int valueToSearch);
RcppExport SEXP _bsearchtools_ubLogical(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(ubLogical(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// indexesInRangeLogical
Rcpp::IntegerVector indexesInRangeLogical(Rcpp::LogicalVector sortedValues, int lbInclusive, int ubInclusive, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesInRangeLogical(SEXP sortedValuesSEXP, SEXP lbInclusiveSEXP, SEXP ubInclusiveSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type lbInclusive(lbInclusiveSEXP);
    Rcpp::traits::input_parameter< int >::type ubInclusive(ubInclusiveSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesInRangeLogical(sortedValues, lbInclusive, ubInclusive, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// indexesEqualToLogical
Rcpp::IntegerVector indexesEqualToLogical(Rcpp::LogicalVector sortedValues, int valueToSearch, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesEqualToLogical(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< int >::type valueToSearch(valueToSearchSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesEqualToLogical(sortedValues, valueToSearch, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// lb
int lb(SEXP sortedValues, SEXP valueToSearch);
RcppExport SEXP _bsearchtools_lb(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(lb(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// ub
int ub(SEXP sortedValues, SEXP valueToSearch);
RcppExport SEXP _bsearchtools_ub(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type valueToSearch(valueToSearchSEXP);
    rcpp_result_gen = Rcpp::wrap(ub(sortedValues, valueToSearch));
    return rcpp_result_gen;
END_RCPP
}
// indexesInRange
Rcpp::IntegerVector indexesInRange(SEXP sortedValues, SEXP lbInclusive, SEXP ubInclusive, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesInRange(SEXP sortedValuesSEXP, SEXP lbInclusiveSEXP, SEXP ubInclusiveSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type lbInclusive(lbInclusiveSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ubInclusive(ubInclusiveSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesInRange(sortedValues, lbInclusive, ubInclusive, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// indexesEqualTo
Rcpp::IntegerVector indexesEqualTo(SEXP sortedValues, SEXP valueToSearch, SEXP indexesRemap);
RcppExport SEXP _bsearchtools_indexesEqualTo(SEXP sortedValuesSEXP, SEXP valueToSearchSEXP, SEXP indexesRemapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type sortedValues(sortedValuesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type valueToSearch(valueToSearchSEXP);
    Rcpp::traits::input_parameter< SEXP >::type indexesRemap(indexesRemapSEXP);
    rcpp_result_gen = Rcpp::wrap(indexesEqualTo(sortedValues, valueToSearch, indexesRemap));
    return rcpp_result_gen;
END_RCPP
}
// intersectInteger
Rcpp::IntegerVector intersectInteger(Rcpp::IntegerVector x, Rcpp::IntegerVector y);
RcppExport SEXP _bsearchtools_intersectInteger(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(intersectInteger(x, y));
    return rcpp_result_gen;
END_RCPP
}
// unionInteger
Rcpp::IntegerVector unionInteger(Rcpp::IntegerVector vec1, Rcpp::IntegerVector vec2);
RcppExport SEXP _bsearchtools_unionInteger(SEXP vec1SEXP, SEXP vec2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vec1(vec1SEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vec2(vec2SEXP);
    rcpp_result_gen = Rcpp::wrap(unionInteger(vec1, vec2));
    return rcpp_result_gen;
END_RCPP
}
// unionIntegerList
Rcpp::IntegerVector unionIntegerList(Rcpp::List list);
RcppExport SEXP _bsearchtools_unionIntegerList(SEXP listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type list(listSEXP);
    rcpp_result_gen = Rcpp::wrap(unionIntegerList(list));
    return rcpp_result_gen;
END_RCPP
}
