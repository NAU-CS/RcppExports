// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// parse_names
DataFrame parse_names(CharacterVector names);
RcppExport SEXP humaniformat_parse_names(SEXP namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type names(namesSEXP);
    __result = Rcpp::wrap(parse_names(names));
    return __result;
END_RCPP
}
// format_reverse
CharacterVector format_reverse(CharacterVector names);
RcppExport SEXP humaniformat_format_reverse(SEXP namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type names(namesSEXP);
    __result = Rcpp::wrap(format_reverse(names));
    return __result;
END_RCPP
}
// format_period
CharacterVector format_period(CharacterVector names);
RcppExport SEXP humaniformat_format_period(SEXP namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type names(namesSEXP);
    __result = Rcpp::wrap(format_period(names));
    return __result;
END_RCPP
}
// get_
CharacterVector get_(CharacterVector names, int element);
RcppExport SEXP humaniformat_get_(SEXP namesSEXP, SEXP elementSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type names(namesSEXP);
    Rcpp::traits::input_parameter< int >::type element(elementSEXP);
    __result = Rcpp::wrap(get_(names, element));
    return __result;
END_RCPP
}
// set_
CharacterVector set_(CharacterVector names, int element, String replacement);
RcppExport SEXP humaniformat_set_(SEXP namesSEXP, SEXP elementSEXP, SEXP replacementSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type names(namesSEXP);
    Rcpp::traits::input_parameter< int >::type element(elementSEXP);
    Rcpp::traits::input_parameter< String >::type replacement(replacementSEXP);
    __result = Rcpp::wrap(set_(names, element, replacement));
    return __result;
END_RCPP
}