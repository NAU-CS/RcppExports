// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// compute_indiscernibility
List compute_indiscernibility(List input, CharacterVector attr_val, CharacterVector unique_attr_val);
RcppExport SEXP _RoughSets_compute_indiscernibility(SEXP inputSEXP, SEXP attr_valSEXP, SEXP unique_attr_valSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type attr_val(attr_valSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type unique_attr_val(unique_attr_valSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_indiscernibility(input, attr_val, unique_attr_val));
    return rcpp_result_gen;
END_RCPP
}
// compute_chaos
List compute_chaos(List input, CharacterVector dec_val, CharacterVector unique_dec_val);
RcppExport SEXP _RoughSets_compute_chaos(SEXP inputSEXP, SEXP dec_valSEXP, SEXP unique_dec_valSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dec_val(dec_valSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type unique_dec_val(unique_dec_valSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_chaos(input, dec_val, unique_dec_val));
    return rcpp_result_gen;
END_RCPP
}
