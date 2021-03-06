// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "semver_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// parse_ptr
List parse_ptr(std::vector< std::string > versions);
RcppExport SEXP semver_parse_ptr(SEXP versionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::string > >::type versions(versionsSEXP);
    rcpp_result_gen = Rcpp::wrap(parse_ptr(versions));
    return rcpp_result_gen;
END_RCPP
}
// render_ptr
Rcpp::List render_ptr(XPtrsver200 verPtr);
RcppExport SEXP semver_render_ptr(SEXP verPtrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtrsver200 >::type verPtr(verPtrSEXP);
    rcpp_result_gen = Rcpp::wrap(render_ptr(verPtr));
    return rcpp_result_gen;
END_RCPP
}
// ptr_comparator
int ptr_comparator(XPtrsver200 verPtr1, XPtrsver200 verPtr2);
RcppExport SEXP semver_ptr_comparator(SEXP verPtr1SEXP, SEXP verPtr2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtrsver200 >::type verPtr1(verPtr1SEXP);
    Rcpp::traits::input_parameter< XPtrsver200 >::type verPtr2(verPtr2SEXP);
    rcpp_result_gen = Rcpp::wrap(ptr_comparator(verPtr1, verPtr2));
    return rcpp_result_gen;
END_RCPP
}
// set_ptr
SEXP set_ptr(XPtrsver200 verPtr, int cint, SEXP newvalue);
RcppExport SEXP semver_set_ptr(SEXP verPtrSEXP, SEXP cintSEXP, SEXP newvalueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtrsver200 >::type verPtr(verPtrSEXP);
    Rcpp::traits::input_parameter< int >::type cint(cintSEXP);
    Rcpp::traits::input_parameter< SEXP >::type newvalue(newvalueSEXP);
    rcpp_result_gen = Rcpp::wrap(set_ptr(verPtr, cint, newvalue));
    return rcpp_result_gen;
END_RCPP
}
// reset_ptr
SEXP reset_ptr(XPtrsver200 verPtr, int cint, SEXP newvalue);
RcppExport SEXP semver_reset_ptr(SEXP verPtrSEXP, SEXP cintSEXP, SEXP newvalueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtrsver200 >::type verPtr(verPtrSEXP);
    Rcpp::traits::input_parameter< int >::type cint(cintSEXP);
    Rcpp::traits::input_parameter< SEXP >::type newvalue(newvalueSEXP);
    rcpp_result_gen = Rcpp::wrap(reset_ptr(verPtr, cint, newvalue));
    return rcpp_result_gen;
END_RCPP
}
// increment_ptr
SEXP increment_ptr(XPtrsver200 verPtr, int cint, SEXP increment);
RcppExport SEXP semver_increment_ptr(SEXP verPtrSEXP, SEXP cintSEXP, SEXP incrementSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtrsver200 >::type verPtr(verPtrSEXP);
    Rcpp::traits::input_parameter< int >::type cint(cintSEXP);
    Rcpp::traits::input_parameter< SEXP >::type increment(incrementSEXP);
    rcpp_result_gen = Rcpp::wrap(increment_ptr(verPtr, cint, increment));
    return rcpp_result_gen;
END_RCPP
}
