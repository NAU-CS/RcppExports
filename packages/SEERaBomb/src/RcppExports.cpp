// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// fillPYM
NumericMatrix fillPYM(SEXP PYin, SEXP PYM);
RcppExport SEXP _SEERaBomb_fillPYM(SEXP PYinSEXP, SEXP PYMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type PYin(PYinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type PYM(PYMSEXP);
    rcpp_result_gen = Rcpp::wrap(fillPYM(PYin, PYM));
    return rcpp_result_gen;
END_RCPP
}