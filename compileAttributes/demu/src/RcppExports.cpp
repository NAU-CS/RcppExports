// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// sqrt_
sp_mat sqrt_(sp_mat X);
RcppExport SEXP _demu_sqrt_(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< sp_mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(sqrt_(X));
    return rcpp_result_gen;
END_RCPP
}
// subspace_
mat subspace_(mat V);
RcppExport SEXP _demu_subspace_(SEXP VSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< mat >::type V(VSEXP);
    rcpp_result_gen = Rcpp::wrap(subspace_(V));
    return rcpp_result_gen;
END_RCPP
}
// simDppModal_
vec simDppModal_(sp_mat R, uword n);
RcppExport SEXP _demu_simDppModal_(SEXP RSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< sp_mat >::type R(RSEXP);
    Rcpp::traits::input_parameter< uword >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(simDppModal_(R, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_demu_sqrt_", (DL_FUNC) &_demu_sqrt_, 1},
    {"_demu_subspace_", (DL_FUNC) &_demu_subspace_, 1},
    {"_demu_simDppModal_", (DL_FUNC) &_demu_simDppModal_, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_demu(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
