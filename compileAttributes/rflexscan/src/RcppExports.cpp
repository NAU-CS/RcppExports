// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// runFleXScan
List runFleXScan(const List& setting, const NumericMatrix& case_mat, const NumericMatrix& coord_mat, const NumericMatrix& adj_mat);
RcppExport SEXP _rflexscan_runFleXScan(SEXP settingSEXP, SEXP case_matSEXP, SEXP coord_matSEXP, SEXP adj_matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type setting(settingSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type case_mat(case_matSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type coord_mat(coord_matSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type adj_mat(adj_matSEXP);
    rcpp_result_gen = Rcpp::wrap(runFleXScan(setting, case_mat, coord_mat, adj_mat));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rflexscan_runFleXScan", (DL_FUNC) &_rflexscan_runFleXScan, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_rflexscan(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
