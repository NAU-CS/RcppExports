// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// order_vec
Rcpp::NumericVector order_vec(Rcpp::NumericVector& data);
RcppExport SEXP _pseudorank_order_vec(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(order_vec(data));
    return rcpp_result_gen;
END_RCPP
}
// psrankCpp
Rcpp::NumericVector psrankCpp(Rcpp::NumericVector& data, Rcpp::NumericVector& group, Rcpp::NumericVector& n);
RcppExport SEXP _pseudorank_psrankCpp(SEXP dataSEXP, SEXP groupSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type group(groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(psrankCpp(data, group, n));
    return rcpp_result_gen;
END_RCPP
}
// psrankMinCpp
Rcpp::NumericVector psrankMinCpp(Rcpp::NumericVector& data, Rcpp::NumericVector& group, Rcpp::NumericVector& n);
RcppExport SEXP _pseudorank_psrankMinCpp(SEXP dataSEXP, SEXP groupSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type group(groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(psrankMinCpp(data, group, n));
    return rcpp_result_gen;
END_RCPP
}
// psrankMaxCpp
Rcpp::NumericVector psrankMaxCpp(Rcpp::NumericVector& data, Rcpp::NumericVector& group, Rcpp::NumericVector& n);
RcppExport SEXP _pseudorank_psrankMaxCpp(SEXP dataSEXP, SEXP groupSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type group(groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(psrankMaxCpp(data, group, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_pseudorank_order_vec", (DL_FUNC) &_pseudorank_order_vec, 1},
    {"_pseudorank_psrankCpp", (DL_FUNC) &_pseudorank_psrankCpp, 3},
    {"_pseudorank_psrankMinCpp", (DL_FUNC) &_pseudorank_psrankMinCpp, 3},
    {"_pseudorank_psrankMaxCpp", (DL_FUNC) &_pseudorank_psrankMaxCpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_pseudorank(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
