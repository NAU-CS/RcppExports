// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// estep
NumericMatrix estep(NumericMatrix A, NumericVector L);
RcppExport SEXP _acc_estep(SEXP ASEXP, SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type L(LSEXP);
    rcpp_result_gen = Rcpp::wrap(estep(A, L));
    return rcpp_result_gen;
END_RCPP
}
// estepAEE
NumericMatrix estepAEE(NumericMatrix A, NumericVector L, double a);
RcppExport SEXP _acc_estepAEE(SEXP ASEXP, SEXP LSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type L(LSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(estepAEE(A, L, a));
    return rcpp_result_gen;
END_RCPP
}
// makePanelMatrix
NumericMatrix makePanelMatrix(NumericVector T, NumericVector M, NumericVector UID, NumericVector TGD, NumericVector C);
RcppExport SEXP _acc_makePanelMatrix(SEXP TSEXP, SEXP MSEXP, SEXP UIDSEXP, SEXP TGDSEXP, SEXP CSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type T(TSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type M(MSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type UID(UIDSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type TGD(TGDSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type C(CSEXP);
    rcpp_result_gen = Rcpp::wrap(makePanelMatrix(T, M, UID, TGD, C));
    return rcpp_result_gen;
END_RCPP
}
// matrixInverse
arma::mat matrixInverse(arma::mat S);
RcppExport SEXP _acc_matrixInverse(SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type S(SSEXP);
    rcpp_result_gen = Rcpp::wrap(matrixInverse(S));
    return rcpp_result_gen;
END_RCPP
}
// rollSumCpp
NumericVector rollSumCpp(NumericVector x, int n);
RcppExport SEXP _acc_rollSumCpp(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rollSumCpp(x, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_acc_estep", (DL_FUNC) &_acc_estep, 2},
    {"_acc_estepAEE", (DL_FUNC) &_acc_estepAEE, 3},
    {"_acc_makePanelMatrix", (DL_FUNC) &_acc_makePanelMatrix, 5},
    {"_acc_matrixInverse", (DL_FUNC) &_acc_matrixInverse, 1},
    {"_acc_rollSumCpp", (DL_FUNC) &_acc_rollSumCpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_acc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}