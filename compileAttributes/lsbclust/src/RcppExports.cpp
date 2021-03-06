// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// ClustMeans
NumericMatrix ClustMeans(int nclust, IntegerVector start, NumericMatrix data);
RcppExport SEXP _lsbclust_ClustMeans(SEXP nclustSEXP, SEXP startSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nclust(nclustSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(ClustMeans(nclust, start, data));
    return rcpp_result_gen;
END_RCPP
}
// ComputeMeans
NumericMatrix ComputeMeans(IntegerVector cm, NumericMatrix data, NumericVector weight, int nclust);
RcppExport SEXP _lsbclust_ComputeMeans(SEXP cmSEXP, SEXP dataSEXP, SEXP weightSEXP, SEXP nclustSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type cm(cmSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< int >::type nclust(nclustSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeMeans(cm, data, weight, nclust));
    return rcpp_result_gen;
END_RCPP
}
// AssignCluster
List AssignCluster(NumericMatrix data, NumericVector weight, NumericMatrix M, int nclust);
RcppExport SEXP _lsbclust_AssignCluster(SEXP dataSEXP, SEXP weightSEXP, SEXP MSEXP, SEXP nclustSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type nclust(nclustSEXP);
    rcpp_result_gen = Rcpp::wrap(AssignCluster(data, weight, M, nclust));
    return rcpp_result_gen;
END_RCPP
}
// KMeansW
List KMeansW(int nclust, IntegerVector start, NumericMatrix data, NumericVector weight, double eps, int IterMax);
RcppExport SEXP _lsbclust_KMeansW(SEXP nclustSEXP, SEXP startSEXP, SEXP dataSEXP, SEXP weightSEXP, SEXP epsSEXP, SEXP IterMaxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nclust(nclustSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< int >::type IterMax(IterMaxSEXP);
    rcpp_result_gen = Rcpp::wrap(KMeansW(nclust, start, data, weight, eps, IterMax));
    return rcpp_result_gen;
END_RCPP
}
// LossMat
NumericMatrix LossMat(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _lsbclust_LossMat(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(LossMat(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lsbclust_ClustMeans", (DL_FUNC) &_lsbclust_ClustMeans, 3},
    {"_lsbclust_ComputeMeans", (DL_FUNC) &_lsbclust_ComputeMeans, 4},
    {"_lsbclust_AssignCluster", (DL_FUNC) &_lsbclust_AssignCluster, 4},
    {"_lsbclust_KMeansW", (DL_FUNC) &_lsbclust_KMeansW, 6},
    {"_lsbclust_LossMat", (DL_FUNC) &_lsbclust_LossMat, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_lsbclust(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
