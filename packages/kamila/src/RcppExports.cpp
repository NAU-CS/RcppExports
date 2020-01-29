// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// dptm
NumericMatrix dptm(NumericMatrix pts, NumericMatrix myMeans, NumericVector wgts, int ppDim, int kkMean, int nn);
RcppExport SEXP _kamila_dptm(SEXP ptsSEXP, SEXP myMeansSEXP, SEXP wgtsSEXP, SEXP ppDimSEXP, SEXP kkMeanSEXP, SEXP nnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type pts(ptsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type myMeans(myMeansSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wgts(wgtsSEXP);
    Rcpp::traits::input_parameter< int >::type ppDim(ppDimSEXP);
    Rcpp::traits::input_parameter< int >::type kkMean(kkMeanSEXP);
    Rcpp::traits::input_parameter< int >::type nn(nnSEXP);
    rcpp_result_gen = Rcpp::wrap(dptm(pts, myMeans, wgts, ppDim, kkMean, nn));
    return rcpp_result_gen;
END_RCPP
}
// rowMax
NumericVector rowMax(NumericMatrix inMat);
RcppExport SEXP _kamila_rowMax(SEXP inMatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type inMat(inMatSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMax(inMat));
    return rcpp_result_gen;
END_RCPP
}
// rowMin
NumericVector rowMin(NumericMatrix inMat);
RcppExport SEXP _kamila_rowMin(SEXP inMatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type inMat(inMatSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMin(inMat));
    return rcpp_result_gen;
END_RCPP
}
// rowMaxInds
NumericVector rowMaxInds(NumericMatrix inMat);
RcppExport SEXP _kamila_rowMaxInds(SEXP inMatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type inMat(inMatSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMaxInds(inMat));
    return rcpp_result_gen;
END_RCPP
}
// sumMatList
NumericMatrix sumMatList(List x);
RcppExport SEXP _kamila_sumMatList(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sumMatList(x));
    return rcpp_result_gen;
END_RCPP
}
// getIndividualLogProbs
List getIndividualLogProbs(NumericMatrix catFactorNum, NumericVector catWeights, List logProbsCond_i);
RcppExport SEXP _kamila_getIndividualLogProbs(SEXP catFactorNumSEXP, SEXP catWeightsSEXP, SEXP logProbsCond_iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type catFactorNum(catFactorNumSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type catWeights(catWeightsSEXP);
    Rcpp::traits::input_parameter< List >::type logProbsCond_i(logProbsCond_iSEXP);
    rcpp_result_gen = Rcpp::wrap(getIndividualLogProbs(catFactorNum, catWeights, logProbsCond_i));
    return rcpp_result_gen;
END_RCPP
}
// aggregateMeans
NumericMatrix aggregateMeans(NumericMatrix conVar, IntegerVector membNew, int kk);
RcppExport SEXP _kamila_aggregateMeans(SEXP conVarSEXP, SEXP membNewSEXP, SEXP kkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type conVar(conVarSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type membNew(membNewSEXP);
    Rcpp::traits::input_parameter< int >::type kk(kkSEXP);
    rcpp_result_gen = Rcpp::wrap(aggregateMeans(conVar, membNew, kk));
    return rcpp_result_gen;
END_RCPP
}
// jointTabSmoothedList
List jointTabSmoothedList(IntegerMatrix catFactorNum, IntegerVector membNew, IntegerVector numLev, double catBw, int kk);
RcppExport SEXP _kamila_jointTabSmoothedList(SEXP catFactorNumSEXP, SEXP membNewSEXP, SEXP numLevSEXP, SEXP catBwSEXP, SEXP kkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type catFactorNum(catFactorNumSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type membNew(membNewSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type numLev(numLevSEXP);
    Rcpp::traits::input_parameter< double >::type catBw(catBwSEXP);
    Rcpp::traits::input_parameter< int >::type kk(kkSEXP);
    rcpp_result_gen = Rcpp::wrap(jointTabSmoothedList(catFactorNum, membNew, numLev, catBw, kk));
    return rcpp_result_gen;
END_RCPP
}
