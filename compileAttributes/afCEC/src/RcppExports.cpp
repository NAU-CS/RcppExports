// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// UpdateMeansForQuadraticFunction
void UpdateMeansForQuadraticFunction(Rcpp::List& res);
RcppExport SEXP _afCEC_UpdateMeansForQuadraticFunction(SEXP resSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type res(resSEXP);
    UpdateMeansForQuadraticFunction(res);
    return R_NilValue;
END_RCPP
}
// CalculateEllipsesOfConfidenceForQuadraticFunction
Rcpp::List CalculateEllipsesOfConfidenceForQuadraticFunction(Rcpp::List res, double confidence, int segments);
RcppExport SEXP _afCEC_CalculateEllipsesOfConfidenceForQuadraticFunction(SEXP resSEXP, SEXP confidenceSEXP, SEXP segmentsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type res(resSEXP);
    Rcpp::traits::input_parameter< double >::type confidence(confidenceSEXP);
    Rcpp::traits::input_parameter< int >::type segments(segmentsSEXP);
    rcpp_result_gen = Rcpp::wrap(CalculateEllipsesOfConfidenceForQuadraticFunction(res, confidence, segments));
    return rcpp_result_gen;
END_RCPP
}
// CalculateEllipsoidsOfConfidenceForQuadraticFunction
Rcpp::List CalculateEllipsoidsOfConfidenceForQuadraticFunction(Rcpp::List res, double confidence, int gridRes);
RcppExport SEXP _afCEC_CalculateEllipsoidsOfConfidenceForQuadraticFunction(SEXP resSEXP, SEXP confidenceSEXP, SEXP gridResSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type res(resSEXP);
    Rcpp::traits::input_parameter< double >::type confidence(confidenceSEXP);
    Rcpp::traits::input_parameter< int >::type gridRes(gridResSEXP);
    rcpp_result_gen = Rcpp::wrap(CalculateEllipsoidsOfConfidenceForQuadraticFunction(res, confidence, gridRes));
    return rcpp_result_gen;
END_RCPP
}
// afCECCppRoutine
Rcpp::List afCECCppRoutine(const arma::mat& points, int maxClusters, const SEXP& initialLabels, double cardMin, double costThreshold, int minIterations, int maxIterations, int numberOfStarts, const std::string& method, const arma::mat& values, bool interactive);
RcppExport SEXP _afCEC_afCECCppRoutine(SEXP pointsSEXP, SEXP maxClustersSEXP, SEXP initialLabelsSEXP, SEXP cardMinSEXP, SEXP costThresholdSEXP, SEXP minIterationsSEXP, SEXP maxIterationsSEXP, SEXP numberOfStartsSEXP, SEXP methodSEXP, SEXP valuesSEXP, SEXP interactiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< int >::type maxClusters(maxClustersSEXP);
    Rcpp::traits::input_parameter< const SEXP& >::type initialLabels(initialLabelsSEXP);
    Rcpp::traits::input_parameter< double >::type cardMin(cardMinSEXP);
    Rcpp::traits::input_parameter< double >::type costThreshold(costThresholdSEXP);
    Rcpp::traits::input_parameter< int >::type minIterations(minIterationsSEXP);
    Rcpp::traits::input_parameter< int >::type maxIterations(maxIterationsSEXP);
    Rcpp::traits::input_parameter< int >::type numberOfStarts(numberOfStartsSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type method(methodSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< bool >::type interactive(interactiveSEXP);
    rcpp_result_gen = Rcpp::wrap(afCECCppRoutine(points, maxClusters, initialLabels, cardMin, costThreshold, minIterations, maxIterations, numberOfStarts, method, values, interactive));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_afCEC_UpdateMeansForQuadraticFunction", (DL_FUNC) &_afCEC_UpdateMeansForQuadraticFunction, 1},
    {"_afCEC_CalculateEllipsesOfConfidenceForQuadraticFunction", (DL_FUNC) &_afCEC_CalculateEllipsesOfConfidenceForQuadraticFunction, 3},
    {"_afCEC_CalculateEllipsoidsOfConfidenceForQuadraticFunction", (DL_FUNC) &_afCEC_CalculateEllipsoidsOfConfidenceForQuadraticFunction, 3},
    {"_afCEC_afCECCppRoutine", (DL_FUNC) &_afCEC_afCECCppRoutine, 11},
    {NULL, NULL, 0}
};

RcppExport void R_init_afCEC(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}