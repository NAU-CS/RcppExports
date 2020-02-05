// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// TukeyRegion
List TukeyRegion(NumericMatrix data, int depth, String method, bool trgFacets, bool checkInnerPoint, bool retHalfspaces, bool retHalfspacesNR, bool retInnerPoint, bool retVertices, bool retFacets, bool retVolume, bool retBarycenter, IntegerMatrix halfspaces, NumericVector innerPoint, int verbosity);
RcppExport SEXP _TukeyRegion_TukeyRegion(SEXP dataSEXP, SEXP depthSEXP, SEXP methodSEXP, SEXP trgFacetsSEXP, SEXP checkInnerPointSEXP, SEXP retHalfspacesSEXP, SEXP retHalfspacesNRSEXP, SEXP retInnerPointSEXP, SEXP retVerticesSEXP, SEXP retFacetsSEXP, SEXP retVolumeSEXP, SEXP retBarycenterSEXP, SEXP halfspacesSEXP, SEXP innerPointSEXP, SEXP verbositySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type depth(depthSEXP);
    Rcpp::traits::input_parameter< String >::type method(methodSEXP);
    Rcpp::traits::input_parameter< bool >::type trgFacets(trgFacetsSEXP);
    Rcpp::traits::input_parameter< bool >::type checkInnerPoint(checkInnerPointSEXP);
    Rcpp::traits::input_parameter< bool >::type retHalfspaces(retHalfspacesSEXP);
    Rcpp::traits::input_parameter< bool >::type retHalfspacesNR(retHalfspacesNRSEXP);
    Rcpp::traits::input_parameter< bool >::type retInnerPoint(retInnerPointSEXP);
    Rcpp::traits::input_parameter< bool >::type retVertices(retVerticesSEXP);
    Rcpp::traits::input_parameter< bool >::type retFacets(retFacetsSEXP);
    Rcpp::traits::input_parameter< bool >::type retVolume(retVolumeSEXP);
    Rcpp::traits::input_parameter< bool >::type retBarycenter(retBarycenterSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type halfspaces(halfspacesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type innerPoint(innerPointSEXP);
    Rcpp::traits::input_parameter< int >::type verbosity(verbositySEXP);
    rcpp_result_gen = Rcpp::wrap(TukeyRegion(data, depth, method, trgFacets, checkInnerPoint, retHalfspaces, retHalfspacesNR, retInnerPoint, retVertices, retFacets, retVolume, retBarycenter, halfspaces, innerPoint, verbosity));
    return rcpp_result_gen;
END_RCPP
}
// TukeyMedian
List TukeyMedian(NumericMatrix data, String algMedian, String method, bool trgFacets, bool retHalfspaces, bool retHalfspacesNR, bool retInnerPoint, bool retVertices, bool retFacets, bool retVolume, bool retBarycenter, int verbosity);
RcppExport SEXP _TukeyRegion_TukeyMedian(SEXP dataSEXP, SEXP algMedianSEXP, SEXP methodSEXP, SEXP trgFacetsSEXP, SEXP retHalfspacesSEXP, SEXP retHalfspacesNRSEXP, SEXP retInnerPointSEXP, SEXP retVerticesSEXP, SEXP retFacetsSEXP, SEXP retVolumeSEXP, SEXP retBarycenterSEXP, SEXP verbositySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< String >::type algMedian(algMedianSEXP);
    Rcpp::traits::input_parameter< String >::type method(methodSEXP);
    Rcpp::traits::input_parameter< bool >::type trgFacets(trgFacetsSEXP);
    Rcpp::traits::input_parameter< bool >::type retHalfspaces(retHalfspacesSEXP);
    Rcpp::traits::input_parameter< bool >::type retHalfspacesNR(retHalfspacesNRSEXP);
    Rcpp::traits::input_parameter< bool >::type retInnerPoint(retInnerPointSEXP);
    Rcpp::traits::input_parameter< bool >::type retVertices(retVerticesSEXP);
    Rcpp::traits::input_parameter< bool >::type retFacets(retFacetsSEXP);
    Rcpp::traits::input_parameter< bool >::type retVolume(retVolumeSEXP);
    Rcpp::traits::input_parameter< bool >::type retBarycenter(retBarycenterSEXP);
    Rcpp::traits::input_parameter< int >::type verbosity(verbositySEXP);
    rcpp_result_gen = Rcpp::wrap(TukeyMedian(data, algMedian, method, trgFacets, retHalfspaces, retHalfspacesNR, retInnerPoint, retVertices, retFacets, retVolume, retBarycenter, verbosity));
    return rcpp_result_gen;
END_RCPP
}
// TukeyRegionPlot
void TukeyRegionPlot(List region, bool newPlot, bool drawPoints, bool drawRidges, CharacterVector colorBackground, CharacterVector colorPoints, CharacterVector colorFacets, CharacterVector colorRidges, double lwd2D, int lty2D, double alpha);
RcppExport SEXP _TukeyRegion_TukeyRegionPlot(SEXP regionSEXP, SEXP newPlotSEXP, SEXP drawPointsSEXP, SEXP drawRidgesSEXP, SEXP colorBackgroundSEXP, SEXP colorPointsSEXP, SEXP colorFacetsSEXP, SEXP colorRidgesSEXP, SEXP lwd2DSEXP, SEXP lty2DSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type region(regionSEXP);
    Rcpp::traits::input_parameter< bool >::type newPlot(newPlotSEXP);
    Rcpp::traits::input_parameter< bool >::type drawPoints(drawPointsSEXP);
    Rcpp::traits::input_parameter< bool >::type drawRidges(drawRidgesSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type colorBackground(colorBackgroundSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type colorPoints(colorPointsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type colorFacets(colorFacetsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type colorRidges(colorRidgesSEXP);
    Rcpp::traits::input_parameter< double >::type lwd2D(lwd2DSEXP);
    Rcpp::traits::input_parameter< int >::type lty2D(lty2DSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    TukeyRegionPlot(region, newPlot, drawPoints, drawRidges, colorBackground, colorPoints, colorFacets, colorRidges, lwd2D, lty2D, alpha);
    return R_NilValue;
END_RCPP
}
// TukeyRegionSummary
void TukeyRegionSummary(List region);
RcppExport SEXP _TukeyRegion_TukeyRegionSummary(SEXP regionSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type region(regionSEXP);
    TukeyRegionSummary(region);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_TukeyRegion_TukeyRegion", (DL_FUNC) &_TukeyRegion_TukeyRegion, 15},
    {"_TukeyRegion_TukeyMedian", (DL_FUNC) &_TukeyRegion_TukeyMedian, 12},
    {"_TukeyRegion_TukeyRegionPlot", (DL_FUNC) &_TukeyRegion_TukeyRegionPlot, 11},
    {"_TukeyRegion_TukeyRegionSummary", (DL_FUNC) &_TukeyRegion_TukeyRegionSummary, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_TukeyRegion(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
