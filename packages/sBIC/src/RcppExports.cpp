// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// pruneEdgesHelper
IntegerVector pruneEdgesHelper(IntegerVector support, IntegerMatrix edgeList, int numLeaves, int numVertices);
RcppExport SEXP sBIC_pruneEdgesHelper(SEXP supportSEXP, SEXP edgeListSEXP, SEXP numLeavesSEXP, SEXP numVerticesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type support(supportSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type edgeList(edgeListSEXP);
    Rcpp::traits::input_parameter< int >::type numLeaves(numLeavesSEXP);
    Rcpp::traits::input_parameter< int >::type numVertices(numVerticesSEXP);
    rcpp_result_gen = Rcpp::wrap(pruneEdgesHelper(support, edgeList, numLeaves, numVertices));
    return rcpp_result_gen;
END_RCPP
}
