// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// bipartition2
std::vector< std::vector<int> > bipartition2(IntegerMatrix orig, int nTips);
RcppExport SEXP _ape_bipartition2(SEXP origSEXP, SEXP nTipsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type orig(origSEXP);
    Rcpp::traits::input_parameter< int >::type nTips(nTipsSEXP);
    rcpp_result_gen = Rcpp::wrap(bipartition2(orig, nTips));
    return rcpp_result_gen;
END_RCPP
}
// prop_part2
List prop_part2(SEXP trees, int nTips);
RcppExport SEXP _ape_prop_part2(SEXP treesSEXP, SEXP nTipsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type trees(treesSEXP);
    Rcpp::traits::input_parameter< int >::type nTips(nTipsSEXP);
    rcpp_result_gen = Rcpp::wrap(prop_part2(trees, nTips));
    return rcpp_result_gen;
END_RCPP
}
// reorderRcpp
IntegerVector reorderRcpp(IntegerMatrix orig, int nTips, int root, int order);
RcppExport SEXP _ape_reorderRcpp(SEXP origSEXP, SEXP nTipsSEXP, SEXP rootSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type orig(origSEXP);
    Rcpp::traits::input_parameter< int >::type nTips(nTipsSEXP);
    Rcpp::traits::input_parameter< int >::type root(rootSEXP);
    Rcpp::traits::input_parameter< int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(reorderRcpp(orig, nTips, root, order));
    return rcpp_result_gen;
END_RCPP
}
