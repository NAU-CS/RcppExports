// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// as_fusionTree
Rcpp::IntegerMatrix as_fusionTree(IntegerMatrix merge, IntegerVector order);
RcppExport SEXP _mergeTrees_as_fusionTree(SEXP mergeSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type merge(mergeSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(as_fusionTree(merge, order));
    return rcpp_result_gen;
END_RCPP
}
// export_order
Rcpp::IntegerVector export_order(const IntegerMatrix& merge, const IntegerVector& size);
RcppExport SEXP _mergeTrees_export_order(SEXP mergeSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix& >::type merge(mergeSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(export_order(merge, size));
    return rcpp_result_gen;
END_RCPP
}
// getMergeMatrix
Rcpp::List getMergeMatrix(IntegerVector group, IntegerVector parent, IntegerVector order);
RcppExport SEXP _mergeTrees_getMergeMatrix(SEXP groupSEXP, SEXP parentSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type group(groupSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type parent(parentSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(getMergeMatrix(group, parent, order));
    return rcpp_result_gen;
END_RCPP
}
// pruneSplits
List pruneSplits(List listSetRules, IntegerMatrix orderRules, int n);
RcppExport SEXP _mergeTrees_pruneSplits(SEXP listSetRulesSEXP, SEXP orderRulesSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type listSetRules(listSetRulesSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type orderRules(orderRulesSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(pruneSplits(listSetRules, orderRules, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mergeTrees_as_fusionTree", (DL_FUNC) &_mergeTrees_as_fusionTree, 2},
    {"_mergeTrees_export_order", (DL_FUNC) &_mergeTrees_export_order, 2},
    {"_mergeTrees_getMergeMatrix", (DL_FUNC) &_mergeTrees_getMergeMatrix, 3},
    {"_mergeTrees_pruneSplits", (DL_FUNC) &_mergeTrees_pruneSplits, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_mergeTrees(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
