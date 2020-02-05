// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// isLabelName
bool isLabelName(Rcpp::CharacterVector lblToCheck, Rcpp::CharacterVector lbl);
RcppExport SEXP _phylobase_isLabelName(SEXP lblToCheckSEXP, SEXP lblSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type lblToCheck(lblToCheckSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type lbl(lblSEXP);
    rcpp_result_gen = Rcpp::wrap(isLabelName(lblToCheck, lbl));
    return rcpp_result_gen;
END_RCPP
}
// nRoots
int nRoots(Rcpp::IntegerVector ances);
RcppExport SEXP _phylobase_nRoots(SEXP ancesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    rcpp_result_gen = Rcpp::wrap(nRoots(ances));
    return rcpp_result_gen;
END_RCPP
}
// tabulateTips
std::vector<int> tabulateTips(Rcpp::IntegerVector ances);
RcppExport SEXP _phylobase_tabulateTips(SEXP ancesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    rcpp_result_gen = Rcpp::wrap(tabulateTips(ances));
    return rcpp_result_gen;
END_RCPP
}
// nTipsSafe
int nTipsSafe(Rcpp::IntegerVector ances);
RcppExport SEXP _phylobase_nTipsSafe(SEXP ancesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    rcpp_result_gen = Rcpp::wrap(nTipsSafe(ances));
    return rcpp_result_gen;
END_RCPP
}
// nTipsFastCpp
int nTipsFastCpp(Rcpp::IntegerVector ances);
RcppExport SEXP _phylobase_nTipsFastCpp(SEXP ancesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    rcpp_result_gen = Rcpp::wrap(nTipsFastCpp(ances));
    return rcpp_result_gen;
END_RCPP
}
// hasSingleton
bool hasSingleton(Rcpp::IntegerVector ances);
RcppExport SEXP _phylobase_hasSingleton(SEXP ancesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    rcpp_result_gen = Rcpp::wrap(hasSingleton(ances));
    return rcpp_result_gen;
END_RCPP
}
// hasPolytomy
bool hasPolytomy(Rcpp::IntegerVector ances);
RcppExport SEXP _phylobase_hasPolytomy(SEXP ancesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    rcpp_result_gen = Rcpp::wrap(hasPolytomy(ances));
    return rcpp_result_gen;
END_RCPP
}
// tipsSafe
Rcpp::IntegerVector tipsSafe(Rcpp::IntegerVector ances, Rcpp::IntegerVector desc);
RcppExport SEXP _phylobase_tipsSafe(SEXP ancesSEXP, SEXP descSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type desc(descSEXP);
    rcpp_result_gen = Rcpp::wrap(tipsSafe(ances, desc));
    return rcpp_result_gen;
END_RCPP
}
// tipsFast
Rcpp::IntegerVector tipsFast(Rcpp::IntegerVector ances);
RcppExport SEXP _phylobase_tipsFast(SEXP ancesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type ances(ancesSEXP);
    rcpp_result_gen = Rcpp::wrap(tipsFast(ances));
    return rcpp_result_gen;
END_RCPP
}
// getAllNodesSafe
Rcpp::IntegerVector getAllNodesSafe(Rcpp::IntegerMatrix edge);
RcppExport SEXP _phylobase_getAllNodesSafe(SEXP edgeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type edge(edgeSEXP);
    rcpp_result_gen = Rcpp::wrap(getAllNodesSafe(edge));
    return rcpp_result_gen;
END_RCPP
}
// getAllNodesFast
Rcpp::IntegerVector getAllNodesFast(Rcpp::IntegerMatrix edge);
RcppExport SEXP _phylobase_getAllNodesFast(SEXP edgeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type edge(edgeSEXP);
    rcpp_result_gen = Rcpp::wrap(getAllNodesFast(edge));
    return rcpp_result_gen;
END_RCPP
}
// testEqInt
Rcpp::List testEqInt(Rcpp::IntegerVector x, Rcpp::IntegerVector y);
RcppExport SEXP _phylobase_testEqInt(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(testEqInt(x, y));
    return rcpp_result_gen;
END_RCPP
}
// all_naC
bool all_naC(Rcpp::NumericVector x);
RcppExport SEXP _phylobase_all_naC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(all_naC(x));
    return rcpp_result_gen;
END_RCPP
}
// any_naC
bool any_naC(Rcpp::NumericVector x);
RcppExport SEXP _phylobase_any_naC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(any_naC(x));
    return rcpp_result_gen;
END_RCPP
}
// nb_naC
int nb_naC(Rcpp::NumericVector x);
RcppExport SEXP _phylobase_nb_naC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(nb_naC(x));
    return rcpp_result_gen;
END_RCPP
}
// getRange
Rcpp::NumericVector getRange(Rcpp::NumericVector x, const bool na_rm);
RcppExport SEXP _phylobase_getRange(SEXP xSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(getRange(x, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// hasDuplicatedLabelsCpp
bool hasDuplicatedLabelsCpp(Rcpp::CharacterVector label);
RcppExport SEXP _phylobase_hasDuplicatedLabelsCpp(SEXP labelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type label(labelSEXP);
    rcpp_result_gen = Rcpp::wrap(hasDuplicatedLabelsCpp(label));
    return rcpp_result_gen;
END_RCPP
}
// edgeIdCpp
Rcpp::CharacterVector edgeIdCpp(Rcpp::IntegerMatrix edge, std::string type);
RcppExport SEXP _phylobase_edgeIdCpp(SEXP edgeSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type edge(edgeSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(edgeIdCpp(edge, type));
    return rcpp_result_gen;
END_RCPP
}
// checkTreeCpp
Rcpp::List checkTreeCpp(Rcpp::S4 obj, Rcpp::List opts);
RcppExport SEXP _phylobase_checkTreeCpp(SEXP objSEXP, SEXP optsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::S4 >::type obj(objSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type opts(optsSEXP);
    rcpp_result_gen = Rcpp::wrap(checkTreeCpp(obj, opts));
    return rcpp_result_gen;
END_RCPP
}