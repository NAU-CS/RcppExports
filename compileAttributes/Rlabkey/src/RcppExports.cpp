// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// listToMatrix
CharacterMatrix listToMatrix(List data, List names);
RcppExport SEXP _Rlabkey_listToMatrix(SEXP dataSEXP, SEXP namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type data(dataSEXP);
    Rcpp::traits::input_parameter< List >::type names(namesSEXP);
    rcpp_result_gen = Rcpp::wrap(listToMatrix(data, names));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP .JSON_to_R(SEXP);
RcppExport SEXP .parser_new();
RcppExport SEXP parser_add(SEXP, SEXP, SEXP);
RcppExport SEXP parser_delete(SEXP);
RcppExport SEXP parser_finalize(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_Rlabkey_listToMatrix", (DL_FUNC) &_Rlabkey_listToMatrix, 2},
    {".JSON_to_R",      (DL_FUNC) &.JSON_to_R,      1},
    {".parser_new",     (DL_FUNC) &.parser_new,     0},
    {"parser_add",      (DL_FUNC) &parser_add,      3},
    {"parser_delete",   (DL_FUNC) &parser_delete,   1},
    {"parser_finalize", (DL_FUNC) &parser_finalize, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_Rlabkey(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}