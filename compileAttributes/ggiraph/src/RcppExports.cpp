// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// DSVG_
bool DSVG_(std::string file, double width, double height, std::string bg, int pointsize, bool standalone, std::string canvas_id, Rcpp::List aliases);
RcppExport SEXP _ggiraph_DSVG_(SEXP fileSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP bgSEXP, SEXP pointsizeSEXP, SEXP standaloneSEXP, SEXP canvas_idSEXP, SEXP aliasesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type file(fileSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type height(heightSEXP);
    Rcpp::traits::input_parameter< std::string >::type bg(bgSEXP);
    Rcpp::traits::input_parameter< int >::type pointsize(pointsizeSEXP);
    Rcpp::traits::input_parameter< bool >::type standalone(standaloneSEXP);
    Rcpp::traits::input_parameter< std::string >::type canvas_id(canvas_idSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type aliases(aliasesSEXP);
    rcpp_result_gen = Rcpp::wrap(DSVG_(file, width, height, bg, pointsize, standalone, canvas_id, aliases));
    return rcpp_result_gen;
END_RCPP
}
// set_tracer_on
bool set_tracer_on(int dn);
RcppExport SEXP _ggiraph_set_tracer_on(SEXP dnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type dn(dnSEXP);
    rcpp_result_gen = Rcpp::wrap(set_tracer_on(dn));
    return rcpp_result_gen;
END_RCPP
}
// set_tracer_off
bool set_tracer_off(int dn);
RcppExport SEXP _ggiraph_set_tracer_off(SEXP dnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type dn(dnSEXP);
    rcpp_result_gen = Rcpp::wrap(set_tracer_off(dn));
    return rcpp_result_gen;
END_RCPP
}
// collect_id
Rcpp::IntegerVector collect_id(int dn);
RcppExport SEXP _ggiraph_collect_id(SEXP dnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type dn(dnSEXP);
    rcpp_result_gen = Rcpp::wrap(collect_id(dn));
    return rcpp_result_gen;
END_RCPP
}
// add_attribute
bool add_attribute(int dn, Rcpp::IntegerVector id, std::vector< std::string > str, std::string name);
RcppExport SEXP _ggiraph_add_attribute(SEXP dnSEXP, SEXP idSEXP, SEXP strSEXP, SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type dn(dnSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type id(idSEXP);
    Rcpp::traits::input_parameter< std::vector< std::string > >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    rcpp_result_gen = Rcpp::wrap(add_attribute(dn, id, str, name));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ggiraph_DSVG_", (DL_FUNC) &_ggiraph_DSVG_, 8},
    {"_ggiraph_set_tracer_on", (DL_FUNC) &_ggiraph_set_tracer_on, 1},
    {"_ggiraph_set_tracer_off", (DL_FUNC) &_ggiraph_set_tracer_off, 1},
    {"_ggiraph_collect_id", (DL_FUNC) &_ggiraph_collect_id, 1},
    {"_ggiraph_add_attribute", (DL_FUNC) &_ggiraph_add_attribute, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_ggiraph(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
