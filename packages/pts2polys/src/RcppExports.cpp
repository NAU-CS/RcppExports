// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// pts2polys
List pts2polys(std::string in_string, int SAMPLESIZE, long MINLEN, long GRIDSIZE, long MINX, long MAXX, long MINY, long MAXY);
RcppExport SEXP _pts2polys_pts2polys(SEXP in_stringSEXP, SEXP SAMPLESIZESEXP, SEXP MINLENSEXP, SEXP GRIDSIZESEXP, SEXP MINXSEXP, SEXP MAXXSEXP, SEXP MINYSEXP, SEXP MAXYSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type in_string(in_stringSEXP);
    Rcpp::traits::input_parameter< int >::type SAMPLESIZE(SAMPLESIZESEXP);
    Rcpp::traits::input_parameter< long >::type MINLEN(MINLENSEXP);
    Rcpp::traits::input_parameter< long >::type GRIDSIZE(GRIDSIZESEXP);
    Rcpp::traits::input_parameter< long >::type MINX(MINXSEXP);
    Rcpp::traits::input_parameter< long >::type MAXX(MAXXSEXP);
    Rcpp::traits::input_parameter< long >::type MINY(MINYSEXP);
    Rcpp::traits::input_parameter< long >::type MAXY(MAXYSEXP);
    rcpp_result_gen = Rcpp::wrap(pts2polys(in_string, SAMPLESIZE, MINLEN, GRIDSIZE, MINX, MAXX, MINY, MAXY));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_pts2polys_pts2polys", (DL_FUNC) &_pts2polys_pts2polys, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_pts2polys(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}