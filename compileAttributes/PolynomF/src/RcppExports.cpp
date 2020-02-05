// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// poly_divide
List poly_divide(NumericVector Num0, NumericVector Den);
RcppExport SEXP _PolynomF_poly_divide(SEXP Num0SEXP, SEXP DenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Num0(Num0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Den(DenSEXP);
    rcpp_result_gen = Rcpp::wrap(poly_divide(Num0, Den));
    return rcpp_result_gen;
END_RCPP
}
// poly_product
NumericVector poly_product(NumericVector P, NumericVector Q);
RcppExport SEXP _PolynomF_poly_product(SEXP PSEXP, SEXP QSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type P(PSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Q(QSEXP);
    rcpp_result_gen = Rcpp::wrap(poly_product(P, Q));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_PolynomF_poly_divide", (DL_FUNC) &_PolynomF_poly_divide, 2},
    {"_PolynomF_poly_product", (DL_FUNC) &_PolynomF_poly_product, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_PolynomF(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}