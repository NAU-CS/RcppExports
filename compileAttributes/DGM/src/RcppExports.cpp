// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// dlmLplCpp
arma::rowvec dlmLplCpp(NumericVector Yt_, NumericMatrix Ft_, double delta, double m0_, double CS0_, double n0, double d0);
RcppExport SEXP _DGM_dlmLplCpp(SEXP Yt_SEXP, SEXP Ft_SEXP, SEXP deltaSEXP, SEXP m0_SEXP, SEXP CS0_SEXP, SEXP n0SEXP, SEXP d0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Yt_(Yt_SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Ft_(Ft_SEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< double >::type m0_(m0_SEXP);
    Rcpp::traits::input_parameter< double >::type CS0_(CS0_SEXP);
    Rcpp::traits::input_parameter< double >::type n0(n0SEXP);
    Rcpp::traits::input_parameter< double >::type d0(d0SEXP);
    rcpp_result_gen = Rcpp::wrap(dlmLplCpp(Yt_, Ft_, delta, m0_, CS0_, n0, d0));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_DGM_dlmLplCpp", (DL_FUNC) &_DGM_dlmLplCpp, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_DGM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}