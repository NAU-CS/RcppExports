// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// set_ODE_Rcpp
List set_ODE_Rcpp(double t, arma::vec n, Rcpp::List parameters);
RcppExport SEXP _RLumModel_set_ODE_Rcpp(SEXP tSEXP, SEXP nSEXP, SEXP parametersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type parameters(parametersSEXP);
    rcpp_result_gen = Rcpp::wrap(set_ODE_Rcpp(t, n, parameters));
    return rcpp_result_gen;
END_RCPP
}
// set_ODE_Rcpp_LM_OSL
List set_ODE_Rcpp_LM_OSL(double t, arma::vec n, Rcpp::List parameters);
RcppExport SEXP _RLumModel_set_ODE_Rcpp_LM_OSL(SEXP tSEXP, SEXP nSEXP, SEXP parametersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type parameters(parametersSEXP);
    rcpp_result_gen = Rcpp::wrap(set_ODE_Rcpp_LM_OSL(t, n, parameters));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RLumModel_set_ODE_Rcpp", (DL_FUNC) &_RLumModel_set_ODE_Rcpp, 3},
    {"_RLumModel_set_ODE_Rcpp_LM_OSL", (DL_FUNC) &_RLumModel_set_ODE_Rcpp_LM_OSL, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_RLumModel(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}