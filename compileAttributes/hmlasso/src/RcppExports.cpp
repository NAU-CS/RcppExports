// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// covC
NumericMatrix covC(NumericMatrix X);
RcppExport SEXP _hmlasso_covC(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(covC(X));
    return rcpp_result_gen;
END_RCPP
}
// softThresholdC
double softThresholdC(double z, double g);
RcppExport SEXP _hmlasso_softThresholdC(SEXP zSEXP, SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type g(gSEXP);
    rcpp_result_gen = Rcpp::wrap(softThresholdC(z, g));
    return rcpp_result_gen;
END_RCPP
}
// updateLassoC
double updateLassoC(double u, double l1, double l2, double v);
RcppExport SEXP _hmlasso_updateLassoC(SEXP uSEXP, SEXP l1SEXP, SEXP l2SEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type l1(l1SEXP);
    Rcpp::traits::input_parameter< double >::type l2(l2SEXP);
    Rcpp::traits::input_parameter< double >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(updateLassoC(u, l1, l2, v));
    return rcpp_result_gen;
END_RCPP
}
// covCdaC
NumericMatrix covCdaC(NumericMatrix Gamma, NumericVector gamma, NumericVector lambda, NumericMatrix R, NumericMatrix init_beta, double delta, double maxit, double eps, String warm, bool strong);
RcppExport SEXP _hmlasso_covCdaC(SEXP GammaSEXP, SEXP gammaSEXP, SEXP lambdaSEXP, SEXP RSEXP, SEXP init_betaSEXP, SEXP deltaSEXP, SEXP maxitSEXP, SEXP epsSEXP, SEXP warmSEXP, SEXP strongSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Gamma(GammaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type R(RSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type init_beta(init_betaSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< double >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< String >::type warm(warmSEXP);
    Rcpp::traits::input_parameter< bool >::type strong(strongSEXP);
    rcpp_result_gen = Rcpp::wrap(covCdaC(Gamma, gamma, lambda, R, init_beta, delta, maxit, eps, warm, strong));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_hmlasso_covC", (DL_FUNC) &_hmlasso_covC, 1},
    {"_hmlasso_softThresholdC", (DL_FUNC) &_hmlasso_softThresholdC, 2},
    {"_hmlasso_updateLassoC", (DL_FUNC) &_hmlasso_updateLassoC, 4},
    {"_hmlasso_covCdaC", (DL_FUNC) &_hmlasso_covCdaC, 10},
    {NULL, NULL, 0}
};

RcppExport void R_init_hmlasso(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
