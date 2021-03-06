// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// makeinter_cpp
Rcpp::List makeinter_cpp(Rcpp::NumericVector X00, Rcpp::NumericVector X10, Rcpp::NumericVector X20);
RcppExport SEXP _sparsereg_makeinter_cpp(SEXP X00SEXP, SEXP X10SEXP, SEXP X20SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X00(X00SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X10(X10SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X20(X20SEXP);
    rcpp_result_gen = Rcpp::wrap(makeinter_cpp(X00, X10, X20));
    return rcpp_result_gen;
END_RCPP
}
// makethreeinter_cpp
Rcpp::List makethreeinter_cpp(Rcpp::NumericVector X00, Rcpp::NumericVector X10, Rcpp::NumericVector X110, Rcpp::NumericVector X20);
RcppExport SEXP _sparsereg_makethreeinter_cpp(SEXP X00SEXP, SEXP X10SEXP, SEXP X110SEXP, SEXP X20SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X00(X00SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X10(X10SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X110(X110SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X20(X20SEXP);
    rcpp_result_gen = Rcpp::wrap(makethreeinter_cpp(X00, X10, X110, X20));
    return rcpp_result_gen;
END_RCPP
}
// updatebeta_cpp
Rcpp::List updatebeta_cpp(Rcpp::NumericVector X0, Rcpp::NumericVector y0, Rcpp::NumericVector betacurr0, Rcpp::NumericVector betamode0, Rcpp::NumericVector lambdavec0, Rcpp::NumericVector dtau0, Rcpp::NumericVector sigmasq0, Rcpp::NumericVector ps_sigmasq0, Rcpp::NumericVector lambdashrink0, Rcpp::NumericVector k0);
RcppExport SEXP _sparsereg_updatebeta_cpp(SEXP X0SEXP, SEXP y0SEXP, SEXP betacurr0SEXP, SEXP betamode0SEXP, SEXP lambdavec0SEXP, SEXP dtau0SEXP, SEXP sigmasq0SEXP, SEXP ps_sigmasq0SEXP, SEXP lambdashrink0SEXP, SEXP k0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X0(X0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y0(y0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type betacurr0(betacurr0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type betamode0(betamode0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type lambdavec0(lambdavec0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dtau0(dtau0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type sigmasq0(sigmasq0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ps_sigmasq0(ps_sigmasq0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type lambdashrink0(lambdashrink0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type k0(k0SEXP);
    rcpp_result_gen = Rcpp::wrap(updatebeta_cpp(X0, y0, betacurr0, betamode0, lambdavec0, dtau0, sigmasq0, ps_sigmasq0, lambdashrink0, k0));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sparsereg_makeinter_cpp", (DL_FUNC) &_sparsereg_makeinter_cpp, 3},
    {"_sparsereg_makethreeinter_cpp", (DL_FUNC) &_sparsereg_makethreeinter_cpp, 4},
    {"_sparsereg_updatebeta_cpp", (DL_FUNC) &_sparsereg_updatebeta_cpp, 10},
    {NULL, NULL, 0}
};

RcppExport void R_init_sparsereg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
