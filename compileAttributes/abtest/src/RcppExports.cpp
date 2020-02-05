// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// minl_cpp
double minl_cpp(double beta, double psi, int y1, int y2, int n1, int n2, double mu_beta, double sigma_beta, double mu_psi, double sigma_psi);
RcppExport SEXP _abtest_minl_cpp(SEXP betaSEXP, SEXP psiSEXP, SEXP y1SEXP, SEXP y2SEXP, SEXP n1SEXP, SEXP n2SEXP, SEXP mu_betaSEXP, SEXP sigma_betaSEXP, SEXP mu_psiSEXP, SEXP sigma_psiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< int >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< int >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< int >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type mu_beta(mu_betaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_beta(sigma_betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu_psi(mu_psiSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_psi(sigma_psiSEXP);
    rcpp_result_gen = Rcpp::wrap(minl_cpp(beta, psi, y1, y2, n1, n2, mu_beta, sigma_beta, mu_psi, sigma_psi));
    return rcpp_result_gen;
END_RCPP
}
// apply_minl_cpp
NumericVector apply_minl_cpp(NumericMatrix s, int y1, int y2, int n1, int n2, double mu_beta, double sigma_beta, double mu_psi, double sigma_psi);
RcppExport SEXP _abtest_apply_minl_cpp(SEXP sSEXP, SEXP y1SEXP, SEXP y2SEXP, SEXP n1SEXP, SEXP n2SEXP, SEXP mu_betaSEXP, SEXP sigma_betaSEXP, SEXP mu_psiSEXP, SEXP sigma_psiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< int >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< int >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type mu_beta(mu_betaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_beta(sigma_betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu_psi(mu_psiSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_psi(sigma_psiSEXP);
    rcpp_result_gen = Rcpp::wrap(apply_minl_cpp(s, y1, y2, n1, n2, mu_beta, sigma_beta, mu_psi, sigma_psi));
    return rcpp_result_gen;
END_RCPP
}
// minlminus_cpp
double minlminus_cpp(double beta, double xi, int y1, int y2, int n1, int n2, double mu_beta, double sigma_beta, double mu_psi, double sigma_psi);
RcppExport SEXP _abtest_minlminus_cpp(SEXP betaSEXP, SEXP xiSEXP, SEXP y1SEXP, SEXP y2SEXP, SEXP n1SEXP, SEXP n2SEXP, SEXP mu_betaSEXP, SEXP sigma_betaSEXP, SEXP mu_psiSEXP, SEXP sigma_psiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< int >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< int >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< int >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type mu_beta(mu_betaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_beta(sigma_betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu_psi(mu_psiSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_psi(sigma_psiSEXP);
    rcpp_result_gen = Rcpp::wrap(minlminus_cpp(beta, xi, y1, y2, n1, n2, mu_beta, sigma_beta, mu_psi, sigma_psi));
    return rcpp_result_gen;
END_RCPP
}
// apply_minlminus_cpp
NumericVector apply_minlminus_cpp(NumericMatrix s, int y1, int y2, int n1, int n2, double mu_beta, double sigma_beta, double mu_psi, double sigma_psi);
RcppExport SEXP _abtest_apply_minlminus_cpp(SEXP sSEXP, SEXP y1SEXP, SEXP y2SEXP, SEXP n1SEXP, SEXP n2SEXP, SEXP mu_betaSEXP, SEXP sigma_betaSEXP, SEXP mu_psiSEXP, SEXP sigma_psiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< int >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< int >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type mu_beta(mu_betaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_beta(sigma_betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu_psi(mu_psiSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_psi(sigma_psiSEXP);
    rcpp_result_gen = Rcpp::wrap(apply_minlminus_cpp(s, y1, y2, n1, n2, mu_beta, sigma_beta, mu_psi, sigma_psi));
    return rcpp_result_gen;
END_RCPP
}
// minlplus_cpp
double minlplus_cpp(double beta, double xi, int y1, int y2, int n1, int n2, double mu_beta, double sigma_beta, double mu_psi, double sigma_psi);
RcppExport SEXP _abtest_minlplus_cpp(SEXP betaSEXP, SEXP xiSEXP, SEXP y1SEXP, SEXP y2SEXP, SEXP n1SEXP, SEXP n2SEXP, SEXP mu_betaSEXP, SEXP sigma_betaSEXP, SEXP mu_psiSEXP, SEXP sigma_psiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< int >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< int >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< int >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type mu_beta(mu_betaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_beta(sigma_betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu_psi(mu_psiSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_psi(sigma_psiSEXP);
    rcpp_result_gen = Rcpp::wrap(minlplus_cpp(beta, xi, y1, y2, n1, n2, mu_beta, sigma_beta, mu_psi, sigma_psi));
    return rcpp_result_gen;
END_RCPP
}
// apply_minlplus_cpp
NumericVector apply_minlplus_cpp(NumericMatrix s, int y1, int y2, int n1, int n2, double mu_beta, double sigma_beta, double mu_psi, double sigma_psi);
RcppExport SEXP _abtest_apply_minlplus_cpp(SEXP sSEXP, SEXP y1SEXP, SEXP y2SEXP, SEXP n1SEXP, SEXP n2SEXP, SEXP mu_betaSEXP, SEXP sigma_betaSEXP, SEXP mu_psiSEXP, SEXP sigma_psiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< int >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< int >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type mu_beta(mu_betaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_beta(sigma_betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu_psi(mu_psiSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_psi(sigma_psiSEXP);
    rcpp_result_gen = Rcpp::wrap(apply_minlplus_cpp(s, y1, y2, n1, n2, mu_beta, sigma_beta, mu_psi, sigma_psi));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_abtest_minl_cpp", (DL_FUNC) &_abtest_minl_cpp, 10},
    {"_abtest_apply_minl_cpp", (DL_FUNC) &_abtest_apply_minl_cpp, 9},
    {"_abtest_minlminus_cpp", (DL_FUNC) &_abtest_minlminus_cpp, 10},
    {"_abtest_apply_minlminus_cpp", (DL_FUNC) &_abtest_apply_minlminus_cpp, 9},
    {"_abtest_minlplus_cpp", (DL_FUNC) &_abtest_minlplus_cpp, 10},
    {"_abtest_apply_minlplus_cpp", (DL_FUNC) &_abtest_apply_minlplus_cpp, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_abtest(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}