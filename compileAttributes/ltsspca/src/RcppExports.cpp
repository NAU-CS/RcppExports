// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// findpcs
Rcpp::List findpcs(arma::mat x, arma::mat B, arma::rowvec mu, int h, double s, int N1, int N2, int Npc, double tol, bool fixed);
RcppExport SEXP _ltsspca_findpcs(SEXP xSEXP, SEXP BSEXP, SEXP muSEXP, SEXP hSEXP, SEXP sSEXP, SEXP N1SEXP, SEXP N2SEXP, SEXP NpcSEXP, SEXP tolSEXP, SEXP fixedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type N1(N1SEXP);
    Rcpp::traits::input_parameter< int >::type N2(N2SEXP);
    Rcpp::traits::input_parameter< int >::type Npc(NpcSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< bool >::type fixed(fixedSEXP);
    rcpp_result_gen = Rcpp::wrap(findpcs(x, B, mu, h, s, N1, N2, Npc, tol, fixed));
    return rcpp_result_gen;
END_RCPP
}
// findpcs2
Rcpp::List findpcs2(arma::mat x, arma::mat B, arma::rowvec mu, int h, double s, int N1, int N2, int Npc, double tol, bool fixed);
RcppExport SEXP _ltsspca_findpcs2(SEXP xSEXP, SEXP BSEXP, SEXP muSEXP, SEXP hSEXP, SEXP sSEXP, SEXP N1SEXP, SEXP N2SEXP, SEXP NpcSEXP, SEXP tolSEXP, SEXP fixedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type N1(N1SEXP);
    Rcpp::traits::input_parameter< int >::type N2(N2SEXP);
    Rcpp::traits::input_parameter< int >::type Npc(NpcSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< bool >::type fixed(fixedSEXP);
    rcpp_result_gen = Rcpp::wrap(findpcs2(x, B, mu, h, s, N1, N2, Npc, tol, fixed));
    return rcpp_result_gen;
END_RCPP
}
// findpcs0
Rcpp::List findpcs0(arma::mat x, arma::mat B, arma::rowvec mu, int Npc, double tol);
RcppExport SEXP _ltsspca_findpcs0(SEXP xSEXP, SEXP BSEXP, SEXP muSEXP, SEXP NpcSEXP, SEXP tolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type Npc(NpcSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    rcpp_result_gen = Rcpp::wrap(findpcs0(x, B, mu, Npc, tol));
    return rcpp_result_gen;
END_RCPP
}
// findsparsePC
Rcpp::List findsparsePC(arma::mat x, arma::mat b, arma::rowvec mu, double alpha, int l, int N2bis, int Npc, double tol);
RcppExport SEXP _ltsspca_findsparsePC(SEXP xSEXP, SEXP bSEXP, SEXP muSEXP, SEXP alphaSEXP, SEXP lSEXP, SEXP N2bisSEXP, SEXP NpcSEXP, SEXP tolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    Rcpp::traits::input_parameter< int >::type N2bis(N2bisSEXP);
    Rcpp::traits::input_parameter< int >::type Npc(NpcSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    rcpp_result_gen = Rcpp::wrap(findsparsePC(x, b, mu, alpha, l, N2bis, Npc, tol));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ltsspca_findpcs", (DL_FUNC) &_ltsspca_findpcs, 10},
    {"_ltsspca_findpcs2", (DL_FUNC) &_ltsspca_findpcs2, 10},
    {"_ltsspca_findpcs0", (DL_FUNC) &_ltsspca_findpcs0, 5},
    {"_ltsspca_findsparsePC", (DL_FUNC) &_ltsspca_findsparsePC, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_ltsspca(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
