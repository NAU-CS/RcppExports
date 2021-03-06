// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// fastLm
Rcpp::List fastLm(const arma::vec& y, const arma::mat& X);
RcppExport SEXP _RRI_fastLm(SEXP ySEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(fastLm(y, X));
    return rcpp_result_gen;
END_RCPP
}
// OLS_c
arma::vec OLS_c(arma::vec y, arma::mat X);
RcppExport SEXP _RRI_OLS_c(SEXP ySEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(OLS_c(y, X));
    return rcpp_result_gen;
END_RCPP
}
// restricted_OLS_c
arma::mat restricted_OLS_c(arma::vec y, arma::mat X, arma::vec bhat, arma::mat Q, double c);
RcppExport SEXP _RRI_restricted_OLS_c(SEXP ySEXP, SEXP XSEXP, SEXP bhatSEXP, SEXP QSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bhat(bhatSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Q(QSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(restricted_OLS_c(y, X, bhat, Q, c));
    return rcpp_result_gen;
END_RCPP
}
// r_test_c
Rcpp::List r_test_c(arma::vec y, arma::mat X, arma::vec lam, double lam0, Rcpp::List cluster_eps_r, bool use_perm, bool use_sign, int num_R);
RcppExport SEXP _RRI_r_test_c(SEXP ySEXP, SEXP XSEXP, SEXP lamSEXP, SEXP lam0SEXP, SEXP cluster_eps_rSEXP, SEXP use_permSEXP, SEXP use_signSEXP, SEXP num_RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< double >::type lam0(lam0SEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type cluster_eps_r(cluster_eps_rSEXP);
    Rcpp::traits::input_parameter< bool >::type use_perm(use_permSEXP);
    Rcpp::traits::input_parameter< bool >::type use_sign(use_signSEXP);
    Rcpp::traits::input_parameter< int >::type num_R(num_RSEXP);
    rcpp_result_gen = Rcpp::wrap(r_test_c(y, X, lam, lam0, cluster_eps_r, use_perm, use_sign, num_R));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RRI_fastLm", (DL_FUNC) &_RRI_fastLm, 2},
    {"_RRI_OLS_c", (DL_FUNC) &_RRI_OLS_c, 2},
    {"_RRI_restricted_OLS_c", (DL_FUNC) &_RRI_restricted_OLS_c, 5},
    {"_RRI_r_test_c", (DL_FUNC) &_RRI_r_test_c, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_RRI(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
