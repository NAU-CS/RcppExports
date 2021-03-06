// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// soft_threshold
void soft_threshold(const arma::vec& beta, const double lam, const int r, arma::vec& result);
RcppExport SEXP _varband_soft_threshold(SEXP betaSEXP, SEXP lamSEXP, SEXP rSEXP, SEXP resultSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const double >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< const int >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type result(resultSEXP);
    soft_threshold(beta, lam, r, result);
    return R_NilValue;
END_RCPP
}
// close_update
void close_update(const arma::mat& S, const arma::mat& S_inv, const int r, const double rho, const arma::vec& u, const arma::vec& gamma, arma::vec& res);
RcppExport SEXP _varband_close_update(SEXP SSEXP, SEXP S_invSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP uSEXP, SEXP gammaSEXP, SEXP resSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type S(SSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type S_inv(S_invSEXP);
    Rcpp::traits::input_parameter< const int >::type r(rSEXP);
    Rcpp::traits::input_parameter< const double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type u(uSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type res(resSEXP);
    close_update(S, S_inv, r, rho, u, gamma, res);
    return R_NilValue;
END_RCPP
}
// inverse_update
void inverse_update(const arma::mat& S, double rho, arma::mat& S_inv);
RcppExport SEXP _varband_inverse_update(SEXP SSEXP, SEXP rhoSEXP, SEXP S_invSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type S(SSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type S_inv(S_invSEXP);
    inverse_update(S, rho, S_inv);
    return R_NilValue;
END_RCPP
}
// elliproj_u
void elliproj_u(const arma::vec& y, const double tau, arma::vec& pp);
RcppExport SEXP _varband_elliproj_u(SEXP ySEXP, SEXP tauSEXP, SEXP ppSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type pp(ppSEXP);
    elliproj_u(y, tau, pp);
    return R_NilValue;
END_RCPP
}
// rootfind
double rootfind(const arma::vec& pp, const arma::vec& ww, double tau, int l);
RcppExport SEXP _varband_rootfind(SEXP ppSEXP, SEXP wwSEXP, SEXP tauSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type pp(ppSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type ww(wwSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(rootfind(pp, ww, tau, l));
    return rcpp_result_gen;
END_RCPP
}
// elliproj_w
void elliproj_w(const arma::vec& y, const double tau, arma::vec& pp);
RcppExport SEXP _varband_elliproj_w(SEXP ySEXP, SEXP tauSEXP, SEXP ppSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type pp(ppSEXP);
    elliproj_w(y, tau, pp);
    return R_NilValue;
END_RCPP
}
// rowadmm
arma::vec rowadmm(const arma::mat& S, const arma::vec& init_row, const double lambda, const bool w, double tol, const int itermax);
RcppExport SEXP _varband_rowadmm(SEXP SSEXP, SEXP init_rowSEXP, SEXP lambdaSEXP, SEXP wSEXP, SEXP tolSEXP, SEXP itermaxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type S(SSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type init_row(init_rowSEXP);
    Rcpp::traits::input_parameter< const double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const bool >::type w(wSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< const int >::type itermax(itermaxSEXP);
    rcpp_result_gen = Rcpp::wrap(rowadmm(S, init_row, lambda, w, tol, itermax));
    return rcpp_result_gen;
END_RCPP
}
// rowadmm_lasso
arma::vec rowadmm_lasso(const arma::mat& S, const arma::vec& init_row, const double lambda, double tol, const int itermax);
RcppExport SEXP _varband_rowadmm_lasso(SEXP SSEXP, SEXP init_rowSEXP, SEXP lambdaSEXP, SEXP tolSEXP, SEXP itermaxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type S(SSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type init_row(init_rowSEXP);
    Rcpp::traits::input_parameter< const double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< const int >::type itermax(itermaxSEXP);
    rcpp_result_gen = Rcpp::wrap(rowadmm_lasso(S, init_row, lambda, tol, itermax));
    return rcpp_result_gen;
END_RCPP
}
// varband
arma::mat varband(arma::mat S, double lambda, arma::mat init, bool w, bool lasso);
RcppExport SEXP _varband_varband(SEXP SSEXP, SEXP lambdaSEXP, SEXP initSEXP, SEXP wSEXP, SEXP lassoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type S(SSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type init(initSEXP);
    Rcpp::traits::input_parameter< bool >::type w(wSEXP);
    Rcpp::traits::input_parameter< bool >::type lasso(lassoSEXP);
    rcpp_result_gen = Rcpp::wrap(varband(S, lambda, init, w, lasso));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_varband_soft_threshold", (DL_FUNC) &_varband_soft_threshold, 4},
    {"_varband_close_update", (DL_FUNC) &_varband_close_update, 7},
    {"_varband_inverse_update", (DL_FUNC) &_varband_inverse_update, 3},
    {"_varband_elliproj_u", (DL_FUNC) &_varband_elliproj_u, 3},
    {"_varband_rootfind", (DL_FUNC) &_varband_rootfind, 4},
    {"_varband_elliproj_w", (DL_FUNC) &_varband_elliproj_w, 3},
    {"_varband_rowadmm", (DL_FUNC) &_varband_rowadmm, 6},
    {"_varband_rowadmm_lasso", (DL_FUNC) &_varband_rowadmm_lasso, 5},
    {"_varband_varband", (DL_FUNC) &_varband_varband, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_varband(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
