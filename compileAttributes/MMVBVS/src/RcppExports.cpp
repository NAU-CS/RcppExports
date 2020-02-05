// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// colmeanNA
arma::rowvec colmeanNA(arma::mat Y);
RcppExport SEXP _MMVBVS_colmeanNA(SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(colmeanNA(Y));
    return rcpp_result_gen;
END_RCPP
}
// sample_index
int sample_index(const int size, const NumericVector prob);
RcppExport SEXP _MMVBVS_sample_index(SEXP sizeSEXP, SEXP probSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type prob(probSEXP);
    rcpp_result_gen = Rcpp::wrap(sample_index(size, prob));
    return rcpp_result_gen;
END_RCPP
}
// mvrnormArma
arma::mat mvrnormArma(const int n, const arma::vec mu, const arma::mat Sigma);
RcppExport SEXP _MMVBVS_mvrnormArma(SEXP nSEXP, SEXP muSEXP, SEXP SigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Sigma(SigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(mvrnormArma(n, mu, Sigma));
    return rcpp_result_gen;
END_RCPP
}
// dmvnrm_arma
double dmvnrm_arma(const arma::rowvec x, const arma::rowvec mean, const arma::mat sigma, const bool logd);
RcppExport SEXP _MMVBVS_dmvnrm_arma(SEXP xSEXP, SEXP meanSEXP, SEXP sigmaSEXP, SEXP logdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::rowvec >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< const bool >::type logd(logdSEXP);
    rcpp_result_gen = Rcpp::wrap(dmvnrm_arma(x, mean, sigma, logd));
    return rcpp_result_gen;
END_RCPP
}
// em_with_zero_mean_c
arma::mat em_with_zero_mean_c(arma::mat y, const int maxit);
RcppExport SEXP _MMVBVS_em_with_zero_mean_c(SEXP ySEXP, SEXP maxitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    Rcpp::traits::input_parameter< const int >::type maxit(maxitSEXP);
    rcpp_result_gen = Rcpp::wrap(em_with_zero_mean_c(y, maxit));
    return rcpp_result_gen;
END_RCPP
}
// rinvwish_c
arma::cube rinvwish_c(const int n, const int v, const arma::mat S);
RcppExport SEXP _MMVBVS_rinvwish_c(SEXP nSEXP, SEXP vSEXP, SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type v(vSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type S(SSEXP);
    rcpp_result_gen = Rcpp::wrap(rinvwish_c(n, v, S));
    return rcpp_result_gen;
END_RCPP
}
// get_target_c
arma::vec get_target_c(const arma::vec X, const arma::mat Y, const double sigmabeta, const arma::mat Sigma, const arma::vec gam, const arma::vec beta);
RcppExport SEXP _MMVBVS_get_target_c(SEXP XSEXP, SEXP YSEXP, SEXP sigmabetaSEXP, SEXP SigmaSEXP, SEXP gamSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const double >::type sigmabeta(sigmabetaSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Sigma(SigmaSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(get_target_c(X, Y, sigmabeta, Sigma, gam, beta));
    return rcpp_result_gen;
END_RCPP
}
// update_Sigma_c
arma::mat update_Sigma_c(const int n, const int nu, const arma::vec X, const arma::vec beta, const arma::mat Phi, const arma::mat Y);
RcppExport SEXP _MMVBVS_update_Sigma_c(SEXP nSEXP, SEXP nuSEXP, SEXP XSEXP, SEXP betaSEXP, SEXP PhiSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Phi(PhiSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(update_Sigma_c(n, nu, X, beta, Phi, Y));
    return rcpp_result_gen;
END_RCPP
}
// update_gamma_random_c
Rcpp::List update_gamma_random_c(const arma::vec X, const arma::mat Y, const arma::vec gam);
RcppExport SEXP _MMVBVS_update_gamma_random_c(SEXP XSEXP, SEXP YSEXP, SEXP gamSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type gam(gamSEXP);
    rcpp_result_gen = Rcpp::wrap(update_gamma_random_c(X, Y, gam));
    return rcpp_result_gen;
END_RCPP
}
// betagam_accept_random_c
arma::vec betagam_accept_random_c(const arma::vec X, const arma::mat Y, const double sigmabeta1, const arma::mat inputSigma, const double Vbeta, const arma::vec gam1, const arma::vec beta1, const arma::vec gam2, const arma::vec beta2, const int changeind, const int change);
RcppExport SEXP _MMVBVS_betagam_accept_random_c(SEXP XSEXP, SEXP YSEXP, SEXP sigmabeta1SEXP, SEXP inputSigmaSEXP, SEXP VbetaSEXP, SEXP gam1SEXP, SEXP beta1SEXP, SEXP gam2SEXP, SEXP beta2SEXP, SEXP changeindSEXP, SEXP changeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const double >::type sigmabeta1(sigmabeta1SEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type inputSigma(inputSigmaSEXP);
    Rcpp::traits::input_parameter< const double >::type Vbeta(VbetaSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type gam1(gam1SEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type beta1(beta1SEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type gam2(gam2SEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type beta2(beta2SEXP);
    Rcpp::traits::input_parameter< const int >::type changeind(changeindSEXP);
    Rcpp::traits::input_parameter< const int >::type change(changeSEXP);
    rcpp_result_gen = Rcpp::wrap(betagam_accept_random_c(X, Y, sigmabeta1, inputSigma, Vbeta, gam1, beta1, gam2, beta2, changeind, change));
    return rcpp_result_gen;
END_RCPP
}
// update_betagam_random_c
Rcpp::List update_betagam_random_c(const arma::vec X, const arma::mat Y, arma::vec gam1, arma::vec beta1, const arma::mat Sigma, const double sigmabeta, const double Vbeta, const int bgiter, const double smallchange);
RcppExport SEXP _MMVBVS_update_betagam_random_c(SEXP XSEXP, SEXP YSEXP, SEXP gam1SEXP, SEXP beta1SEXP, SEXP SigmaSEXP, SEXP sigmabetaSEXP, SEXP VbetaSEXP, SEXP bgiterSEXP, SEXP smallchangeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gam1(gam1SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta1(beta1SEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Sigma(SigmaSEXP);
    Rcpp::traits::input_parameter< const double >::type sigmabeta(sigmabetaSEXP);
    Rcpp::traits::input_parameter< const double >::type Vbeta(VbetaSEXP);
    Rcpp::traits::input_parameter< const int >::type bgiter(bgiterSEXP);
    Rcpp::traits::input_parameter< const double >::type smallchange(smallchangeSEXP);
    rcpp_result_gen = Rcpp::wrap(update_betagam_random_c(X, Y, gam1, beta1, Sigma, sigmabeta, Vbeta, bgiter, smallchange));
    return rcpp_result_gen;
END_RCPP
}
// get_sigmabeta_from_h_c
double get_sigmabeta_from_h_c(const double h, const arma::vec gam, const arma::mat Sigma, const arma::vec X, const int P);
RcppExport SEXP _MMVBVS_get_sigmabeta_from_h_c(SEXP hSEXP, SEXP gamSEXP, SEXP SigmaSEXP, SEXP XSEXP, SEXP PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Sigma(SigmaSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const int >::type P(PSEXP);
    rcpp_result_gen = Rcpp::wrap(get_sigmabeta_from_h_c(h, gam, Sigma, X, P));
    return rcpp_result_gen;
END_RCPP
}
// get_h_from_sigmabeta_c
double get_h_from_sigmabeta_c(const arma::vec X, const double sigmabeta, const arma::mat Sigma, const arma::vec gam, const int n, const int P);
RcppExport SEXP _MMVBVS_get_h_from_sigmabeta_c(SEXP XSEXP, SEXP sigmabetaSEXP, SEXP SigmaSEXP, SEXP gamSEXP, SEXP nSEXP, SEXP PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const double >::type sigmabeta(sigmabetaSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Sigma(SigmaSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type P(PSEXP);
    rcpp_result_gen = Rcpp::wrap(get_h_from_sigmabeta_c(X, sigmabeta, Sigma, gam, n, P));
    return rcpp_result_gen;
END_RCPP
}
// update_h_c
Rcpp::List update_h_c(const double initialh, const int hiter, const arma::vec gam, const arma::vec beta, const arma::mat Sig, const arma::vec X, int P);
RcppExport SEXP _MMVBVS_update_h_c(SEXP initialhSEXP, SEXP hiterSEXP, SEXP gamSEXP, SEXP betaSEXP, SEXP SigSEXP, SEXP XSEXP, SEXP PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type initialh(initialhSEXP);
    Rcpp::traits::input_parameter< const int >::type hiter(hiterSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Sig(SigSEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type P(PSEXP);
    rcpp_result_gen = Rcpp::wrap(update_h_c(initialh, hiter, gam, beta, Sig, X, P));
    return rcpp_result_gen;
END_RCPP
}
// mmvbvs
Rcpp::List mmvbvs(const arma::vec X, const arma::mat Y, const Rcpp::List initial_chain, const arma::mat Phi, const arma::rowvec marcor, const int niter, const int bgiter, const int hiter, const int burnin, const int Vbeta, const double smallchange, const bool verbose);
RcppExport SEXP _MMVBVS_mmvbvs(SEXP XSEXP, SEXP YSEXP, SEXP initial_chainSEXP, SEXP PhiSEXP, SEXP marcorSEXP, SEXP niterSEXP, SEXP bgiterSEXP, SEXP hiterSEXP, SEXP burninSEXP, SEXP VbetaSEXP, SEXP smallchangeSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List >::type initial_chain(initial_chainSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Phi(PhiSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec >::type marcor(marcorSEXP);
    Rcpp::traits::input_parameter< const int >::type niter(niterSEXP);
    Rcpp::traits::input_parameter< const int >::type bgiter(bgiterSEXP);
    Rcpp::traits::input_parameter< const int >::type hiter(hiterSEXP);
    Rcpp::traits::input_parameter< const int >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< const int >::type Vbeta(VbetaSEXP);
    Rcpp::traits::input_parameter< const double >::type smallchange(smallchangeSEXP);
    Rcpp::traits::input_parameter< const bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(mmvbvs(X, Y, initial_chain, Phi, marcor, niter, bgiter, hiter, burnin, Vbeta, smallchange, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MMVBVS_colmeanNA", (DL_FUNC) &_MMVBVS_colmeanNA, 1},
    {"_MMVBVS_sample_index", (DL_FUNC) &_MMVBVS_sample_index, 2},
    {"_MMVBVS_mvrnormArma", (DL_FUNC) &_MMVBVS_mvrnormArma, 3},
    {"_MMVBVS_dmvnrm_arma", (DL_FUNC) &_MMVBVS_dmvnrm_arma, 4},
    {"_MMVBVS_em_with_zero_mean_c", (DL_FUNC) &_MMVBVS_em_with_zero_mean_c, 2},
    {"_MMVBVS_rinvwish_c", (DL_FUNC) &_MMVBVS_rinvwish_c, 3},
    {"_MMVBVS_get_target_c", (DL_FUNC) &_MMVBVS_get_target_c, 6},
    {"_MMVBVS_update_Sigma_c", (DL_FUNC) &_MMVBVS_update_Sigma_c, 6},
    {"_MMVBVS_update_gamma_random_c", (DL_FUNC) &_MMVBVS_update_gamma_random_c, 3},
    {"_MMVBVS_betagam_accept_random_c", (DL_FUNC) &_MMVBVS_betagam_accept_random_c, 11},
    {"_MMVBVS_update_betagam_random_c", (DL_FUNC) &_MMVBVS_update_betagam_random_c, 9},
    {"_MMVBVS_get_sigmabeta_from_h_c", (DL_FUNC) &_MMVBVS_get_sigmabeta_from_h_c, 5},
    {"_MMVBVS_get_h_from_sigmabeta_c", (DL_FUNC) &_MMVBVS_get_h_from_sigmabeta_c, 6},
    {"_MMVBVS_update_h_c", (DL_FUNC) &_MMVBVS_update_h_c, 7},
    {"_MMVBVS_mmvbvs", (DL_FUNC) &_MMVBVS_mmvbvs, 12},
    {NULL, NULL, 0}
};

RcppExport void R_init_MMVBVS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}