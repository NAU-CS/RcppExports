// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rowMinsC
arma::vec rowMinsC(arma::mat x);
RcppExport SEXP _GEEaSPU_rowMinsC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMinsC(x));
    return rcpp_result_gen;
END_RCPP
}
// count_if
double count_if(arma::uvec x);
RcppExport SEXP _GEEaSPU_count_if(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::uvec >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(count_if(x));
    return rcpp_result_gen;
END_RCPP
}
// rankC
arma::vec rankC(NumericVector x);
RcppExport SEXP _GEEaSPU_rankC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rankC(x));
    return rcpp_result_gen;
END_RCPP
}
// which_min
NumericVector which_min(NumericVector y, LogicalVector x);
RcppExport SEXP _GEEaSPU_which_min(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_min(y, x));
    return rcpp_result_gen;
END_RCPP
}
// getEigen
List getEigen(arma::mat sigma);
RcppExport SEXP _GEEaSPU_getEigen(SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(getEigen(sigma));
    return rcpp_result_gen;
END_RCPP
}
// a8
arma::mat a8(arma::mat U, int p, int k);
RcppExport SEXP _GEEaSPU_a8(SEXP USEXP, SEXP pSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type U(USEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(a8(U, p, k));
    return rcpp_result_gen;
END_RCPP
}
// signC
double signC(double x);
RcppExport SEXP _GEEaSPU_signC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(signC(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_standPow
arma::vec rcpp_standPow(arma::mat U1, double power);
RcppExport SEXP _GEEaSPU_rcpp_standPow(SEXP U1SEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type U1(U1SEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_standPow(U1, power));
    return rcpp_result_gen;
END_RCPP
}
// InfU
arma::vec InfU(arma::mat U);
RcppExport SEXP _GEEaSPU_InfU(SEXP USEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type U(USEXP);
    rcpp_result_gen = Rcpp::wrap(InfU(U));
    return rcpp_result_gen;
END_RCPP
}
// gauss_score
arma::mat gauss_score(arma::mat invR, arma::mat G, arma::mat res, int n, int k, int p);
RcppExport SEXP _GEEaSPU_gauss_score(SEXP invRSEXP, SEXP GSEXP, SEXP resSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(gauss_score(invR, G, res, n, k, p));
    return rcpp_result_gen;
END_RCPP
}
// gauss_score_cov
List gauss_score_cov(arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp);
RcppExport SEXP _GEEaSPU_gauss_score_cov(SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    rcpp_result_gen = Rcpp::wrap(gauss_score_cov(invR, G, x, res, covres, n, k, p, tp));
    return rcpp_result_gen;
END_RCPP
}
// bin_score
arma::mat bin_score(arma::vec va, arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp);
RcppExport SEXP _GEEaSPU_bin_score(SEXP vaSEXP, SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type va(vaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    rcpp_result_gen = Rcpp::wrap(bin_score(va, invR, G, x, res, covres, n, k, p, tp));
    return rcpp_result_gen;
END_RCPP
}
// bin_score_cov
List bin_score_cov(arma::vec va, arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp);
RcppExport SEXP _GEEaSPU_bin_score_cov(SEXP vaSEXP, SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type va(vaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    rcpp_result_gen = Rcpp::wrap(bin_score_cov(va, invR, G, x, res, covres, n, k, p, tp));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_spuval
arma::mat rcpp_spuval(arma::mat U, arma::mat po, arma::mat po2, int p, int k);
RcppExport SEXP _GEEaSPU_rcpp_spuval(SEXP USEXP, SEXP poSEXP, SEXP po2SEXP, SEXP pSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type U(USEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_spuval(U, po, po2, p, k));
    return rcpp_result_gen;
END_RCPP
}
// perm
List perm(arma::mat invR, arma::mat G, arma::mat res, int n, int k, int p, arma::mat po, arma::mat po2, int nperm);
RcppExport SEXP _GEEaSPU_perm(SEXP invRSEXP, SEXP GSEXP, SEXP resSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP npermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nperm(npermSEXP);
    rcpp_result_gen = Rcpp::wrap(perm(invR, G, res, n, k, p, po, po2, nperm));
    return rcpp_result_gen;
END_RCPP
}
// perm_score
List perm_score(arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp, arma::mat po, arma::mat po2, int nperm);
RcppExport SEXP _GEEaSPU_perm_score(SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP npermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nperm(npermSEXP);
    rcpp_result_gen = Rcpp::wrap(perm_score(invR, G, x, res, covres, n, k, p, tp, po, po2, nperm));
    return rcpp_result_gen;
END_RCPP
}
// permhigh
List permhigh(arma::mat invR, arma::mat G, arma::mat res, int n, int k, int p, arma::mat po, arma::mat po2, int nperm);
RcppExport SEXP _GEEaSPU_permhigh(SEXP invRSEXP, SEXP GSEXP, SEXP resSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP npermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nperm(npermSEXP);
    rcpp_result_gen = Rcpp::wrap(permhigh(invR, G, res, n, k, p, po, po2, nperm));
    return rcpp_result_gen;
END_RCPP
}
// permhigh_score
List permhigh_score(arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp, arma::mat po, arma::mat po2, int nperm);
RcppExport SEXP _GEEaSPU_permhigh_score(SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP npermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nperm(npermSEXP);
    rcpp_result_gen = Rcpp::wrap(permhigh_score(invR, G, x, res, covres, n, k, p, tp, po, po2, nperm));
    return rcpp_result_gen;
END_RCPP
}
// sim
List sim(int f, arma::vec va, arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp, arma::mat po, arma::mat po2, int nsim);
RcppExport SEXP _GEEaSPU_sim(SEXP fSEXP, SEXP vaSEXP, SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP nsimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type f(fSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type va(vaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nsim(nsimSEXP);
    rcpp_result_gen = Rcpp::wrap(sim(f, va, invR, G, x, res, covres, n, k, p, tp, po, po2, nsim));
    return rcpp_result_gen;
END_RCPP
}
// sim_score
List sim_score(int f, arma::vec va, arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp, arma::mat po, arma::mat po2, int nsim);
RcppExport SEXP _GEEaSPU_sim_score(SEXP fSEXP, SEXP vaSEXP, SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP nsimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type f(fSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type va(vaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nsim(nsimSEXP);
    rcpp_result_gen = Rcpp::wrap(sim_score(f, va, invR, G, x, res, covres, n, k, p, tp, po, po2, nsim));
    return rcpp_result_gen;
END_RCPP
}
// simhigh
List simhigh(int f, arma::vec va, arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp, arma::mat po, arma::mat po2, int nperm);
RcppExport SEXP _GEEaSPU_simhigh(SEXP fSEXP, SEXP vaSEXP, SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP npermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type f(fSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type va(vaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nperm(npermSEXP);
    rcpp_result_gen = Rcpp::wrap(simhigh(f, va, invR, G, x, res, covres, n, k, p, tp, po, po2, nperm));
    return rcpp_result_gen;
END_RCPP
}
// simhigh_score
List simhigh_score(int f, arma::vec va, arma::mat invR, arma::mat G, arma::mat x, arma::mat res, arma::mat covres, int n, int k, int p, int tp, arma::mat po, arma::mat po2, int nperm);
RcppExport SEXP _GEEaSPU_simhigh_score(SEXP fSEXP, SEXP vaSEXP, SEXP invRSEXP, SEXP GSEXP, SEXP xSEXP, SEXP resSEXP, SEXP covresSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP tpSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP npermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type f(fSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type va(vaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covres(covresSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type tp(tpSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< int >::type nperm(npermSEXP);
    rcpp_result_gen = Rcpp::wrap(simhigh_score(f, va, invR, G, x, res, covres, n, k, p, tp, po, po2, nperm));
    return rcpp_result_gen;
END_RCPP
}
// pathval
arma::mat pathval(arma::mat out, int k, arma::vec nSNPs, int nGenes, arma::mat po, arma::mat po2, arma::mat po3);
RcppExport SEXP _GEEaSPU_pathval(SEXP outSEXP, SEXP kSEXP, SEXP nSNPsSEXP, SEXP nGenesSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP po3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type out(outSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type nSNPs(nSNPsSEXP);
    Rcpp::traits::input_parameter< int >::type nGenes(nGenesSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po3(po3SEXP);
    rcpp_result_gen = Rcpp::wrap(pathval(out, k, nSNPs, nGenes, po, po2, po3));
    return rcpp_result_gen;
END_RCPP
}
// permpath
List permpath(arma::mat invR, arma::mat G, arma::mat res, arma::vec nSNPs, int nGenes, int n, int k, int p, arma::mat po, arma::mat po2, arma::mat po3, int nperm);
RcppExport SEXP _GEEaSPU_permpath(SEXP invRSEXP, SEXP GSEXP, SEXP resSEXP, SEXP nSNPsSEXP, SEXP nGenesSEXP, SEXP nSEXP, SEXP kSEXP, SEXP pSEXP, SEXP poSEXP, SEXP po2SEXP, SEXP po3SEXP, SEXP npermSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type invR(invRSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G(GSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type res(resSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type nSNPs(nSNPsSEXP);
    Rcpp::traits::input_parameter< int >::type nGenes(nGenesSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po(poSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po2(po2SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type po3(po3SEXP);
    Rcpp::traits::input_parameter< int >::type nperm(npermSEXP);
    rcpp_result_gen = Rcpp::wrap(permpath(invR, G, res, nSNPs, nGenes, n, k, p, po, po2, po3, nperm));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_GEEaSPU_rowMinsC", (DL_FUNC) &_GEEaSPU_rowMinsC, 1},
    {"_GEEaSPU_count_if", (DL_FUNC) &_GEEaSPU_count_if, 1},
    {"_GEEaSPU_rankC", (DL_FUNC) &_GEEaSPU_rankC, 1},
    {"_GEEaSPU_which_min", (DL_FUNC) &_GEEaSPU_which_min, 2},
    {"_GEEaSPU_getEigen", (DL_FUNC) &_GEEaSPU_getEigen, 1},
    {"_GEEaSPU_a8", (DL_FUNC) &_GEEaSPU_a8, 3},
    {"_GEEaSPU_signC", (DL_FUNC) &_GEEaSPU_signC, 1},
    {"_GEEaSPU_rcpp_standPow", (DL_FUNC) &_GEEaSPU_rcpp_standPow, 2},
    {"_GEEaSPU_InfU", (DL_FUNC) &_GEEaSPU_InfU, 1},
    {"_GEEaSPU_gauss_score", (DL_FUNC) &_GEEaSPU_gauss_score, 6},
    {"_GEEaSPU_gauss_score_cov", (DL_FUNC) &_GEEaSPU_gauss_score_cov, 9},
    {"_GEEaSPU_bin_score", (DL_FUNC) &_GEEaSPU_bin_score, 10},
    {"_GEEaSPU_bin_score_cov", (DL_FUNC) &_GEEaSPU_bin_score_cov, 10},
    {"_GEEaSPU_rcpp_spuval", (DL_FUNC) &_GEEaSPU_rcpp_spuval, 5},
    {"_GEEaSPU_perm", (DL_FUNC) &_GEEaSPU_perm, 9},
    {"_GEEaSPU_perm_score", (DL_FUNC) &_GEEaSPU_perm_score, 12},
    {"_GEEaSPU_permhigh", (DL_FUNC) &_GEEaSPU_permhigh, 9},
    {"_GEEaSPU_permhigh_score", (DL_FUNC) &_GEEaSPU_permhigh_score, 12},
    {"_GEEaSPU_sim", (DL_FUNC) &_GEEaSPU_sim, 14},
    {"_GEEaSPU_sim_score", (DL_FUNC) &_GEEaSPU_sim_score, 14},
    {"_GEEaSPU_simhigh", (DL_FUNC) &_GEEaSPU_simhigh, 14},
    {"_GEEaSPU_simhigh_score", (DL_FUNC) &_GEEaSPU_simhigh_score, 14},
    {"_GEEaSPU_pathval", (DL_FUNC) &_GEEaSPU_pathval, 7},
    {"_GEEaSPU_permpath", (DL_FUNC) &_GEEaSPU_permpath, 12},
    {NULL, NULL, 0}
};

RcppExport void R_init_GEEaSPU(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
