// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// cswei
Eigen::VectorXd cswei(const Eigen::VectorXd& w_v, const Eigen::VectorXd& rs_rs, const Eigen::MatrixXi& ind, const Eigen::VectorXd& rev);
RcppExport SEXP _coxmeg_cswei(SEXP w_vSEXP, SEXP rs_rsSEXP, SEXP indSEXP, SEXP revSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type w_v(w_vSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rs_rs(rs_rsSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXi& >::type ind(indSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rev(revSEXP);
    rcpp_result_gen = Rcpp::wrap(cswei(w_v, rs_rs, ind, rev));
    return rcpp_result_gen;
END_RCPP
}
// invsph
Rcpp::List invsph(Eigen::SparseMatrix<double>& A, const Eigen::VectorXd& der, const Eigen::VectorXd& dv, const Eigen::VectorXd& v1, const Eigen::MatrixXd& mx, const Eigen::VectorXd& v2, const Eigen::VectorXd& v3, const Eigen::MatrixXd& v4, const Eigen::VectorXd& av, const Eigen::VectorXd& bw, const Eigen::VectorXd& f, const Eigen::VectorXd& inv, const Eigen::VectorXd& tau, const int sol);
RcppExport SEXP _coxmeg_invsph(SEXP ASEXP, SEXP derSEXP, SEXP dvSEXP, SEXP v1SEXP, SEXP mxSEXP, SEXP v2SEXP, SEXP v3SEXP, SEXP v4SEXP, SEXP avSEXP, SEXP bwSEXP, SEXP fSEXP, SEXP invSEXP, SEXP tauSEXP, SEXP solSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double>& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type der(derSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type dv(dvSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type mx(mxSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type v2(v2SEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type v3(v3SEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type v4(v4SEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type av(avSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type bw(bwSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type f(fSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type inv(invSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< const int >::type sol(solSEXP);
    rcpp_result_gen = Rcpp::wrap(invsph(A, der, dv, v1, mx, v2, v3, v4, av, bw, f, inv, tau, sol));
    return rcpp_result_gen;
END_RCPP
}
// logdeth
double logdeth(Eigen::SparseMatrix<double>& A, const Eigen::VectorXd& dv, const Eigen::VectorXd& bw_v, const Eigen::VectorXd& w, const Eigen::VectorXd& cs_p, const Eigen::MatrixXi& v4, const Eigen::VectorXd& a, const Eigen::VectorXd& tau, const Eigen::VectorXi& inv, const Eigen::VectorXi& detap);
RcppExport SEXP _coxmeg_logdeth(SEXP ASEXP, SEXP dvSEXP, SEXP bw_vSEXP, SEXP wSEXP, SEXP cs_pSEXP, SEXP v4SEXP, SEXP aSEXP, SEXP tauSEXP, SEXP invSEXP, SEXP detapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double>& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type dv(dvSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type bw_v(bw_vSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type w(wSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type cs_p(cs_pSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXi& >::type v4(v4SEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXi& >::type inv(invSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXi& >::type detap(detapSEXP);
    rcpp_result_gen = Rcpp::wrap(logdeth(A, dv, bw_v, w, cs_p, v4, a, tau, inv, detap));
    return rcpp_result_gen;
END_RCPP
}
// pcg_dense
Eigen::MatrixXd pcg_dense(const Eigen::MatrixXd& A, const Eigen::MatrixXd& B, const double tol);
RcppExport SEXP _coxmeg_pcg_dense(SEXP ASEXP, SEXP BSEXP, SEXP tolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const double >::type tol(tolSEXP);
    rcpp_result_gen = Rcpp::wrap(pcg_dense(A, B, tol));
    return rcpp_result_gen;
END_RCPP
}
// pcg_sparse
Eigen::MatrixXd pcg_sparse(Eigen::SparseMatrix<double>& A, const Eigen::MatrixXd& B, const double tol);
RcppExport SEXP _coxmeg_pcg_sparse(SEXP ASEXP, SEXP BSEXP, SEXP tolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double>& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const double >::type tol(tolSEXP);
    rcpp_result_gen = Rcpp::wrap(pcg_sparse(A, B, tol));
    return rcpp_result_gen;
END_RCPP
}
// rs_sum
Rcpp::List rs_sum(const Eigen::VectorXd& rk_v, const Eigen::VectorXd& d);
RcppExport SEXP _coxmeg_rs_sum(SEXP rk_vSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rk_v(rk_vSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(rs_sum(rk_v, d));
    return rcpp_result_gen;
END_RCPP
}
// csqei
Eigen::MatrixXd csqei(const Eigen::VectorXd& w_v, const Eigen::MatrixXd& mx, const Eigen::VectorXd& rs_rs, const Eigen::VectorXd& rs_cs, const Eigen::MatrixXi& ind, const Eigen::VectorXd& av);
RcppExport SEXP _coxmeg_csqei(SEXP w_vSEXP, SEXP mxSEXP, SEXP rs_rsSEXP, SEXP rs_csSEXP, SEXP indSEXP, SEXP avSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type w_v(w_vSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type mx(mxSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rs_rs(rs_rsSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rs_cs(rs_csSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXi& >::type ind(indSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type av(avSEXP);
    rcpp_result_gen = Rcpp::wrap(csqei(w_v, mx, rs_rs, rs_cs, ind, av));
    return rcpp_result_gen;
END_RCPP
}
// wma_cp
Eigen::MatrixXd wma_cp(const Eigen::VectorXd& w, const Eigen::VectorXd& cs_p, const Eigen::MatrixXi& ind, const Eigen::VectorXd& a);
RcppExport SEXP _coxmeg_wma_cp(SEXP wSEXP, SEXP cs_pSEXP, SEXP indSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type w(wSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type cs_p(cs_pSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXi& >::type ind(indSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(wma_cp(w, cs_p, ind, a));
    return rcpp_result_gen;
END_RCPP
}
// score_test
Eigen::VectorXd score_test(const Eigen::VectorXd& deriv, const Eigen::VectorXd& bw_v, const Eigen::VectorXd& w, const Eigen::VectorXd& rs_rs, const Eigen::VectorXd& rs_cs, const Eigen::VectorXd& cs_p, const Eigen::MatrixXi& ind, const Eigen::VectorXd& a, const Eigen::VectorXd& a2, const Eigen::VectorXd& tau, const Eigen::MatrixXd& v, const Eigen::MatrixXd& cov, const Eigen::MatrixXd& x);
RcppExport SEXP _coxmeg_score_test(SEXP derivSEXP, SEXP bw_vSEXP, SEXP wSEXP, SEXP rs_rsSEXP, SEXP rs_csSEXP, SEXP cs_pSEXP, SEXP indSEXP, SEXP aSEXP, SEXP a2SEXP, SEXP tauSEXP, SEXP vSEXP, SEXP covSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type deriv(derivSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type bw_v(bw_vSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type w(wSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rs_rs(rs_rsSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rs_cs(rs_csSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type cs_p(cs_pSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXi& >::type ind(indSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type a2(a2SEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type v(vSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type cov(covSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(score_test(deriv, bw_v, w, rs_rs, rs_cs, cs_p, ind, a, a2, tau, v, cov, x));
    return rcpp_result_gen;
END_RCPP
}
// logdet_ch
double logdet_ch(const Eigen::MatrixXd& X_m, const Eigen::MatrixXd& rad_m, const Eigen::VectorXd& bma_d, const Eigen::VectorXd& bpa_d, const Eigen::VectorXd& cj_v);
RcppExport SEXP _coxmeg_logdet_ch(SEXP X_mSEXP, SEXP rad_mSEXP, SEXP bma_dSEXP, SEXP bpa_dSEXP, SEXP cj_vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type X_m(X_mSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type rad_m(rad_mSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type bma_d(bma_dSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type bpa_d(bpa_dSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type cj_v(cj_vSEXP);
    rcpp_result_gen = Rcpp::wrap(logdet_ch(X_m, rad_m, bma_d, bpa_d, cj_v));
    return rcpp_result_gen;
END_RCPP
}
// logdet_lanczos
double logdet_lanczos(const Eigen::MatrixXd& X_m, const Eigen::MatrixXd& rad_m, const Eigen::VectorXi& m_d);
RcppExport SEXP _coxmeg_logdet_lanczos(SEXP X_mSEXP, SEXP rad_mSEXP, SEXP m_dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type X_m(X_mSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type rad_m(rad_mSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXi& >::type m_d(m_dSEXP);
    rcpp_result_gen = Rcpp::wrap(logdet_lanczos(X_m, rad_m, m_d));
    return rcpp_result_gen;
END_RCPP
}
// logdet_lanczos_sp
double logdet_lanczos_sp(const Eigen::SparseMatrix<double>& X_m, const Eigen::MatrixXd& rad_m, const Eigen::VectorXi& m_d);
RcppExport SEXP _coxmeg_logdet_lanczos_sp(SEXP X_mSEXP, SEXP rad_mSEXP, SEXP m_dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::SparseMatrix<double>& >::type X_m(X_mSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type rad_m(rad_mSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXi& >::type m_d(m_dSEXP);
    rcpp_result_gen = Rcpp::wrap(logdet_lanczos_sp(X_m, rad_m, m_d));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coxmeg_cswei", (DL_FUNC) &_coxmeg_cswei, 4},
    {"_coxmeg_invsph", (DL_FUNC) &_coxmeg_invsph, 14},
    {"_coxmeg_logdeth", (DL_FUNC) &_coxmeg_logdeth, 10},
    {"_coxmeg_pcg_dense", (DL_FUNC) &_coxmeg_pcg_dense, 3},
    {"_coxmeg_pcg_sparse", (DL_FUNC) &_coxmeg_pcg_sparse, 3},
    {"_coxmeg_rs_sum", (DL_FUNC) &_coxmeg_rs_sum, 2},
    {"_coxmeg_csqei", (DL_FUNC) &_coxmeg_csqei, 6},
    {"_coxmeg_wma_cp", (DL_FUNC) &_coxmeg_wma_cp, 4},
    {"_coxmeg_score_test", (DL_FUNC) &_coxmeg_score_test, 13},
    {"_coxmeg_logdet_ch", (DL_FUNC) &_coxmeg_logdet_ch, 5},
    {"_coxmeg_logdet_lanczos", (DL_FUNC) &_coxmeg_logdet_lanczos, 3},
    {"_coxmeg_logdet_lanczos_sp", (DL_FUNC) &_coxmeg_logdet_lanczos_sp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_coxmeg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
