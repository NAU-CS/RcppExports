// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// CtoBp
NumericMatrix CtoBp(NumericMatrix& D, double by, int num_proc);
RcppExport SEXP _minimaxdesign_CtoBp(SEXP DSEXP, SEXP bySEXP, SEXP num_procSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type num_proc(num_procSEXP);
    rcpp_result_gen = Rcpp::wrap(CtoBp(D, by, num_proc));
    return rcpp_result_gen;
END_RCPP
}
// printBar
void printBar(double prop);
RcppExport SEXP _minimaxdesign_printBar(SEXP propSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type prop(propSEXP);
    printBar(prop);
    return R_NilValue;
END_RCPP
}
// kmeansreg
NumericMatrix kmeansreg(NumericMatrix& Rcpp_point, NumericMatrix& Rcpp_cluster_center, double p, double pw, int it_max, double inn_tol, int num_proc);
RcppExport SEXP _minimaxdesign_kmeansreg(SEXP Rcpp_pointSEXP, SEXP Rcpp_cluster_centerSEXP, SEXP pSEXP, SEXP pwSEXP, SEXP it_maxSEXP, SEXP inn_tolSEXP, SEXP num_procSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_point(Rcpp_pointSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_cluster_center(Rcpp_cluster_centerSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type pw(pwSEXP);
    Rcpp::traits::input_parameter< int >::type it_max(it_maxSEXP);
    Rcpp::traits::input_parameter< double >::type inn_tol(inn_tolSEXP);
    Rcpp::traits::input_parameter< int >::type num_proc(num_procSEXP);
    rcpp_result_gen = Rcpp::wrap(kmeansreg(Rcpp_point, Rcpp_cluster_center, p, pw, it_max, inn_tol, num_proc));
    return rcpp_result_gen;
END_RCPP
}
// kmeanspso
List kmeanspso(NumericMatrix& Rcpp_point, NumericMatrix& Rcpp_evalpts, NumericMatrix& Rcpp_cluster_center, double p, double pw, double w, double c1, double c2, int mM_part_num, int it_max, int mM_it_max, int it_lim, int mM_it_lim, double it_tol, double mM_it_tol, double inn_tol, int inn_itmax, int num_proc, double tol, double lb, double ub);
RcppExport SEXP _minimaxdesign_kmeanspso(SEXP Rcpp_pointSEXP, SEXP Rcpp_evalptsSEXP, SEXP Rcpp_cluster_centerSEXP, SEXP pSEXP, SEXP pwSEXP, SEXP wSEXP, SEXP c1SEXP, SEXP c2SEXP, SEXP mM_part_numSEXP, SEXP it_maxSEXP, SEXP mM_it_maxSEXP, SEXP it_limSEXP, SEXP mM_it_limSEXP, SEXP it_tolSEXP, SEXP mM_it_tolSEXP, SEXP inn_tolSEXP, SEXP inn_itmaxSEXP, SEXP num_procSEXP, SEXP tolSEXP, SEXP lbSEXP, SEXP ubSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_point(Rcpp_pointSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_evalpts(Rcpp_evalptsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_cluster_center(Rcpp_cluster_centerSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type pw(pwSEXP);
    Rcpp::traits::input_parameter< double >::type w(wSEXP);
    Rcpp::traits::input_parameter< double >::type c1(c1SEXP);
    Rcpp::traits::input_parameter< double >::type c2(c2SEXP);
    Rcpp::traits::input_parameter< int >::type mM_part_num(mM_part_numSEXP);
    Rcpp::traits::input_parameter< int >::type it_max(it_maxSEXP);
    Rcpp::traits::input_parameter< int >::type mM_it_max(mM_it_maxSEXP);
    Rcpp::traits::input_parameter< int >::type it_lim(it_limSEXP);
    Rcpp::traits::input_parameter< int >::type mM_it_lim(mM_it_limSEXP);
    Rcpp::traits::input_parameter< double >::type it_tol(it_tolSEXP);
    Rcpp::traits::input_parameter< double >::type mM_it_tol(mM_it_tolSEXP);
    Rcpp::traits::input_parameter< double >::type inn_tol(inn_tolSEXP);
    Rcpp::traits::input_parameter< int >::type inn_itmax(inn_itmaxSEXP);
    Rcpp::traits::input_parameter< int >::type num_proc(num_procSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< double >::type lb(lbSEXP);
    Rcpp::traits::input_parameter< double >::type ub(ubSEXP);
    rcpp_result_gen = Rcpp::wrap(kmeanspso(Rcpp_point, Rcpp_evalpts, Rcpp_cluster_center, p, pw, w, c1, c2, mM_part_num, it_max, mM_it_max, it_lim, mM_it_lim, it_tol, mM_it_tol, inn_tol, inn_itmax, num_proc, tol, lb, ub));
    return rcpp_result_gen;
END_RCPP
}
// mMcritPt
List mMcritPt(NumericMatrix& Rcpp_point, NumericMatrix& Rcpp_evalpts);
RcppExport SEXP _minimaxdesign_mMcritPt(SEXP Rcpp_pointSEXP, SEXP Rcpp_evalptsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_point(Rcpp_pointSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_evalpts(Rcpp_evalptsSEXP);
    rcpp_result_gen = Rcpp::wrap(mMcritPt(Rcpp_point, Rcpp_evalpts));
    return rcpp_result_gen;
END_RCPP
}
// mMcrit_allpts
NumericVector mMcrit_allpts(NumericMatrix& Rcpp_point, NumericMatrix& Rcpp_evalpts);
RcppExport SEXP _minimaxdesign_mMcrit_allpts(SEXP Rcpp_pointSEXP, SEXP Rcpp_evalptsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_point(Rcpp_pointSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_evalpts(Rcpp_evalptsSEXP);
    rcpp_result_gen = Rcpp::wrap(mMcrit_allpts(Rcpp_point, Rcpp_evalpts));
    return rcpp_result_gen;
END_RCPP
}
// mMcrit_proj
double mMcrit_proj(NumericMatrix& Rcpp_pts, NumericMatrix& Rcpp_evalpts, NumericMatrix& indices);
RcppExport SEXP _minimaxdesign_mMcrit_proj(SEXP Rcpp_ptsSEXP, SEXP Rcpp_evalptsSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_pts(Rcpp_ptsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_evalpts(Rcpp_evalptsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(mMcrit_proj(Rcpp_pts, Rcpp_evalpts, indices));
    return rcpp_result_gen;
END_RCPP
}
// avgcrit_proj
double avgcrit_proj(NumericMatrix& Rcpp_pts, NumericMatrix& Rcpp_evalpts, NumericMatrix& indices);
RcppExport SEXP _minimaxdesign_avgcrit_proj(SEXP Rcpp_ptsSEXP, SEXP Rcpp_evalptsSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_pts(Rcpp_ptsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_evalpts(Rcpp_evalptsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(avgcrit_proj(Rcpp_pts, Rcpp_evalpts, indices));
    return rcpp_result_gen;
END_RCPP
}
// kmeansobj
double kmeansobj(NumericMatrix& Rcpp_point, NumericMatrix& Rcpp_evalpts, int p);
RcppExport SEXP _minimaxdesign_kmeansobj(SEXP Rcpp_pointSEXP, SEXP Rcpp_evalptsSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_point(Rcpp_pointSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type Rcpp_evalpts(Rcpp_evalptsSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(kmeansobj(Rcpp_point, Rcpp_evalpts, p));
    return rcpp_result_gen;
END_RCPP
}
// CtoAA
NumericMatrix CtoAA(NumericMatrix& D, double by, int num_proc);
RcppExport SEXP _minimaxdesign_CtoAA(SEXP DSEXP, SEXP bySEXP, SEXP num_procSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type num_proc(num_procSEXP);
    rcpp_result_gen = Rcpp::wrap(CtoAA(D, by, num_proc));
    return rcpp_result_gen;
END_RCPP
}
// CtoB2
NumericMatrix CtoB2(NumericMatrix& D, double by, int num_proc);
RcppExport SEXP _minimaxdesign_CtoB2(SEXP DSEXP, SEXP bySEXP, SEXP num_procSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type num_proc(num_procSEXP);
    rcpp_result_gen = Rcpp::wrap(CtoB2(D, by, num_proc));
    return rcpp_result_gen;
END_RCPP
}
// closestPt
NumericMatrix closestPt(NumericMatrix& points, NumericMatrix& grd);
RcppExport SEXP _minimaxdesign_closestPt(SEXP pointsSEXP, SEXP grdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type grd(grdSEXP);
    rcpp_result_gen = Rcpp::wrap(closestPt(points, grd));
    return rcpp_result_gen;
END_RCPP
}
