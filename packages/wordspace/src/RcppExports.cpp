// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// CPP_col_dist_dense
NumericMatrix CPP_col_dist_dense(NumericMatrix x, NumericMatrix y, int metric_code, double param1, bool symmetric);
RcppExport SEXP _wordspace_CPP_col_dist_dense(SEXP xSEXP, SEXP ySEXP, SEXP metric_codeSEXP, SEXP param1SEXP, SEXP symmetricSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type metric_code(metric_codeSEXP);
    Rcpp::traits::input_parameter< double >::type param1(param1SEXP);
    Rcpp::traits::input_parameter< bool >::type symmetric(symmetricSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_col_dist_dense(x, y, metric_code, param1, symmetric));
    return rcpp_result_gen;
END_RCPP
}
// CPP_col_dist_sparse
NumericMatrix CPP_col_dist_sparse(int nc1, IntegerVector xp, IntegerVector xrow, NumericVector x, int nc2, IntegerVector yp, IntegerVector yrow, NumericVector y, int metric_code, double param1, bool symmetric);
RcppExport SEXP _wordspace_CPP_col_dist_sparse(SEXP nc1SEXP, SEXP xpSEXP, SEXP xrowSEXP, SEXP xSEXP, SEXP nc2SEXP, SEXP ypSEXP, SEXP yrowSEXP, SEXP ySEXP, SEXP metric_codeSEXP, SEXP param1SEXP, SEXP symmetricSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nc1(nc1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type xp(xpSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type xrow(xrowSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nc2(nc2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yp(ypSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yrow(yrowSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type metric_code(metric_codeSEXP);
    Rcpp::traits::input_parameter< double >::type param1(param1SEXP);
    Rcpp::traits::input_parameter< bool >::type symmetric(symmetricSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_col_dist_sparse(nc1, xp, xrow, x, nc2, yp, yrow, y, metric_code, param1, symmetric));
    return rcpp_result_gen;
END_RCPP
}
// CPP_get_openmp_threads
DataFrame CPP_get_openmp_threads();
RcppExport SEXP _wordspace_CPP_get_openmp_threads() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(CPP_get_openmp_threads());
    return rcpp_result_gen;
END_RCPP
}
// CPP_set_openmp_threads
void CPP_set_openmp_threads(int n);
RcppExport SEXP _wordspace_CPP_set_openmp_threads(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    CPP_set_openmp_threads(n);
    return R_NilValue;
END_RCPP
}
// CPP_random_indexing_sparse
NumericMatrix CPP_random_indexing_sparse(int nr, int nc, IntegerVector p, IntegerVector row_of, NumericVector x, int n_ri, double rate, bool verbose);
RcppExport SEXP _wordspace_CPP_random_indexing_sparse(SEXP nrSEXP, SEXP ncSEXP, SEXP pSEXP, SEXP row_ofSEXP, SEXP xSEXP, SEXP n_riSEXP, SEXP rateSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nr(nrSEXP);
    Rcpp::traits::input_parameter< int >::type nc(ncSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row_of(row_ofSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n_ri(n_riSEXP);
    Rcpp::traits::input_parameter< double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_random_indexing_sparse(nr, nc, p, row_of, x, n_ri, rate, verbose));
    return rcpp_result_gen;
END_RCPP
}
// CPP_row_norms_dense
NumericVector CPP_row_norms_dense(NumericMatrix x, int norm_code, double p_norm);
RcppExport SEXP _wordspace_CPP_row_norms_dense(SEXP xSEXP, SEXP norm_codeSEXP, SEXP p_normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type norm_code(norm_codeSEXP);
    Rcpp::traits::input_parameter< double >::type p_norm(p_normSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_row_norms_dense(x, norm_code, p_norm));
    return rcpp_result_gen;
END_RCPP
}
// CPP_row_norms_sparse
NumericVector CPP_row_norms_sparse(int nr, int nc, IntegerVector p, IntegerVector row_of, NumericVector x, int norm_code, double p_norm);
RcppExport SEXP _wordspace_CPP_row_norms_sparse(SEXP nrSEXP, SEXP ncSEXP, SEXP pSEXP, SEXP row_ofSEXP, SEXP xSEXP, SEXP norm_codeSEXP, SEXP p_normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nr(nrSEXP);
    Rcpp::traits::input_parameter< int >::type nc(ncSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row_of(row_ofSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type norm_code(norm_codeSEXP);
    Rcpp::traits::input_parameter< double >::type p_norm(p_normSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_row_norms_sparse(nr, nc, p, row_of, x, norm_code, p_norm));
    return rcpp_result_gen;
END_RCPP
}
// CPP_col_norms_dense
NumericVector CPP_col_norms_dense(NumericMatrix x, int norm_code, double p_norm);
RcppExport SEXP _wordspace_CPP_col_norms_dense(SEXP xSEXP, SEXP norm_codeSEXP, SEXP p_normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type norm_code(norm_codeSEXP);
    Rcpp::traits::input_parameter< double >::type p_norm(p_normSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_col_norms_dense(x, norm_code, p_norm));
    return rcpp_result_gen;
END_RCPP
}
// CPP_col_norms_sparse
NumericVector CPP_col_norms_sparse(int nr, int nc, IntegerVector p, IntegerVector row_of, NumericVector x, int norm_code, double p_norm);
RcppExport SEXP _wordspace_CPP_col_norms_sparse(SEXP nrSEXP, SEXP ncSEXP, SEXP pSEXP, SEXP row_ofSEXP, SEXP xSEXP, SEXP norm_codeSEXP, SEXP p_normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nr(nrSEXP);
    Rcpp::traits::input_parameter< int >::type nc(ncSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row_of(row_ofSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type norm_code(norm_codeSEXP);
    Rcpp::traits::input_parameter< double >::type p_norm(p_normSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_col_norms_sparse(nr, nc, p, row_of, x, norm_code, p_norm));
    return rcpp_result_gen;
END_RCPP
}
// CPP_scale_margins_dense
NumericMatrix CPP_scale_margins_dense(NumericMatrix M, NumericVector rows, NumericVector cols, bool duplicate);
RcppExport SEXP _wordspace_CPP_scale_margins_dense(SEXP MSEXP, SEXP rowsSEXP, SEXP colsSEXP, SEXP duplicateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< bool >::type duplicate(duplicateSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_scale_margins_dense(M, rows, cols, duplicate));
    return rcpp_result_gen;
END_RCPP
}
// CPP_scale_margins_sparse
S4 CPP_scale_margins_sparse(S4 M, NumericVector rows, NumericVector cols, bool duplicate);
RcppExport SEXP _wordspace_CPP_scale_margins_sparse(SEXP MSEXP, SEXP rowsSEXP, SEXP colsSEXP, SEXP duplicateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< S4 >::type M(MSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< bool >::type duplicate(duplicateSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_scale_margins_sparse(M, rows, cols, duplicate));
    return rcpp_result_gen;
END_RCPP
}
// CPP_dsm_score_dense
NumericMatrix CPP_dsm_score_dense(NumericMatrix f, NumericVector f1, NumericVector f2, double N, int am_code, int sparse, int transform_code);
RcppExport SEXP _wordspace_CPP_dsm_score_dense(SEXP fSEXP, SEXP f1SEXP, SEXP f2SEXP, SEXP NSEXP, SEXP am_codeSEXP, SEXP sparseSEXP, SEXP transform_codeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type f(fSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f1(f1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f2(f2SEXP);
    Rcpp::traits::input_parameter< double >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type am_code(am_codeSEXP);
    Rcpp::traits::input_parameter< int >::type sparse(sparseSEXP);
    Rcpp::traits::input_parameter< int >::type transform_code(transform_codeSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_dsm_score_dense(f, f1, f2, N, am_code, sparse, transform_code));
    return rcpp_result_gen;
END_RCPP
}
// CPP_dsm_score_sparse
NumericVector CPP_dsm_score_sparse(int nr, int nc, IntegerVector p, IntegerVector row_of, NumericVector f, NumericVector f1, NumericVector f2, double N, int am_code, int sparse, int transform_code);
RcppExport SEXP _wordspace_CPP_dsm_score_sparse(SEXP nrSEXP, SEXP ncSEXP, SEXP pSEXP, SEXP row_ofSEXP, SEXP fSEXP, SEXP f1SEXP, SEXP f2SEXP, SEXP NSEXP, SEXP am_codeSEXP, SEXP sparseSEXP, SEXP transform_codeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nr(nrSEXP);
    Rcpp::traits::input_parameter< int >::type nc(ncSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row_of(row_ofSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f(fSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f1(f1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f2(f2SEXP);
    Rcpp::traits::input_parameter< double >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type am_code(am_codeSEXP);
    Rcpp::traits::input_parameter< int >::type sparse(sparseSEXP);
    Rcpp::traits::input_parameter< int >::type transform_code(transform_codeSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_dsm_score_sparse(nr, nc, p, row_of, f, f1, f2, N, am_code, sparse, transform_code));
    return rcpp_result_gen;
END_RCPP
}
// CPP_similarity_to_distance
NumericMatrix CPP_similarity_to_distance(NumericMatrix M, int opcode, double tol, bool duplicate);
RcppExport SEXP _wordspace_CPP_similarity_to_distance(SEXP MSEXP, SEXP opcodeSEXP, SEXP tolSEXP, SEXP duplicateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type opcode(opcodeSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< bool >::type duplicate(duplicateSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_similarity_to_distance(M, opcode, tol, duplicate));
    return rcpp_result_gen;
END_RCPP
}
// CPP_signcount
NumericVector CPP_signcount(NumericVector x);
RcppExport SEXP _wordspace_CPP_signcount(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_signcount(x));
    return rcpp_result_gen;
END_RCPP
}
// CPP_signcount_int
NumericVector CPP_signcount_int(IntegerVector x);
RcppExport SEXP _wordspace_CPP_signcount_int(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(CPP_signcount_int(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_wordspace_CPP_col_dist_dense", (DL_FUNC) &_wordspace_CPP_col_dist_dense, 5},
    {"_wordspace_CPP_col_dist_sparse", (DL_FUNC) &_wordspace_CPP_col_dist_sparse, 11},
    {"_wordspace_CPP_get_openmp_threads", (DL_FUNC) &_wordspace_CPP_get_openmp_threads, 0},
    {"_wordspace_CPP_set_openmp_threads", (DL_FUNC) &_wordspace_CPP_set_openmp_threads, 1},
    {"_wordspace_CPP_random_indexing_sparse", (DL_FUNC) &_wordspace_CPP_random_indexing_sparse, 8},
    {"_wordspace_CPP_row_norms_dense", (DL_FUNC) &_wordspace_CPP_row_norms_dense, 3},
    {"_wordspace_CPP_row_norms_sparse", (DL_FUNC) &_wordspace_CPP_row_norms_sparse, 7},
    {"_wordspace_CPP_col_norms_dense", (DL_FUNC) &_wordspace_CPP_col_norms_dense, 3},
    {"_wordspace_CPP_col_norms_sparse", (DL_FUNC) &_wordspace_CPP_col_norms_sparse, 7},
    {"_wordspace_CPP_scale_margins_dense", (DL_FUNC) &_wordspace_CPP_scale_margins_dense, 4},
    {"_wordspace_CPP_scale_margins_sparse", (DL_FUNC) &_wordspace_CPP_scale_margins_sparse, 4},
    {"_wordspace_CPP_dsm_score_dense", (DL_FUNC) &_wordspace_CPP_dsm_score_dense, 7},
    {"_wordspace_CPP_dsm_score_sparse", (DL_FUNC) &_wordspace_CPP_dsm_score_sparse, 11},
    {"_wordspace_CPP_similarity_to_distance", (DL_FUNC) &_wordspace_CPP_similarity_to_distance, 4},
    {"_wordspace_CPP_signcount", (DL_FUNC) &_wordspace_CPP_signcount, 1},
    {"_wordspace_CPP_signcount_int", (DL_FUNC) &_wordspace_CPP_signcount_int, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_wordspace(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}