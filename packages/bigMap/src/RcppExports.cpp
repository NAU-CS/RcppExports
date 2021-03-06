// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// grid_init
arma::Mat<double> grid_init(arma::Col<double> X, arma::Col<double> Y);
RcppExport SEXP _bigMap_grid_init(SEXP XSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Col<double> >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::Col<double> >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_init(X, Y));
    return rcpp_result_gen;
END_RCPP
}
// grid_p2cell
arma::Col<int> grid_p2cell(double x, double y, arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_p2cell(SEXP xSEXP, SEXP ySEXP, SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_p2cell(x, y, grid));
    return rcpp_result_gen;
END_RCPP
}
// grid_D2cell
arma::Mat<double> grid_D2cell(arma::Mat<double> D, arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_D2cell(SEXP DSEXP, SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<double> >::type D(DSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_D2cell(D, grid));
    return rcpp_result_gen;
END_RCPP
}
// grid_n2cell
arma::Col<int> grid_n2cell(int n, arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_n2cell(SEXP nSEXP, SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_n2cell(n, grid));
    return rcpp_result_gen;
END_RCPP
}
// grid_N2cell
arma::Mat<int> grid_N2cell(arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_N2cell(SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_N2cell(grid));
    return rcpp_result_gen;
END_RCPP
}
// grid_M2cell
arma::Mat<int> grid_M2cell(arma::Col<int> M, arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_M2cell(SEXP MSEXP, SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Col<int> >::type M(MSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_M2cell(M, grid));
    return rcpp_result_gen;
END_RCPP
}
// grid_bound
arma::Col<int> grid_bound(int n, arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_bound(SEXP nSEXP, SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_bound(n, grid));
    return rcpp_result_gen;
END_RCPP
}
// grid_cross
arma::Col<int> grid_cross(int n, arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_cross(SEXP nSEXP, SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_cross(n, grid));
    return rcpp_result_gen;
END_RCPP
}
// grid_peaks
arma::Col<int> grid_peaks(arma::Mat<double> Z, arma::Mat<double> grid);
RcppExport SEXP _bigMap_grid_peaks(SEXP ZSEXP, SEXP gridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<double> >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type grid(gridSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_peaks(Z, grid));
    return rcpp_result_gen;
END_RCPP
}
// wtt_cpp
Rcpp::List wtt_cpp(arma::Col<double> X, arma::Col<double> Y, arma::Mat<double> Z);
RcppExport SEXP _bigMap_wtt_cpp(SEXP XSEXP, SEXP YSEXP, SEXP ZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Col<double> >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::Col<double> >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double> >::type Z(ZSEXP);
    rcpp_result_gen = Rcpp::wrap(wtt_cpp(X, Y, Z));
    return rcpp_result_gen;
END_RCPP
}
// distk
arma::Col<double> distk(int k, SEXP X, bool is_distance);
RcppExport SEXP _bigMap_distk(SEXP kSEXP, SEXP XSEXP, SEXP is_distanceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< bool >::type is_distance(is_distanceSEXP);
    rcpp_result_gen = Rcpp::wrap(distk(k, X, is_distance));
    return rcpp_result_gen;
END_RCPP
}
// zBeta
arma::Col<double> zBeta(int thread_rank, int threads, SEXP X, bool is_distance, double ppx, double tol, int mxI);
RcppExport SEXP _bigMap_zBeta(SEXP thread_rankSEXP, SEXP threadsSEXP, SEXP XSEXP, SEXP is_distanceSEXP, SEXP ppxSEXP, SEXP tolSEXP, SEXP mxISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type thread_rank(thread_rankSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< bool >::type is_distance(is_distanceSEXP);
    Rcpp::traits::input_parameter< double >::type ppx(ppxSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type mxI(mxISEXP);
    rcpp_result_gen = Rcpp::wrap(zBeta(thread_rank, threads, X, is_distance, ppx, tol, mxI));
    return rcpp_result_gen;
END_RCPP
}
// sckt_zTSNE
double sckt_zTSNE(int thread_rank, int threads, int layers, SEXP X, SEXP B, SEXP Y, SEXP I, double iters, double alpha, bool isDistance);
RcppExport SEXP _bigMap_sckt_zTSNE(SEXP thread_rankSEXP, SEXP threadsSEXP, SEXP layersSEXP, SEXP XSEXP, SEXP BSEXP, SEXP YSEXP, SEXP ISEXP, SEXP itersSEXP, SEXP alphaSEXP, SEXP isDistanceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type thread_rank(thread_rankSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< int >::type layers(layersSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type B(BSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type I(ISEXP);
    Rcpp::traits::input_parameter< double >::type iters(itersSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< bool >::type isDistance(isDistanceSEXP);
    rcpp_result_gen = Rcpp::wrap(sckt_zTSNE(thread_rank, threads, layers, X, B, Y, I, iters, alpha, isDistance));
    return rcpp_result_gen;
END_RCPP
}
// zChnks
void zChnks(Rcpp::List& Z_list, const arma::Mat<double>& Y, const arma::Col<int>& I, const Rcpp::List& brks_list);
RcppExport SEXP _bigMap_zChnks(SEXP Z_listSEXP, SEXP YSEXP, SEXP ISEXP, SEXP brks_listSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type Z_list(Z_listSEXP);
    Rcpp::traits::input_parameter< const arma::Mat<double>& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::Col<int>& >::type I(ISEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type brks_list(brks_listSEXP);
    zChnks(Z_list, Y, I, brks_list);
    return R_NilValue;
END_RCPP
}
// updateY
void updateY(arma::Mat<double>& Y, const arma::Col<int>& I, const Rcpp::List& zMap_list, const Rcpp::List& brks_list);
RcppExport SEXP _bigMap_updateY(SEXP YSEXP, SEXP ISEXP, SEXP zMap_listSEXP, SEXP brks_listSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<double>& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::Col<int>& >::type I(ISEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type zMap_list(zMap_listSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type brks_list(brks_listSEXP);
    updateY(Y, I, zMap_list, brks_list);
    return R_NilValue;
END_RCPP
}
// eSize
arma::Col<double> eSize(arma::Mat<double>& Y);
RcppExport SEXP _bigMap_eSize(SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<double>& >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(eSize(Y));
    return rcpp_result_gen;
END_RCPP
}
// mpi_zTSNE
double mpi_zTSNE(SEXP X, SEXP B, arma::Mat<double>& Y, const arma::Col<int>& I, double iters, double alpha, bool isDistance);
RcppExport SEXP _bigMap_mpi_zTSNE(SEXP XSEXP, SEXP BSEXP, SEXP YSEXP, SEXP ISEXP, SEXP itersSEXP, SEXP alphaSEXP, SEXP isDistanceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::Mat<double>& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::Col<int>& >::type I(ISEXP);
    Rcpp::traits::input_parameter< double >::type iters(itersSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< bool >::type isDistance(isDistanceSEXP);
    rcpp_result_gen = Rcpp::wrap(mpi_zTSNE(X, B, Y, I, iters, alpha, isDistance));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_bigMap_grid_init", (DL_FUNC) &_bigMap_grid_init, 2},
    {"_bigMap_grid_p2cell", (DL_FUNC) &_bigMap_grid_p2cell, 3},
    {"_bigMap_grid_D2cell", (DL_FUNC) &_bigMap_grid_D2cell, 2},
    {"_bigMap_grid_n2cell", (DL_FUNC) &_bigMap_grid_n2cell, 2},
    {"_bigMap_grid_N2cell", (DL_FUNC) &_bigMap_grid_N2cell, 1},
    {"_bigMap_grid_M2cell", (DL_FUNC) &_bigMap_grid_M2cell, 2},
    {"_bigMap_grid_bound", (DL_FUNC) &_bigMap_grid_bound, 2},
    {"_bigMap_grid_cross", (DL_FUNC) &_bigMap_grid_cross, 2},
    {"_bigMap_grid_peaks", (DL_FUNC) &_bigMap_grid_peaks, 2},
    {"_bigMap_wtt_cpp", (DL_FUNC) &_bigMap_wtt_cpp, 3},
    {"_bigMap_distk", (DL_FUNC) &_bigMap_distk, 3},
    {"_bigMap_zBeta", (DL_FUNC) &_bigMap_zBeta, 7},
    {"_bigMap_sckt_zTSNE", (DL_FUNC) &_bigMap_sckt_zTSNE, 10},
    {"_bigMap_zChnks", (DL_FUNC) &_bigMap_zChnks, 4},
    {"_bigMap_updateY", (DL_FUNC) &_bigMap_updateY, 4},
    {"_bigMap_eSize", (DL_FUNC) &_bigMap_eSize, 1},
    {"_bigMap_mpi_zTSNE", (DL_FUNC) &_bigMap_mpi_zTSNE, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_bigMap(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
