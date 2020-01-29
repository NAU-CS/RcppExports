// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// Rcpp_COMP_Z
SEXP Rcpp_COMP_Z(int moment, double nu, double lambda, int maxn);
RcppExport SEXP _spaMM_Rcpp_COMP_Z(SEXP momentSEXP, SEXP nuSEXP, SEXP lambdaSEXP, SEXP maxnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type moment(momentSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type maxn(maxnSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_COMP_Z(moment, nu, lambda, maxn));
    return rcpp_result_gen;
END_RCPP
}
// lmwith_sparse_LDLp
SEXP lmwith_sparse_LDLp(SEXP XX, SEXP yy, bool returntQ, bool returnR, bool pivot);
RcppExport SEXP _spaMM_lmwith_sparse_LDLp(SEXP XXSEXP, SEXP yySEXP, SEXP returntQSEXP, SEXP returnRSEXP, SEXP pivotSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type XX(XXSEXP);
    Rcpp::traits::input_parameter< SEXP >::type yy(yySEXP);
    Rcpp::traits::input_parameter< bool >::type returntQ(returntQSEXP);
    Rcpp::traits::input_parameter< bool >::type returnR(returnRSEXP);
    Rcpp::traits::input_parameter< bool >::type pivot(pivotSEXP);
    rcpp_result_gen = Rcpp::wrap(lmwith_sparse_LDLp(XX, yy, returntQ, returnR, pivot));
    return rcpp_result_gen;
END_RCPP
}
// lmwith_sparse_LLp
SEXP lmwith_sparse_LLp(SEXP XX, SEXP yy, bool returntQ, bool returnR, bool pivot);
RcppExport SEXP _spaMM_lmwith_sparse_LLp(SEXP XXSEXP, SEXP yySEXP, SEXP returntQSEXP, SEXP returnRSEXP, SEXP pivotSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type XX(XXSEXP);
    Rcpp::traits::input_parameter< SEXP >::type yy(yySEXP);
    Rcpp::traits::input_parameter< bool >::type returntQ(returntQSEXP);
    Rcpp::traits::input_parameter< bool >::type returnR(returnRSEXP);
    Rcpp::traits::input_parameter< bool >::type pivot(pivotSEXP);
    rcpp_result_gen = Rcpp::wrap(lmwith_sparse_LLp(XX, yy, returntQ, returnR, pivot));
    return rcpp_result_gen;
END_RCPP
}
// lmwithQR
SEXP lmwithQR(SEXP XX, SEXP yy, bool returntQ, bool returnR);
RcppExport SEXP _spaMM_lmwithQR(SEXP XXSEXP, SEXP yySEXP, SEXP returntQSEXP, SEXP returnRSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type XX(XXSEXP);
    Rcpp::traits::input_parameter< SEXP >::type yy(yySEXP);
    Rcpp::traits::input_parameter< bool >::type returntQ(returntQSEXP);
    Rcpp::traits::input_parameter< bool >::type returnR(returnRSEXP);
    rcpp_result_gen = Rcpp::wrap(lmwithQR(XX, yy, returntQ, returnR));
    return rcpp_result_gen;
END_RCPP
}
// lmwithQRP
SEXP lmwithQRP(SEXP XX, SEXP yy, bool returntQ, bool returnR);
RcppExport SEXP _spaMM_lmwithQRP(SEXP XXSEXP, SEXP yySEXP, SEXP returntQSEXP, SEXP returnRSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type XX(XXSEXP);
    Rcpp::traits::input_parameter< SEXP >::type yy(yySEXP);
    Rcpp::traits::input_parameter< bool >::type returntQ(returntQSEXP);
    Rcpp::traits::input_parameter< bool >::type returnR(returnRSEXP);
    rcpp_result_gen = Rcpp::wrap(lmwithQRP(XX, yy, returntQ, returnR));
    return rcpp_result_gen;
END_RCPP
}
// lmwith_sparse_QRp
SEXP lmwith_sparse_QRp(SEXP XX, SEXP yy, bool returntQ, bool returnR, bool COLAMDO);
RcppExport SEXP _spaMM_lmwith_sparse_QRp(SEXP XXSEXP, SEXP yySEXP, SEXP returntQSEXP, SEXP returnRSEXP, SEXP COLAMDOSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type XX(XXSEXP);
    Rcpp::traits::input_parameter< SEXP >::type yy(yySEXP);
    Rcpp::traits::input_parameter< bool >::type returntQ(returntQSEXP);
    Rcpp::traits::input_parameter< bool >::type returnR(returnRSEXP);
    Rcpp::traits::input_parameter< bool >::type COLAMDO(COLAMDOSEXP);
    rcpp_result_gen = Rcpp::wrap(lmwith_sparse_QRp(XX, yy, returntQ, returnR, COLAMDO));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_chol_R
SEXP Rcpp_chol_R(SEXP AA);
RcppExport SEXP _spaMM_Rcpp_chol_R(SEXP AASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_chol_R(AA));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_dense_cbind_mat_mat
NumericMatrix Rcpp_dense_cbind_mat_mat(NumericMatrix a, NumericMatrix b);
RcppExport SEXP _spaMM_Rcpp_dense_cbind_mat_mat(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_dense_cbind_mat_mat(a, b));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_dense_cbind_mat_vec
NumericMatrix Rcpp_dense_cbind_mat_vec(NumericMatrix a, NumericVector b);
RcppExport SEXP _spaMM_Rcpp_dense_cbind_mat_vec(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_dense_cbind_mat_vec(a, b));
    return rcpp_result_gen;
END_RCPP
}
// bessel_lnKnu_e
List bessel_lnKnu_e(Rcpp::NumericVector nu, Rcpp::NumericVector x);
RcppExport SEXP _spaMM_bessel_lnKnu_e(SEXP nuSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_lnKnu_e(nu, x));
    return rcpp_result_gen;
END_RCPP
}
// rankinfo
SEXP rankinfo(SEXP x, SEXP tol);
RcppExport SEXP _spaMM_rankinfo(SEXP xSEXP, SEXP tolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tol(tolSEXP);
    rcpp_result_gen = Rcpp::wrap(rankinfo(x, tol));
    return rcpp_result_gen;
END_RCPP
}
// leverages
SEXP leverages(SEXP XX);
RcppExport SEXP _spaMM_leverages(SEXP XXSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type XX(XXSEXP);
    rcpp_result_gen = Rcpp::wrap(leverages(XX));
    return rcpp_result_gen;
END_RCPP
}
// sweepZ1W
SEXP sweepZ1W(SEXP ZZ, SEXP WW);
RcppExport SEXP _spaMM_sweepZ1W(SEXP ZZSEXP, SEXP WWSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ZZ(ZZSEXP);
    Rcpp::traits::input_parameter< SEXP >::type WW(WWSEXP);
    rcpp_result_gen = Rcpp::wrap(sweepZ1W(ZZ, WW));
    return rcpp_result_gen;
END_RCPP
}
// ZWZt
SEXP ZWZt(SEXP ZZ, SEXP WW);
RcppExport SEXP _spaMM_ZWZt(SEXP ZZSEXP, SEXP WWSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ZZ(ZZSEXP);
    Rcpp::traits::input_parameter< SEXP >::type WW(WWSEXP);
    rcpp_result_gen = Rcpp::wrap(ZWZt(ZZ, WW));
    return rcpp_result_gen;
END_RCPP
}
// ZtWZ
SEXP ZtWZ(SEXP ZZ, SEXP WW);
RcppExport SEXP _spaMM_ZtWZ(SEXP ZZSEXP, SEXP WWSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ZZ(ZZSEXP);
    Rcpp::traits::input_parameter< SEXP >::type WW(WWSEXP);
    rcpp_result_gen = Rcpp::wrap(ZtWZ(ZZ, WW));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_d2hdv2
SEXP Rcpp_d2hdv2(SEXP ZZ, SEXP WA, SEXP WB);
RcppExport SEXP _spaMM_Rcpp_d2hdv2(SEXP ZZSEXP, SEXP WASEXP, SEXP WBSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ZZ(ZZSEXP);
    Rcpp::traits::input_parameter< SEXP >::type WA(WASEXP);
    Rcpp::traits::input_parameter< SEXP >::type WB(WBSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_d2hdv2(ZZ, WA, WB));
    return rcpp_result_gen;
END_RCPP
}
// RcppChol
SEXP RcppChol(SEXP AA);
RcppExport SEXP _spaMM_RcppChol(SEXP AASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    rcpp_result_gen = Rcpp::wrap(RcppChol(AA));
    return rcpp_result_gen;
END_RCPP
}
// crossprodCpp
SEXP crossprodCpp(SEXP Mat, SEXP yy);
RcppExport SEXP _spaMM_crossprodCpp(SEXP MatSEXP, SEXP yySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Mat(MatSEXP);
    Rcpp::traits::input_parameter< SEXP >::type yy(yySEXP);
    rcpp_result_gen = Rcpp::wrap(crossprodCpp(Mat, yy));
    return rcpp_result_gen;
END_RCPP
}
// tcrossprodCpp
SEXP tcrossprodCpp(SEXP Mat, SEXP yy);
RcppExport SEXP _spaMM_tcrossprodCpp(SEXP MatSEXP, SEXP yySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Mat(MatSEXP);
    Rcpp::traits::input_parameter< SEXP >::type yy(yySEXP);
    rcpp_result_gen = Rcpp::wrap(tcrossprodCpp(Mat, yy));
    return rcpp_result_gen;
END_RCPP
}
// LevenbergMsolveCpp
SEXP LevenbergMsolveCpp(SEXP AA, SEXP rrhhss, SEXP dd);
RcppExport SEXP _spaMM_LevenbergMsolveCpp(SEXP AASEXP, SEXP rrhhssSEXP, SEXP ddSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    Rcpp::traits::input_parameter< SEXP >::type rrhhss(rrhhssSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dd(ddSEXP);
    rcpp_result_gen = Rcpp::wrap(LevenbergMsolveCpp(AA, rrhhss, dd));
    return rcpp_result_gen;
END_RCPP
}
// LogAbsDetCpp
SEXP LogAbsDetCpp(SEXP AA);
RcppExport SEXP _spaMM_LogAbsDetCpp(SEXP AASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    rcpp_result_gen = Rcpp::wrap(LogAbsDetCpp(AA));
    return rcpp_result_gen;
END_RCPP
}
// selfAdjointSolverCpp
SEXP selfAdjointSolverCpp(SEXP AA);
RcppExport SEXP _spaMM_selfAdjointSolverCpp(SEXP AASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    rcpp_result_gen = Rcpp::wrap(selfAdjointSolverCpp(AA));
    return rcpp_result_gen;
END_RCPP
}
// dgCprod
SEXP dgCprod(SEXP AA, SEXP BB);
RcppExport SEXP _spaMM_dgCprod(SEXP AASEXP, SEXP BBSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    Rcpp::traits::input_parameter< SEXP >::type BB(BBSEXP);
    rcpp_result_gen = Rcpp::wrap(dgCprod(AA, BB));
    return rcpp_result_gen;
END_RCPP
}
// dgCcrossprod
SEXP dgCcrossprod(SEXP AA, SEXP BB);
RcppExport SEXP _spaMM_dgCcrossprod(SEXP AASEXP, SEXP BBSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    Rcpp::traits::input_parameter< SEXP >::type BB(BBSEXP);
    rcpp_result_gen = Rcpp::wrap(dgCcrossprod(AA, BB));
    return rcpp_result_gen;
END_RCPP
}
// dgCtcrossprod
SEXP dgCtcrossprod(SEXP AA, SEXP BB);
RcppExport SEXP _spaMM_dgCtcrossprod(SEXP AASEXP, SEXP BBSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    Rcpp::traits::input_parameter< SEXP >::type BB(BBSEXP);
    rcpp_result_gen = Rcpp::wrap(dgCtcrossprod(AA, BB));
    return rcpp_result_gen;
END_RCPP
}
// crossprod_not_dge
SEXP crossprod_not_dge(SEXP AA, SEXP BB, bool eval_dens);
RcppExport SEXP _spaMM_crossprod_not_dge(SEXP AASEXP, SEXP BBSEXP, SEXP eval_densSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    Rcpp::traits::input_parameter< SEXP >::type BB(BBSEXP);
    Rcpp::traits::input_parameter< bool >::type eval_dens(eval_densSEXP);
    rcpp_result_gen = Rcpp::wrap(crossprod_not_dge(AA, BB, eval_dens));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_crossprod
SEXP Rcpp_crossprod(SEXP AA, SEXP BB, bool eval_dens);
RcppExport SEXP _spaMM_Rcpp_crossprod(SEXP AASEXP, SEXP BBSEXP, SEXP eval_densSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type AA(AASEXP);
    Rcpp::traits::input_parameter< SEXP >::type BB(BBSEXP);
    Rcpp::traits::input_parameter< bool >::type eval_dens(eval_densSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_crossprod(AA, BB, eval_dens));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_spaMM_Rcpp_COMP_Z", (DL_FUNC) &_spaMM_Rcpp_COMP_Z, 4},
    {"_spaMM_lmwith_sparse_LDLp", (DL_FUNC) &_spaMM_lmwith_sparse_LDLp, 5},
    {"_spaMM_lmwith_sparse_LLp", (DL_FUNC) &_spaMM_lmwith_sparse_LLp, 5},
    {"_spaMM_lmwithQR", (DL_FUNC) &_spaMM_lmwithQR, 4},
    {"_spaMM_lmwithQRP", (DL_FUNC) &_spaMM_lmwithQRP, 4},
    {"_spaMM_lmwith_sparse_QRp", (DL_FUNC) &_spaMM_lmwith_sparse_QRp, 5},
    {"_spaMM_Rcpp_chol_R", (DL_FUNC) &_spaMM_Rcpp_chol_R, 1},
    {"_spaMM_Rcpp_dense_cbind_mat_mat", (DL_FUNC) &_spaMM_Rcpp_dense_cbind_mat_mat, 2},
    {"_spaMM_Rcpp_dense_cbind_mat_vec", (DL_FUNC) &_spaMM_Rcpp_dense_cbind_mat_vec, 2},
    {"_spaMM_bessel_lnKnu_e", (DL_FUNC) &_spaMM_bessel_lnKnu_e, 2},
    {"_spaMM_rankinfo", (DL_FUNC) &_spaMM_rankinfo, 2},
    {"_spaMM_leverages", (DL_FUNC) &_spaMM_leverages, 1},
    {"_spaMM_sweepZ1W", (DL_FUNC) &_spaMM_sweepZ1W, 2},
    {"_spaMM_ZWZt", (DL_FUNC) &_spaMM_ZWZt, 2},
    {"_spaMM_ZtWZ", (DL_FUNC) &_spaMM_ZtWZ, 2},
    {"_spaMM_Rcpp_d2hdv2", (DL_FUNC) &_spaMM_Rcpp_d2hdv2, 3},
    {"_spaMM_RcppChol", (DL_FUNC) &_spaMM_RcppChol, 1},
    {"_spaMM_crossprodCpp", (DL_FUNC) &_spaMM_crossprodCpp, 2},
    {"_spaMM_tcrossprodCpp", (DL_FUNC) &_spaMM_tcrossprodCpp, 2},
    {"_spaMM_LevenbergMsolveCpp", (DL_FUNC) &_spaMM_LevenbergMsolveCpp, 3},
    {"_spaMM_LogAbsDetCpp", (DL_FUNC) &_spaMM_LogAbsDetCpp, 1},
    {"_spaMM_selfAdjointSolverCpp", (DL_FUNC) &_spaMM_selfAdjointSolverCpp, 1},
    {"_spaMM_dgCprod", (DL_FUNC) &_spaMM_dgCprod, 2},
    {"_spaMM_dgCcrossprod", (DL_FUNC) &_spaMM_dgCcrossprod, 2},
    {"_spaMM_dgCtcrossprod", (DL_FUNC) &_spaMM_dgCtcrossprod, 2},
    {"_spaMM_crossprod_not_dge", (DL_FUNC) &_spaMM_crossprod_not_dge, 3},
    {"_spaMM_Rcpp_crossprod", (DL_FUNC) &_spaMM_Rcpp_crossprod, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_spaMM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
