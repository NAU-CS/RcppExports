// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// colMax_dense
Eigen::MatrixXd colMax_dense(const Eigen::Map<Eigen::MatrixXd>& A);
RcppExport SEXP _fastglm_colMax_dense(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd>& >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(colMax_dense(A));
    return rcpp_result_gen;
END_RCPP
}
// colMin_dense
Eigen::MatrixXd colMin_dense(const Eigen::Map<Eigen::MatrixXd>& A);
RcppExport SEXP _fastglm_colMin_dense(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd>& >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(colMin_dense(A));
    return rcpp_result_gen;
END_RCPP
}
// fit_glm
List fit_glm(Rcpp::NumericMatrix x, Rcpp::NumericVector y, Rcpp::NumericVector weights, Rcpp::NumericVector offset, Rcpp::NumericVector start, Rcpp::NumericVector mu, Rcpp::NumericVector eta, Function var, Function mu_eta, Function linkinv, Function dev_resids, Function valideta, Function validmu, int type, double tol, int maxit);
RcppExport SEXP _fastglm_fit_glm(SEXP xSEXP, SEXP ySEXP, SEXP weightsSEXP, SEXP offsetSEXP, SEXP startSEXP, SEXP muSEXP, SEXP etaSEXP, SEXP varSEXP, SEXP mu_etaSEXP, SEXP linkinvSEXP, SEXP dev_residsSEXP, SEXP validetaSEXP, SEXP validmuSEXP, SEXP typeSEXP, SEXP tolSEXP, SEXP maxitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< Function >::type var(varSEXP);
    Rcpp::traits::input_parameter< Function >::type mu_eta(mu_etaSEXP);
    Rcpp::traits::input_parameter< Function >::type linkinv(linkinvSEXP);
    Rcpp::traits::input_parameter< Function >::type dev_resids(dev_residsSEXP);
    Rcpp::traits::input_parameter< Function >::type valideta(validetaSEXP);
    Rcpp::traits::input_parameter< Function >::type validmu(validmuSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_glm(x, y, weights, offset, start, mu, eta, var, mu_eta, linkinv, dev_resids, valideta, validmu, type, tol, maxit));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fastglm_colMax_dense", (DL_FUNC) &_fastglm_colMax_dense, 1},
    {"_fastglm_colMin_dense", (DL_FUNC) &_fastglm_colMin_dense, 1},
    {"_fastglm_fit_glm", (DL_FUNC) &_fastglm_fit_glm, 16},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastglm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}