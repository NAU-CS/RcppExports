// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// permute
List permute(Eigen::MatrixXd iTest, Eigen::MatrixXd jTest, SEXP times, SEXP selectvec);
RcppExport SEXP _MediaK_permute(SEXP iTestSEXP, SEXP jTestSEXP, SEXP timesSEXP, SEXP selectvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type iTest(iTestSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type jTest(jTestSEXP);
    Rcpp::traits::input_parameter< SEXP >::type times(timesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type selectvec(selectvecSEXP);
    rcpp_result_gen = Rcpp::wrap(permute(iTest, jTest, times, selectvec));
    return rcpp_result_gen;
END_RCPP
}
// dis_value
double dis_value(Eigen::MatrixXd iTest, Eigen::MatrixXd jTest, SEXP select);
RcppExport SEXP _MediaK_dis_value(SEXP iTestSEXP, SEXP jTestSEXP, SEXP selectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type iTest(iTestSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type jTest(jTestSEXP);
    Rcpp::traits::input_parameter< SEXP >::type select(selectSEXP);
    rcpp_result_gen = Rcpp::wrap(dis_value(iTest, jTest, select));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MediaK_permute", (DL_FUNC) &_MediaK_permute, 4},
    {"_MediaK_dis_value", (DL_FUNC) &_MediaK_dis_value, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_MediaK(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}