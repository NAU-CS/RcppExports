// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// permute
List permute(Eigen::MatrixXd iTest, Eigen::MatrixXd jTest, SEXP times, SEXP selectvec);
RcppExport SEXP MediaK_permute(SEXP iTestSEXP, SEXP jTestSEXP, SEXP timesSEXP, SEXP selectvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type iTest(iTestSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type jTest(jTestSEXP);
    Rcpp::traits::input_parameter< SEXP >::type times(timesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type selectvec(selectvecSEXP);
    __result = Rcpp::wrap(permute(iTest, jTest, times, selectvec));
    return __result;
END_RCPP
}
// dis_value
double dis_value(Eigen::MatrixXd iTest, Eigen::MatrixXd jTest, SEXP select);
RcppExport SEXP MediaK_dis_value(SEXP iTestSEXP, SEXP jTestSEXP, SEXP selectSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type iTest(iTestSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type jTest(jTestSEXP);
    Rcpp::traits::input_parameter< SEXP >::type select(selectSEXP);
    __result = Rcpp::wrap(dis_value(iTest, jTest, select));
    return __result;
END_RCPP
}