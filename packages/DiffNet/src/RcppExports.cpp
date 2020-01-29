// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// GHD_Fast
double GHD_Fast(Eigen::SparseMatrix<double> A, Eigen::SparseMatrix<double> B);
RcppExport SEXP DiffNet_GHD_Fast(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(GHD_Fast(A, B));
    return rcpp_result_gen;
END_RCPP
}
// MU_Fast
double MU_Fast(Eigen::SparseMatrix<double> A, Eigen::SparseMatrix<double> B);
RcppExport SEXP DiffNet_MU_Fast(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(MU_Fast(A, B));
    return rcpp_result_gen;
END_RCPP
}
// STD_Fast
double STD_Fast(Eigen::SparseMatrix<double> A, Eigen::SparseMatrix<double> B);
RcppExport SEXP DiffNet_STD_Fast(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(STD_Fast(A, B));
    return rcpp_result_gen;
END_RCPP
}
