// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// testvals
Eigen::VectorXd testvals(Eigen::Map<Eigen::MatrixXd> y, Eigen::Map<Eigen::MatrixXd> Coefpremult, Eigen::Map<Eigen::VectorXd> partial, Eigen::Map<Eigen::MatrixXd> X, Eigen::Map<Eigen::MatrixXd> Wmat, Eigen::Map<Eigen::MatrixXd> Bmat, Eigen::Map<Eigen::MatrixXd> R, int dim, int Nrep, int q, int cores, bool Eicker);
RcppExport SEXP acrt_testvals(SEXP ySEXP, SEXP CoefpremultSEXP, SEXP partialSEXP, SEXP XSEXP, SEXP WmatSEXP, SEXP BmatSEXP, SEXP RSEXP, SEXP dimSEXP, SEXP NrepSEXP, SEXP qSEXP, SEXP coresSEXP, SEXP EickerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type y(ySEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type Coefpremult(CoefpremultSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXd> >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type X(XSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type Wmat(WmatSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type Bmat(BmatSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type R(RSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< int >::type Nrep(NrepSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< bool >::type Eicker(EickerSEXP);
    rcpp_result_gen = Rcpp::wrap(testvals(y, Coefpremult, partial, X, Wmat, Bmat, R, dim, Nrep, q, cores, Eicker));
    return rcpp_result_gen;
END_RCPP
}
// ctest
Eigen::VectorXd ctest(Eigen::MatrixXd umat, Eigen::MatrixXd Rbmat, Eigen::MatrixXd Wmat, Eigen::MatrixXd Bmat, int cores);
RcppExport SEXP acrt_ctest(SEXP umatSEXP, SEXP RbmatSEXP, SEXP WmatSEXP, SEXP BmatSEXP, SEXP coresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type umat(umatSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type Rbmat(RbmatSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type Wmat(WmatSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type Bmat(BmatSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    rcpp_result_gen = Rcpp::wrap(ctest(umat, Rbmat, Wmat, Bmat, cores));
    return rcpp_result_gen;
END_RCPP
}
// ctestE
Eigen::VectorXd ctestE(Eigen::MatrixXd umat, Eigen::MatrixXd Rbmat, Eigen::MatrixXd Wmat, Eigen::MatrixXd Bmat, int cores);
RcppExport SEXP acrt_ctestE(SEXP umatSEXP, SEXP RbmatSEXP, SEXP WmatSEXP, SEXP BmatSEXP, SEXP coresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type umat(umatSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type Rbmat(RbmatSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type Wmat(WmatSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type Bmat(BmatSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    rcpp_result_gen = Rcpp::wrap(ctestE(umat, Rbmat, Wmat, Bmat, cores));
    return rcpp_result_gen;
END_RCPP
}
