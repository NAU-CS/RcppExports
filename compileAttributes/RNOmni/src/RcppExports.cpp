// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// cov
SEXP cov(const Eigen::Map<Eigen::MatrixXd> A, const Eigen::Map<Eigen::MatrixXd> B, const bool cor);
RcppExport SEXP _RNOmni_cov(SEXP ASEXP, SEXP BSEXP, SEXP corSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type A(ASEXP);
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type B(BSEXP);
    Rcpp::traits::input_parameter< const bool >::type cor(corSEXP);
    rcpp_result_gen = Rcpp::wrap(cov(A, B, cor));
    return rcpp_result_gen;
END_RCPP
}
// matIP
SEXP matIP(const Eigen::Map<Eigen::MatrixXd> A, const Eigen::Map<Eigen::MatrixXd> B);
RcppExport SEXP _RNOmni_matIP(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type A(ASEXP);
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(matIP(A, B));
    return rcpp_result_gen;
END_RCPP
}
// matInv
SEXP matInv(const Eigen::Map<Eigen::MatrixXd> A);
RcppExport SEXP _RNOmni_matInv(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(matInv(A));
    return rcpp_result_gen;
END_RCPP
}
// SchurC
SEXP SchurC(const Eigen::Map<Eigen::MatrixXd> Ibb, const Eigen::Map<Eigen::MatrixXd> Iaa, const Eigen::Map<Eigen::MatrixXd> Iba);
RcppExport SEXP _RNOmni_SchurC(SEXP IbbSEXP, SEXP IaaSEXP, SEXP IbaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type Ibb(IbbSEXP);
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type Iaa(IaaSEXP);
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type Iba(IbaSEXP);
    rcpp_result_gen = Rcpp::wrap(SchurC(Ibb, Iaa, Iba));
    return rcpp_result_gen;
END_RCPP
}
// fitOLS
SEXP fitOLS(const Eigen::Map<Eigen::VectorXd> y, const Eigen::Map<Eigen::MatrixXd> X);
RcppExport SEXP _RNOmni_fitOLS(SEXP ySEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::VectorXd> >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(fitOLS(y, X));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RNOmni_cov", (DL_FUNC) &_RNOmni_cov, 3},
    {"_RNOmni_matIP", (DL_FUNC) &_RNOmni_matIP, 2},
    {"_RNOmni_matInv", (DL_FUNC) &_RNOmni_matInv, 1},
    {"_RNOmni_SchurC", (DL_FUNC) &_RNOmni_SchurC, 3},
    {"_RNOmni_fitOLS", (DL_FUNC) &_RNOmni_fitOLS, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_RNOmni(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
