// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// CCK
SEXP CCK(SEXP Y, SEXP k);
RcppExport SEXP _tsBSS_CCK(SEXP YSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(CCK(Y, k));
    return rcpp_result_gen;
END_RCPP
}
// PVCk
SEXP PVCk(SEXP Y, SEXP k);
RcppExport SEXP _tsBSS_PVCk(SEXP YSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(PVCk(Y, k));
    return rcpp_result_gen;
END_RCPP
}
// TIK
SEXP TIK(SEXP Y, SEXP U, SEXP k, SEXP method);
RcppExport SEXP _tsBSS_TIK(SEXP YSEXP, SEXP USEXP, SEXP kSEXP, SEXP methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type U(USEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    Rcpp::traits::input_parameter< SEXP >::type method(methodSEXP);
    rcpp_result_gen = Rcpp::wrap(TIK(Y, U, k, method));
    return rcpp_result_gen;
END_RCPP
}
// TIKlc
SEXP TIKlc(SEXP Y, SEXP U, SEXP k, SEXP method);
RcppExport SEXP _tsBSS_TIKlc(SEXP YSEXP, SEXP USEXP, SEXP kSEXP, SEXP methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type U(USEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    Rcpp::traits::input_parameter< SEXP >::type method(methodSEXP);
    rcpp_result_gen = Rcpp::wrap(TIKlc(Y, U, k, method));
    return rcpp_result_gen;
END_RCPP
}
// TIK1
SEXP TIK1(SEXP Y, SEXP U, SEXP k);
RcppExport SEXP _tsBSS_TIK1(SEXP YSEXP, SEXP USEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type U(USEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(TIK1(Y, U, k));
    return rcpp_result_gen;
END_RCPP
}
// TSIR
SEXP TSIR(SEXP X, SEXP slices, SEXP k, SEXP h);
RcppExport SEXP _tsBSS_TSIR(SEXP XSEXP, SEXP slicesSEXP, SEXP kSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type slices(slicesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    Rcpp::traits::input_parameter< SEXP >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(TSIR(X, slices, k, h));
    return rcpp_result_gen;
END_RCPP
}
// TSAVE
SEXP TSAVE(SEXP X, SEXP slices, SEXP k, SEXP h);
RcppExport SEXP _tsBSS_TSAVE(SEXP XSEXP, SEXP slicesSEXP, SEXP kSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type slices(slicesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    Rcpp::traits::input_parameter< SEXP >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(TSAVE(X, slices, k, h));
    return rcpp_result_gen;
END_RCPP
}
// varExx_k
arma::mat varExx_k(arma::mat X, arma::vec k);
RcppExport SEXP _tsBSS_varExx_k(SEXP XSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(varExx_k(X, k));
    return rcpp_result_gen;
END_RCPP
}
// lblinM
SEXP lblinM(SEXP X, SEXP k);
RcppExport SEXP _tsBSS_lblinM(SEXP XSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(lblinM(X, k));
    return rcpp_result_gen;
END_RCPP
}
// lbsqM
SEXP lbsqM(SEXP X, SEXP k);
RcppExport SEXP _tsBSS_lbsqM(SEXP XSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(lbsqM(X, k));
    return rcpp_result_gen;
END_RCPP
}
// EIGEN
SEXP EIGEN(SEXP X);
RcppExport SEXP _tsBSS_EIGEN(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(EIGEN(X));
    return rcpp_result_gen;
END_RCPP
}
// PREPBSS
SEXP PREPBSS(SEXP X, SEXP n);
RcppExport SEXP _tsBSS_PREPBSS(SEXP XSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(PREPBSS(X, n));
    return rcpp_result_gen;
END_RCPP
}