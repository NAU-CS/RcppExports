// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// aux_FrobeniusDiff
double aux_FrobeniusDiff(arma::mat& A, arma::mat& B);
RcppExport SEXP _NetworkDistance_aux_FrobeniusDiff(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(aux_FrobeniusDiff(A, B));
    return rcpp_result_gen;
END_RCPP
}
// lfdistance
double lfdistance(arma::mat& L1, arma::mat& L2, double inct);
RcppExport SEXP _NetworkDistance_lfdistance(SEXP L1SEXP, SEXP L2SEXP, SEXP inctSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type L1(L1SEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type L2(L2SEXP);
    Rcpp::traits::input_parameter< double >::type inct(inctSEXP);
    rcpp_result_gen = Rcpp::wrap(lfdistance(L1, L2, inct));
    return rcpp_result_gen;
END_RCPP
}
// lfdistance_new
double lfdistance_new(arma::mat& L1, arma::vec D1, arma::mat& L2, arma::vec D2, arma::vec timestamp);
RcppExport SEXP _NetworkDistance_lfdistance_new(SEXP L1SEXP, SEXP D1SEXP, SEXP L2SEXP, SEXP D2SEXP, SEXP timestampSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type L1(L1SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type D1(D1SEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type L2(L2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type D2(D2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type timestamp(timestampSEXP);
    rcpp_result_gen = Rcpp::wrap(lfdistance_new(L1, D1, L2, D2, timestamp));
    return rcpp_result_gen;
END_RCPP
}
// lfdistance_new_faster
arma::mat lfdistance_new_faster(arma::cube& vecs, arma::mat& vals, arma::vec timestamps);
RcppExport SEXP _NetworkDistance_lfdistance_new_faster(SEXP vecsSEXP, SEXP valsSEXP, SEXP timestampsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube& >::type vecs(vecsSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type vals(valsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type timestamps(timestampsSEXP);
    rcpp_result_gen = Rcpp::wrap(lfdistance_new_faster(vecs, vals, timestamps));
    return rcpp_result_gen;
END_RCPP
}
// cpp_gdd
Rcpp::List cpp_gdd(arma::cube& vecs, arma::mat& vals, arma::vec timestamps);
RcppExport SEXP _NetworkDistance_cpp_gdd(SEXP vecsSEXP, SEXP valsSEXP, SEXP timestampsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube& >::type vecs(vecsSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type vals(valsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type timestamps(timestampsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_gdd(vecs, vals, timestamps));
    return rcpp_result_gen;
END_RCPP
}
// eigendec
double eigendec(arma::mat& L);
RcppExport SEXP _NetworkDistance_eigendec(SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type L(LSEXP);
    rcpp_result_gen = Rcpp::wrap(eigendec(L));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_NetworkDistance_aux_FrobeniusDiff", (DL_FUNC) &_NetworkDistance_aux_FrobeniusDiff, 2},
    {"_NetworkDistance_lfdistance", (DL_FUNC) &_NetworkDistance_lfdistance, 3},
    {"_NetworkDistance_lfdistance_new", (DL_FUNC) &_NetworkDistance_lfdistance_new, 5},
    {"_NetworkDistance_lfdistance_new_faster", (DL_FUNC) &_NetworkDistance_lfdistance_new_faster, 3},
    {"_NetworkDistance_cpp_gdd", (DL_FUNC) &_NetworkDistance_cpp_gdd, 3},
    {"_NetworkDistance_eigendec", (DL_FUNC) &_NetworkDistance_eigendec, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_NetworkDistance(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
