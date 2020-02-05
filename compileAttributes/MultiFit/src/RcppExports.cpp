// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// discretizeCpp
Rcpp::List discretizeCpp(arma::mat a, arma::mat b, arma::rowvec w, arma::uvec mask, arma::umat ij, int Dx, int Dy);
RcppExport SEXP _MultiFit_discretizeCpp(SEXP aSEXP, SEXP bSEXP, SEXP wSEXP, SEXP maskSEXP, SEXP ijSEXP, SEXP DxSEXP, SEXP DySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type w(wSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type mask(maskSEXP);
    Rcpp::traits::input_parameter< arma::umat >::type ij(ijSEXP);
    Rcpp::traits::input_parameter< int >::type Dx(DxSEXP);
    Rcpp::traits::input_parameter< int >::type Dy(DySEXP);
    rcpp_result_gen = Rcpp::wrap(discretizeCpp(a, b, w, mask, ij, Dx, Dy));
    return rcpp_result_gen;
END_RCPP
}
// make_CDF
Rcpp::List make_CDF(int lp, arma::uvec col0_tot, arma::uvec row1_tot, arma::vec min_margin, Rcpp::List ALL_PROBS, bool compute_all_holm, double min_p);
RcppExport SEXP _MultiFit_make_CDF(SEXP lpSEXP, SEXP col0_totSEXP, SEXP row1_totSEXP, SEXP min_marginSEXP, SEXP ALL_PROBSSEXP, SEXP compute_all_holmSEXP, SEXP min_pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type lp(lpSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type col0_tot(col0_totSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type row1_tot(row1_totSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type min_margin(min_marginSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type ALL_PROBS(ALL_PROBSSEXP);
    Rcpp::traits::input_parameter< bool >::type compute_all_holm(compute_all_holmSEXP);
    Rcpp::traits::input_parameter< double >::type min_p(min_pSEXP);
    rcpp_result_gen = Rcpp::wrap(make_CDF(lp, col0_tot, row1_tot, min_margin, ALL_PROBS, compute_all_holm, min_p));
    return rcpp_result_gen;
END_RCPP
}
// single_Fisher_test
Rcpp::List single_Fisher_test(arma::rowvec t, bool correct, bool ret_all_probs);
RcppExport SEXP _MultiFit_single_Fisher_test(SEXP tSEXP, SEXP correctSEXP, SEXP ret_all_probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::rowvec >::type t(tSEXP);
    Rcpp::traits::input_parameter< bool >::type correct(correctSEXP);
    Rcpp::traits::input_parameter< bool >::type ret_all_probs(ret_all_probsSEXP);
    rcpp_result_gen = Rcpp::wrap(single_Fisher_test(t, correct, ret_all_probs));
    return rcpp_result_gen;
END_RCPP
}
// genStepFunCpp
NumericVector genStepFunCpp(Rcpp::List SPS, NumericVector sort_p, int lc, int lp);
RcppExport SEXP _MultiFit_genStepFunCpp(SEXP SPSSEXP, SEXP sort_pSEXP, SEXP lcSEXP, SEXP lpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type SPS(SPSSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sort_p(sort_pSEXP);
    Rcpp::traits::input_parameter< int >::type lc(lcSEXP);
    Rcpp::traits::input_parameter< int >::type lp(lpSEXP);
    rcpp_result_gen = Rcpp::wrap(genStepFunCpp(SPS, sort_p, lc, lp));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MultiFit_discretizeCpp", (DL_FUNC) &_MultiFit_discretizeCpp, 7},
    {"_MultiFit_make_CDF", (DL_FUNC) &_MultiFit_make_CDF, 7},
    {"_MultiFit_single_Fisher_test", (DL_FUNC) &_MultiFit_single_Fisher_test, 3},
    {"_MultiFit_genStepFunCpp", (DL_FUNC) &_MultiFit_genStepFunCpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_MultiFit(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
