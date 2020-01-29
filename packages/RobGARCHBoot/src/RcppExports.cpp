// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// ROBUSTGARCHloss_RCPP
SEXP ROBUSTGARCHloss_RCPP(NumericVector theta, NumericVector r, double sigma2);
RcppExport SEXP _RobGARCHBoot_ROBUSTGARCHloss_RCPP(SEXP thetaSEXP, SEXP rSEXP, SEXP sigma2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type sigma2(sigma2SEXP);
    rcpp_result_gen = Rcpp::wrap(ROBUSTGARCHloss_RCPP(theta, r, sigma2));
    return rcpp_result_gen;
END_RCPP
}
// grid_RCPP
SEXP grid_RCPP(NumericVector y, double sigmaR);
RcppExport SEXP _RobGARCHBoot_grid_RCPP(SEXP ySEXP, SEXP sigmaRSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type sigmaR(sigmaRSEXP);
    rcpp_result_gen = Rcpp::wrap(grid_RCPP(y, sigmaR));
    return rcpp_result_gen;
END_RCPP
}
// resBoot
SEXP resBoot(NumericVector coeff, NumericVector r, double S, double k);
RcppExport SEXP _RobGARCHBoot_resBoot(SEXP coeffSEXP, SEXP rSEXP, SEXP SSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type coeff(coeffSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type S(SSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(resBoot(coeff, r, S, k));
    return rcpp_result_gen;
END_RCPP
}
// retBoot
SEXP retBoot(NumericVector coeff, double S, NumericVector e, double k);
RcppExport SEXP _RobGARCHBoot_retBoot(SEXP coeffSEXP, SEXP SSEXP, SEXP eSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type coeff(coeffSEXP);
    Rcpp::traits::input_parameter< double >::type S(SSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type e(eSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(retBoot(coeff, S, e, k));
    return rcpp_result_gen;
END_RCPP
}
// sigma2Boot
SEXP sigma2Boot(NumericVector coeff, NumericVector e, double S, NumericVector r, double k);
RcppExport SEXP _RobGARCHBoot_sigma2Boot(SEXP coeffSEXP, SEXP eSEXP, SEXP SSEXP, SEXP rSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type coeff(coeffSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type e(eSEXP);
    Rcpp::traits::input_parameter< double >::type S(SSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(sigma2Boot(coeff, e, S, r, k));
    return rcpp_result_gen;
END_RCPP
}
// foreBoot
SEXP foreBoot(NumericVector coeff, NumericVector e, NumericVector e2, NumericVector h, NumericVector r, int ahead, double k);
RcppExport SEXP _RobGARCHBoot_foreBoot(SEXP coeffSEXP, SEXP eSEXP, SEXP e2SEXP, SEXP hSEXP, SEXP rSEXP, SEXP aheadSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type coeff(coeffSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type e(eSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type e2(e2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type h(hSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< int >::type ahead(aheadSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(foreBoot(coeff, e, e2, h, r, ahead, k));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RobGARCHBoot_ROBUSTGARCHloss_RCPP", (DL_FUNC) &_RobGARCHBoot_ROBUSTGARCHloss_RCPP, 3},
    {"_RobGARCHBoot_grid_RCPP", (DL_FUNC) &_RobGARCHBoot_grid_RCPP, 2},
    {"_RobGARCHBoot_resBoot", (DL_FUNC) &_RobGARCHBoot_resBoot, 4},
    {"_RobGARCHBoot_retBoot", (DL_FUNC) &_RobGARCHBoot_retBoot, 4},
    {"_RobGARCHBoot_sigma2Boot", (DL_FUNC) &_RobGARCHBoot_sigma2Boot, 5},
    {"_RobGARCHBoot_foreBoot", (DL_FUNC) &_RobGARCHBoot_foreBoot, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_RobGARCHBoot(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}