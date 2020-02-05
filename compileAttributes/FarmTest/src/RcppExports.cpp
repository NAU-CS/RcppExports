// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// huberMean
double huberMean(const arma::vec& X, const int n, const double epsilon, const int iteMax);
RcppExport SEXP _FarmTest_huberMean(SEXP XSEXP, SEXP nSEXP, SEXP epsilonSEXP, SEXP iteMaxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< const int >::type iteMax(iteMaxSEXP);
    rcpp_result_gen = Rcpp::wrap(huberMean(X, n, epsilon, iteMax));
    return rcpp_result_gen;
END_RCPP
}
// huberCov
Rcpp::List huberCov(const arma::mat& X, const int n, const int p);
RcppExport SEXP _FarmTest_huberCov(SEXP XSEXP, SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(huberCov(X, n, p));
    return rcpp_result_gen;
END_RCPP
}
// getRej
arma::uvec getRej(const arma::vec& Prob, const double alpha, const int p);
RcppExport SEXP _FarmTest_getRej(SEXP ProbSEXP, SEXP alphaSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type Prob(ProbSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(getRej(Prob, alpha, p));
    return rcpp_result_gen;
END_RCPP
}
// rmTest
Rcpp::List rmTest(const arma::mat& X, const arma::vec& h0, const double alpha, const std::string alternative);
RcppExport SEXP _FarmTest_rmTest(SEXP XSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    rcpp_result_gen = Rcpp::wrap(rmTest(X, h0, alpha, alternative));
    return rcpp_result_gen;
END_RCPP
}
// rmTestBoot
Rcpp::List rmTestBoot(const arma::mat& X, const arma::vec& h0, const double alpha, const std::string alternative, const int B);
RcppExport SEXP _FarmTest_rmTestBoot(SEXP XSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< const int >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(rmTestBoot(X, h0, alpha, alternative, B));
    return rcpp_result_gen;
END_RCPP
}
// rmTestTwo
Rcpp::List rmTestTwo(const arma::mat& X, const arma::mat& Y, const arma::vec& h0, const double alpha, const std::string alternative);
RcppExport SEXP _FarmTest_rmTestTwo(SEXP XSEXP, SEXP YSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    rcpp_result_gen = Rcpp::wrap(rmTestTwo(X, Y, h0, alpha, alternative));
    return rcpp_result_gen;
END_RCPP
}
// rmTestTwoBoot
Rcpp::List rmTestTwoBoot(const arma::mat& X, const arma::mat& Y, const arma::vec& h0, const double alpha, const std::string alternative, const int B);
RcppExport SEXP _FarmTest_rmTestTwoBoot(SEXP XSEXP, SEXP YSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< const int >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(rmTestTwoBoot(X, Y, h0, alpha, alternative, B));
    return rcpp_result_gen;
END_RCPP
}
// farmTest
Rcpp::List farmTest(const arma::mat& X, const arma::vec& h0, int K, const double alpha, const std::string alternative);
RcppExport SEXP _FarmTest_farmTest(SEXP XSEXP, SEXP h0SEXP, SEXP KSEXP, SEXP alphaSEXP, SEXP alternativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    rcpp_result_gen = Rcpp::wrap(farmTest(X, h0, K, alpha, alternative));
    return rcpp_result_gen;
END_RCPP
}
// farmTestTwo
Rcpp::List farmTestTwo(const arma::mat& X, const arma::mat& Y, const arma::vec& h0, int KX, int KY, const double alpha, const std::string alternative);
RcppExport SEXP _FarmTest_farmTestTwo(SEXP XSEXP, SEXP YSEXP, SEXP h0SEXP, SEXP KXSEXP, SEXP KYSEXP, SEXP alphaSEXP, SEXP alternativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< int >::type KX(KXSEXP);
    Rcpp::traits::input_parameter< int >::type KY(KYSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    rcpp_result_gen = Rcpp::wrap(farmTestTwo(X, Y, h0, KX, KY, alpha, alternative));
    return rcpp_result_gen;
END_RCPP
}
// farmTestFac
Rcpp::List farmTestFac(const arma::mat& X, const arma::mat& fac, const arma::vec& h0, const double alpha, const std::string alternative);
RcppExport SEXP _FarmTest_farmTestFac(SEXP XSEXP, SEXP facSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type fac(facSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    rcpp_result_gen = Rcpp::wrap(farmTestFac(X, fac, h0, alpha, alternative));
    return rcpp_result_gen;
END_RCPP
}
// farmTestFacBoot
Rcpp::List farmTestFacBoot(const arma::mat& X, const arma::mat& fac, const arma::vec& h0, const double alpha, const std::string alternative, const int B);
RcppExport SEXP _FarmTest_farmTestFacBoot(SEXP XSEXP, SEXP facSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type fac(facSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< const int >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(farmTestFacBoot(X, fac, h0, alpha, alternative, B));
    return rcpp_result_gen;
END_RCPP
}
// farmTestTwoFac
Rcpp::List farmTestTwoFac(const arma::mat& X, const arma::mat& facX, const arma::mat& Y, const arma::mat& facY, const arma::vec& h0, const double alpha, const std::string alternative);
RcppExport SEXP _FarmTest_farmTestTwoFac(SEXP XSEXP, SEXP facXSEXP, SEXP YSEXP, SEXP facYSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type facX(facXSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type facY(facYSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    rcpp_result_gen = Rcpp::wrap(farmTestTwoFac(X, facX, Y, facY, h0, alpha, alternative));
    return rcpp_result_gen;
END_RCPP
}
// farmTestTwoFacBoot
Rcpp::List farmTestTwoFacBoot(const arma::mat& X, const arma::mat& facX, const arma::mat& Y, const arma::mat& facY, const arma::vec& h0, const double alpha, const std::string alternative, const int B);
RcppExport SEXP _FarmTest_farmTestTwoFacBoot(SEXP XSEXP, SEXP facXSEXP, SEXP YSEXP, SEXP facYSEXP, SEXP h0SEXP, SEXP alphaSEXP, SEXP alternativeSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type facX(facXSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type facY(facYSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type h0(h0SEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const std::string >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< const int >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(farmTestTwoFacBoot(X, facX, Y, facY, h0, alpha, alternative, B));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FarmTest_huberMean", (DL_FUNC) &_FarmTest_huberMean, 4},
    {"_FarmTest_huberCov", (DL_FUNC) &_FarmTest_huberCov, 3},
    {"_FarmTest_getRej", (DL_FUNC) &_FarmTest_getRej, 3},
    {"_FarmTest_rmTest", (DL_FUNC) &_FarmTest_rmTest, 4},
    {"_FarmTest_rmTestBoot", (DL_FUNC) &_FarmTest_rmTestBoot, 5},
    {"_FarmTest_rmTestTwo", (DL_FUNC) &_FarmTest_rmTestTwo, 5},
    {"_FarmTest_rmTestTwoBoot", (DL_FUNC) &_FarmTest_rmTestTwoBoot, 6},
    {"_FarmTest_farmTest", (DL_FUNC) &_FarmTest_farmTest, 5},
    {"_FarmTest_farmTestTwo", (DL_FUNC) &_FarmTest_farmTestTwo, 7},
    {"_FarmTest_farmTestFac", (DL_FUNC) &_FarmTest_farmTestFac, 5},
    {"_FarmTest_farmTestFacBoot", (DL_FUNC) &_FarmTest_farmTestFacBoot, 6},
    {"_FarmTest_farmTestTwoFac", (DL_FUNC) &_FarmTest_farmTestTwoFac, 7},
    {"_FarmTest_farmTestTwoFacBoot", (DL_FUNC) &_FarmTest_farmTestTwoFacBoot, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_FarmTest(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}