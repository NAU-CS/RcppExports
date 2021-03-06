// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// stlSort
IntegerVector stlSort(IntegerVector x);
RcppExport SEXP _BClustLonG_stlSort(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(stlSort(x));
    return rcpp_result_gen;
END_RCPP
}
// myc
IntegerVector myc(IntegerVector x, IntegerVector y);
RcppExport SEXP _BClustLonG_myc(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(myc(x, y));
    return rcpp_result_gen;
END_RCPP
}
// dmvnrmArma
arma::vec dmvnrmArma(arma::mat x, arma::rowvec mean, arma::mat sigma, bool logd);
RcppExport SEXP _BClustLonG_dmvnrmArma(SEXP xSEXP, SEXP meanSEXP, SEXP sigmaSEXP, SEXP logdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< bool >::type logd(logdSEXP);
    rcpp_result_gen = Rcpp::wrap(dmvnrmArma(x, mean, sigma, logd));
    return rcpp_result_gen;
END_RCPP
}
// myfind
arma::uvec myfind(IntegerVector evec, int e);
RcppExport SEXP _BClustLonG_myfind(SEXP evecSEXP, SEXP eSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type evec(evecSEXP);
    Rcpp::traits::input_parameter< int >::type e(eSEXP);
    rcpp_result_gen = Rcpp::wrap(myfind(evec, e));
    return rcpp_result_gen;
END_RCPP
}
// mvrnormArma
arma::mat mvrnormArma(int n, arma::vec mu, arma::mat sigma);
RcppExport SEXP _BClustLonG_mvrnormArma(SEXP nSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(mvrnormArma(n, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}
// samLamV2Cpp
arma::mat samLamV2Cpp(arma::mat A, arma::mat eta, arma::vec sig, arma::mat lam, arma::mat phi, arma::rowvec tau);
RcppExport SEXP _BClustLonG_samLamV2Cpp(SEXP ASEXP, SEXP etaSEXP, SEXP sigSEXP, SEXP lamSEXP, SEXP phiSEXP, SEXP tauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type tau(tauSEXP);
    rcpp_result_gen = Rcpp::wrap(samLamV2Cpp(A, eta, sig, lam, phi, tau));
    return rcpp_result_gen;
END_RCPP
}
// polyurncppBoth
IntegerVector polyurncppBoth(IntegerVector e, arma::mat A, arma::vec muA0, arma::mat sigmaA, arma::mat sigmaAInv, arma::mat sigmaA0, arma::mat sigmaA0Inv, arma::mat B, arma::vec muB0, arma::mat sigmaB, arma::mat sigmaBInv, arma::mat sigmaB0, arma::mat sigmaB0Inv, double c);
RcppExport SEXP _BClustLonG_polyurncppBoth(SEXP eSEXP, SEXP ASEXP, SEXP muA0SEXP, SEXP sigmaASEXP, SEXP sigmaAInvSEXP, SEXP sigmaA0SEXP, SEXP sigmaA0InvSEXP, SEXP BSEXP, SEXP muB0SEXP, SEXP sigmaBSEXP, SEXP sigmaBInvSEXP, SEXP sigmaB0SEXP, SEXP sigmaB0InvSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type e(eSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::vec >::type muA0(muA0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaA(sigmaASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaAInv(sigmaAInvSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaA0(sigmaA0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaA0Inv(sigmaA0InvSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type muB0(muB0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaB(sigmaBSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaBInv(sigmaBInvSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaB0(sigmaB0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaB0Inv(sigmaB0InvSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(polyurncppBoth(e, A, muA0, sigmaA, sigmaAInv, sigmaA0, sigmaA0Inv, B, muB0, sigmaB, sigmaBInv, sigmaB0, sigmaB0Inv, c));
    return rcpp_result_gen;
END_RCPP
}
// polyurncppInt
IntegerVector polyurncppInt(IntegerVector e, arma::vec muA0, arma::mat sigma0, arma::mat A, arma::mat sigma, arma::mat sigmaInv, arma::mat sigma0Inv, double c);
RcppExport SEXP _BClustLonG_polyurncppInt(SEXP eSEXP, SEXP muA0SEXP, SEXP sigma0SEXP, SEXP ASEXP, SEXP sigmaSEXP, SEXP sigmaInvSEXP, SEXP sigma0InvSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type e(eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type muA0(muA0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma0(sigma0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigmaInv(sigmaInvSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma0Inv(sigma0InvSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(polyurncppInt(e, muA0, sigma0, A, sigma, sigmaInv, sigma0Inv, c));
    return rcpp_result_gen;
END_RCPP
}
// calSim
arma::mat calSim(arma::mat mat);
RcppExport SEXP _BClustLonG_calSim(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(calSim(mat));
    return rcpp_result_gen;
END_RCPP
}
