// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// CalcNablabeetaUbeta
double CalcNablabeetaUbeta(arma::vec theta, arma::vec tm, arma::vec event, arma::mat ps, arma::mat Z, arma::mat psDeriv);
RcppExport SEXP _ICcalib_CalcNablabeetaUbeta(SEXP thetaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP ZSEXP, SEXP psDerivSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type event(eventSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ps(psSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type psDeriv(psDerivSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcNablabeetaUbeta(theta, tm, event, ps, Z, psDeriv));
    return rcpp_result_gen;
END_RCPP
}
// CalcNablabeetaUgamma
arma::mat CalcNablabeetaUgamma(arma::vec theta, arma::vec tm, arma::vec event, arma::mat ps, arma::mat Z, arma::mat psDeriv);
RcppExport SEXP _ICcalib_CalcNablabeetaUgamma(SEXP thetaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP ZSEXP, SEXP psDerivSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type event(eventSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ps(psSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type psDeriv(psDerivSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcNablabeetaUgamma(theta, tm, event, ps, Z, psDeriv));
    return rcpp_result_gen;
END_RCPP
}
// CalcSurvFromNPMLE
NumericVector CalcSurvFromNPMLE(NumericVector probs, NumericVector points, NumericMatrix Tbull);
RcppExport SEXP _ICcalib_CalcSurvFromNPMLE(SEXP probsSEXP, SEXP pointsSEXP, SEXP TbullSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Tbull(TbullSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcSurvFromNPMLE(probs, points, Tbull));
    return rcpp_result_gen;
END_RCPP
}
// CalcUbetabeeta
double CalcUbetabeeta(double beta, NumericVector tm, LogicalVector event, NumericMatrix ps, NumericMatrix psDeriv);
RcppExport SEXP _ICcalib_CalcUbetabeeta(SEXP betaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP psDerivSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type event(eventSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ps(psSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type psDeriv(psDerivSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcUbetabeeta(beta, tm, event, ps, psDeriv));
    return rcpp_result_gen;
END_RCPP
}
// CalcUbetabeetaRS
NumericVector CalcUbetabeetaRS(double beta, NumericVector tm, LogicalVector event, NumericMatrix ps, NumericMatrix psDeriv);
RcppExport SEXP _ICcalib_CalcUbetabeetaRS(SEXP betaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP psDerivSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type event(eventSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ps(psSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type psDeriv(psDerivSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcUbetabeetaRS(beta, tm, event, ps, psDeriv));
    return rcpp_result_gen;
END_RCPP
}
// Calcb
NumericVector Calcb(double beta, NumericVector tm, LogicalVector event, NumericMatrix ps);
RcppExport SEXP _ICcalib_Calcb(SEXP betaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type event(eventSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ps(psSEXP);
    rcpp_result_gen = Rcpp::wrap(Calcb(beta, tm, event, ps));
    return rcpp_result_gen;
END_RCPP
}
// CalcbZ
arma::mat CalcbZ(arma::vec theta, arma::vec tm, arma::vec event, arma::mat ps, arma::mat Z);
RcppExport SEXP _ICcalib_CalcbZ(SEXP thetaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP ZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type event(eventSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ps(psSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcbZ(theta, tm, event, ps, Z));
    return rcpp_result_gen;
END_RCPP
}
// CoxLogLik
double CoxLogLik(arma::vec betagamma, arma::vec tm, arma::vec event, arma::mat ps, arma::mat Z);
RcppExport SEXP _ICcalib_CoxLogLik(SEXP betagammaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP ZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type betagamma(betagammaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type event(eventSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ps(psSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    rcpp_result_gen = Rcpp::wrap(CoxLogLik(betagamma, tm, event, ps, Z));
    return rcpp_result_gen;
END_RCPP
}
// CoxLogLikGrad
arma::vec CoxLogLikGrad(arma::vec theta, arma::vec tm, arma::vec event, arma::mat ps, arma::mat Z);
RcppExport SEXP _ICcalib_CoxLogLikGrad(SEXP thetaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP ZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type event(eventSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ps(psSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    rcpp_result_gen = Rcpp::wrap(CoxLogLikGrad(theta, tm, event, ps, Z));
    return rcpp_result_gen;
END_RCPP
}
// CoxLogLikHess
arma::mat CoxLogLikHess(arma::vec theta, arma::vec tm, arma::vec event, arma::mat ps, arma::mat Z);
RcppExport SEXP _ICcalib_CoxLogLikHess(SEXP thetaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP, SEXP ZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type event(eventSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ps(psSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    rcpp_result_gen = Rcpp::wrap(CoxLogLikHess(theta, tm, event, ps, Z));
    return rcpp_result_gen;
END_RCPP
}
// CoxLogLikNoBeta
double CoxLogLikNoBeta(arma::vec gamma, arma::vec tm, arma::vec event, arma::mat Z);
RcppExport SEXP _ICcalib_CoxLogLikNoBeta(SEXP gammaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP ZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type event(eventSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    rcpp_result_gen = Rcpp::wrap(CoxLogLikNoBeta(gamma, tm, event, Z));
    return rcpp_result_gen;
END_RCPP
}
// CoxLogLikX
double CoxLogLikX(double beta, NumericVector tm, LogicalVector event, NumericMatrix ps);
RcppExport SEXP _ICcalib_CoxLogLikX(SEXP betaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type event(eventSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ps(psSEXP);
    rcpp_result_gen = Rcpp::wrap(CoxLogLikX(beta, tm, event, ps));
    return rcpp_result_gen;
END_RCPP
}
// FindIntervalCPP
IntegerVector FindIntervalCPP(double point, NumericMatrix w);
RcppExport SEXP _ICcalib_FindIntervalCPP(SEXP pointSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type point(pointSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(FindIntervalCPP(point, w));
    return rcpp_result_gen;
END_RCPP
}
// FindIntervalCalibCPP
NumericMatrix FindIntervalCalibCPP(NumericMatrix w, NumericMatrix wres);
RcppExport SEXP _ICcalib_FindIntervalCalibCPP(SEXP wSEXP, SEXP wresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type w(wSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type wres(wresSEXP);
    rcpp_result_gen = Rcpp::wrap(FindIntervalCalibCPP(w, wres));
    return rcpp_result_gen;
END_RCPP
}
// FindIntervalCalibCPPvec
NumericVector FindIntervalCalibCPPvec(NumericVector w, NumericVector wres);
RcppExport SEXP _ICcalib_FindIntervalCalibCPPvec(SEXP wSEXP, SEXP wresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wres(wresSEXP);
    rcpp_result_gen = Rcpp::wrap(FindIntervalCalibCPPvec(w, wres));
    return rcpp_result_gen;
END_RCPP
}
// myFmyHess
double myFmyHess(double beta, NumericVector tm, LogicalVector event, NumericMatrix ps);
RcppExport SEXP _ICcalib_myFmyHess(SEXP betaSEXP, SEXP tmSEXP, SEXP eventSEXP, SEXP psSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tm(tmSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type event(eventSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ps(psSEXP);
    rcpp_result_gen = Rcpp::wrap(myFmyHess(beta, tm, event, ps));
    return rcpp_result_gen;
END_RCPP
}
