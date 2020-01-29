// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// distance_2
double distance_2(NumericMatrix data, int ndim, int j);
RcppExport SEXP prclust_distance_2(SEXP dataSEXP, SEXP ndimSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    __result = Rcpp::wrap(distance_2(data, ndim, j));
    return __result;
END_RCPP
}
// distance_umu
double distance_umu(NumericMatrix u, NumericMatrix data, int ndim, int i, int j, int uj);
RcppExport SEXP prclust_distance_umu(SEXP uSEXP, SEXP dataSEXP, SEXP ndimSEXP, SEXP iSEXP, SEXP jSEXP, SEXP ujSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type u(uSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    Rcpp::traits::input_parameter< int >::type uj(ujSEXP);
    __result = Rcpp::wrap(distance_umu(u, data, ndim, i, j, uj));
    return __result;
END_RCPP
}
// residual_mu
double residual_mu(NumericMatrix mu1, NumericMatrix mu, int ndim, int numbers);
RcppExport SEXP prclust_residual_mu(SEXP mu1SEXP, SEXP muSEXP, SEXP ndimSEXP, SEXP numbersSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type mu1(mu1SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type numbers(numbersSEXP);
    __result = Rcpp::wrap(residual_mu(mu1, mu, ndim, numbers));
    return __result;
END_RCPP
}
// is_zero_theta
int is_zero_theta(NumericMatrix theta, int j, int ndim);
RcppExport SEXP prclust_is_zero_theta(SEXP thetaSEXP, SEXP jSEXP, SEXP ndimSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    __result = Rcpp::wrap(is_zero_theta(theta, j, ndim));
    return __result;
END_RCPP
}
// stopping_criteria
int stopping_criteria(NumericMatrix mu, NumericMatrix mu1, int ndim, int numbers, int count, double epsilon);
RcppExport SEXP prclust_stopping_criteria(SEXP muSEXP, SEXP mu1SEXP, SEXP ndimSEXP, SEXP numbersSEXP, SEXP countSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type mu(muSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mu1(mu1SEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type numbers(numbersSEXP);
    Rcpp::traits::input_parameter< int >::type count(countSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    __result = Rcpp::wrap(stopping_criteria(mu, mu1, ndim, numbers, count, epsilon));
    return __result;
END_RCPP
}
// PRclustADMM
List PRclustADMM(NumericMatrix data, double rho, double lambda2, double tau, int mumethod, int methods, double epsilon);
RcppExport SEXP prclust_PRclustADMM(SEXP dataSEXP, SEXP rhoSEXP, SEXP lambda2SEXP, SEXP tauSEXP, SEXP mumethodSEXP, SEXP methodsSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type lambda2(lambda2SEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type mumethod(mumethodSEXP);
    Rcpp::traits::input_parameter< int >::type methods(methodsSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    __result = Rcpp::wrap(PRclustADMM(data, rho, lambda2, tau, mumethod, methods, epsilon));
    return __result;
END_RCPP
}
// clusterStat
List clusterStat(NumericVector trueGroup, NumericVector group);
RcppExport SEXP prclust_clusterStat(SEXP trueGroupSEXP, SEXP groupSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type trueGroup(trueGroupSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type group(groupSEXP);
    __result = Rcpp::wrap(clusterStat(trueGroup, group));
    return __result;
END_RCPP
}
// distance_mu
double distance_mu(NumericMatrix data, int ndim, int i, int j);
RcppExport SEXP prclust_distance_mu(SEXP dataSEXP, SEXP ndimSEXP, SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    __result = Rcpp::wrap(distance_mu(data, ndim, i, j));
    return __result;
END_RCPP
}
// cal_S
double cal_S(NumericMatrix data, NumericMatrix mu, NumericMatrix theta, double lambda1, double lambda2, double tau, int ndim, int numbers, int methods);
RcppExport SEXP prclust_cal_S(SEXP dataSEXP, SEXP muSEXP, SEXP thetaSEXP, SEXP lambda1SEXP, SEXP lambda2SEXP, SEXP tauSEXP, SEXP ndimSEXP, SEXP numbersSEXP, SEXP methodsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mu(muSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type lambda1(lambda1SEXP);
    Rcpp::traits::input_parameter< double >::type lambda2(lambda2SEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type numbers(numbersSEXP);
    Rcpp::traits::input_parameter< int >::type methods(methodsSEXP);
    __result = Rcpp::wrap(cal_S(data, mu, theta, lambda1, lambda2, tau, ndim, numbers, methods));
    return __result;
END_RCPP
}
// judge_iteration
int judge_iteration(NumericMatrix data, NumericMatrix mu, NumericMatrix theta, NumericMatrix mu1, NumericMatrix theta1, double lambda1, double lambda2, double tau, int ndim, int numbers, int count, int methods);
RcppExport SEXP prclust_judge_iteration(SEXP dataSEXP, SEXP muSEXP, SEXP thetaSEXP, SEXP mu1SEXP, SEXP theta1SEXP, SEXP lambda1SEXP, SEXP lambda2SEXP, SEXP tauSEXP, SEXP ndimSEXP, SEXP numbersSEXP, SEXP countSEXP, SEXP methodsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mu(muSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mu1(mu1SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type theta1(theta1SEXP);
    Rcpp::traits::input_parameter< double >::type lambda1(lambda1SEXP);
    Rcpp::traits::input_parameter< double >::type lambda2(lambda2SEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< int >::type numbers(numbersSEXP);
    Rcpp::traits::input_parameter< int >::type count(countSEXP);
    Rcpp::traits::input_parameter< int >::type methods(methodsSEXP);
    __result = Rcpp::wrap(judge_iteration(data, mu, theta, mu1, theta1, lambda1, lambda2, tau, ndim, numbers, count, methods));
    return __result;
END_RCPP
}
// PRclustOriginal
List PRclustOriginal(NumericMatrix data, double lambda1, double lambda2, double tau, int mumethod, int methods);
RcppExport SEXP prclust_PRclustOriginal(SEXP dataSEXP, SEXP lambda1SEXP, SEXP lambda2SEXP, SEXP tauSEXP, SEXP mumethodSEXP, SEXP methodsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type lambda1(lambda1SEXP);
    Rcpp::traits::input_parameter< double >::type lambda2(lambda2SEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type mumethod(mumethodSEXP);
    Rcpp::traits::input_parameter< int >::type methods(methodsSEXP);
    __result = Rcpp::wrap(PRclustOriginal(data, lambda1, lambda2, tau, mumethod, methods));
    return __result;
END_RCPP
}