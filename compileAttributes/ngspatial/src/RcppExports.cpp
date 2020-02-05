// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// bmse
double bmse(const vec& vals);
RcppExport SEXP _ngspatial_bmse(SEXP valsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const vec& >::type vals(valsSEXP);
    rcpp_result_gen = Rcpp::wrap(bmse(vals));
    return rcpp_result_gen;
END_RCPP
}
// rautologistic_
vec rautologistic_(const mat& X, const mat& A, const vec& theta);
RcppExport SEXP _ngspatial_rautologistic_(SEXP XSEXP, SEXP ASEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const vec& >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(rautologistic_(X, A, theta));
    return rcpp_result_gen;
END_RCPP
}
// randWalk
mat randWalk(const mat& X, const mat& A, const colvec& Z, const colvec& theta, double tol, int minit, int maxit, const colvec& sigma, const colvec& etaRange, const mat& V, bool verbose);
RcppExport SEXP _ngspatial_randWalk(SEXP XSEXP, SEXP ASEXP, SEXP ZSEXP, SEXP thetaSEXP, SEXP tolSEXP, SEXP minitSEXP, SEXP maxitSEXP, SEXP sigmaSEXP, SEXP etaRangeSEXP, SEXP VSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const colvec& >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< const colvec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type minit(minitSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< const colvec& >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< const colvec& >::type etaRange(etaRangeSEXP);
    Rcpp::traits::input_parameter< const mat& >::type V(VSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(randWalk(X, A, Z, theta, tol, minit, maxit, sigma, etaRange, V, verbose));
    return rcpp_result_gen;
END_RCPP
}
// randWalkTrain
mat randWalkTrain(const mat& X, const mat& A, const colvec& Z, const colvec& theta, int trainit, const colvec& sigma, const colvec& etaRange, const mat& V, bool verbose);
RcppExport SEXP _ngspatial_randWalkTrain(SEXP XSEXP, SEXP ASEXP, SEXP ZSEXP, SEXP thetaSEXP, SEXP trainitSEXP, SEXP sigmaSEXP, SEXP etaRangeSEXP, SEXP VSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const colvec& >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< const colvec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< int >::type trainit(trainitSEXP);
    Rcpp::traits::input_parameter< const colvec& >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< const colvec& >::type etaRange(etaRangeSEXP);
    Rcpp::traits::input_parameter< const mat& >::type V(VSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(randWalkTrain(X, A, Z, theta, trainit, sigma, etaRange, V, verbose));
    return rcpp_result_gen;
END_RCPP
}
