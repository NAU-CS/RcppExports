// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// EMInterwovenFastICA
List EMInterwovenFastICA(const arma::mat& DataMtr, const arma::mat& MembershipProbs_, double bandWidth, int maxIteration, int icaIteration, double tolerance, bool verbose, bool combine);
RcppExport SEXP _icamix_EMInterwovenFastICA(SEXP DataMtrSEXP, SEXP MembershipProbs_SEXP, SEXP bandWidthSEXP, SEXP maxIterationSEXP, SEXP icaIterationSEXP, SEXP toleranceSEXP, SEXP verboseSEXP, SEXP combineSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type DataMtr(DataMtrSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type MembershipProbs_(MembershipProbs_SEXP);
    Rcpp::traits::input_parameter< double >::type bandWidth(bandWidthSEXP);
    Rcpp::traits::input_parameter< int >::type maxIteration(maxIterationSEXP);
    Rcpp::traits::input_parameter< int >::type icaIteration(icaIterationSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< bool >::type combine(combineSEXP);
    rcpp_result_gen = Rcpp::wrap(EMInterwovenFastICA(DataMtr, MembershipProbs_, bandWidth, maxIteration, icaIteration, tolerance, verbose, combine));
    return rcpp_result_gen;
END_RCPP
}
// WtsFastICA
List WtsFastICA(const arma::mat& X, const arma::vec& wts, const arma::mat& Wconst, bool verbose, double alpha, int maxIteration, double tolerance);
RcppExport SEXP _icamix_WtsFastICA(SEXP XSEXP, SEXP wtsSEXP, SEXP WconstSEXP, SEXP verboseSEXP, SEXP alphaSEXP, SEXP maxIterationSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type wts(wtsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Wconst(WconstSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type maxIteration(maxIterationSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(WtsFastICA(X, wts, Wconst, verbose, alpha, maxIteration, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// WtsKde
List WtsKde(const arma::vec& X, const arma::vec& wts, const arma::vec& grid, double h);
RcppExport SEXP _icamix_WtsKde(SEXP XSEXP, SEXP wtsSEXP, SEXP gridSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type wts(wtsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type grid(gridSEXP);
    Rcpp::traits::input_parameter< double >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(WtsKde(X, wts, grid, h));
    return rcpp_result_gen;
END_RCPP
}
