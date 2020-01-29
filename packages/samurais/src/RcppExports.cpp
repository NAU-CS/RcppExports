// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// IRLS
List IRLS(arma::mat& X, arma::mat& Tau, arma::mat& Gamma, arma::mat& Winit, bool verbose);
RcppExport SEXP _samurais_IRLS(SEXP XSEXP, SEXP TauSEXP, SEXP GammaSEXP, SEXP WinitSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Tau(TauSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Gamma(GammaSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Winit(WinitSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(IRLS(X, Tau, Gamma, Winit, verbose));
    return rcpp_result_gen;
END_RCPP
}
// costMatrix
arma::mat costMatrix(arma::colvec& y, arma::mat& X, double Lmin);
RcppExport SEXP _samurais_costMatrix(SEXP ySEXP, SEXP XSEXP, SEXP LminSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type Lmin(LminSEXP);
    rcpp_result_gen = Rcpp::wrap(costMatrix(y, X, Lmin));
    return rcpp_result_gen;
END_RCPP
}
// forwardsBackwards
List forwardsBackwards(arma::vec& prior, arma::mat& transmat, arma::mat& f_tk);
RcppExport SEXP _samurais_forwardsBackwards(SEXP priorSEXP, SEXP transmatSEXP, SEXP f_tkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type prior(priorSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type transmat(transmatSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type f_tk(f_tkSEXP);
    rcpp_result_gen = Rcpp::wrap(forwardsBackwards(prior, transmat, f_tk));
    return rcpp_result_gen;
END_RCPP
}
// multinomialLogit
List multinomialLogit(arma::mat& W, arma::mat& X, arma::mat& Y, arma::mat& Gamma);
RcppExport SEXP _samurais_multinomialLogit(SEXP WSEXP, SEXP XSEXP, SEXP YSEXP, SEXP GammaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type W(WSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Gamma(GammaSEXP);
    rcpp_result_gen = Rcpp::wrap(multinomialLogit(W, X, Y, Gamma));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_samurais_IRLS", (DL_FUNC) &_samurais_IRLS, 5},
    {"_samurais_costMatrix", (DL_FUNC) &_samurais_costMatrix, 3},
    {"_samurais_forwardsBackwards", (DL_FUNC) &_samurais_forwardsBackwards, 3},
    {"_samurais_multinomialLogit", (DL_FUNC) &_samurais_multinomialLogit, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_samurais(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
