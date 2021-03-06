// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// M_Update_Rcpp
List M_Update_Rcpp(arma::mat Mean, arma::mat D, arma::mat Uinv, arma::mat Vinv, arma::vec nc, arma::mat weightmat, double rho, double lambda, int C, int r, double mtol);
RcppExport SEXP _MatrixLDA_M_Update_Rcpp(SEXP MeanSEXP, SEXP DSEXP, SEXP UinvSEXP, SEXP VinvSEXP, SEXP ncSEXP, SEXP weightmatSEXP, SEXP rhoSEXP, SEXP lambdaSEXP, SEXP CSEXP, SEXP rSEXP, SEXP mtolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Mean(MeanSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type D(DSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Uinv(UinvSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Vinv(VinvSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type nc(ncSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type weightmat(weightmatSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type C(CSEXP);
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type mtol(mtolSEXP);
    rcpp_result_gen = Rcpp::wrap(M_Update_Rcpp(Mean, D, Uinv, Vinv, nc, weightmat, rho, lambda, C, r, mtol));
    return rcpp_result_gen;
END_RCPP
}
