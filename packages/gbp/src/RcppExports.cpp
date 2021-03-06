// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// bpp_solver_dpp_wrapper
DataFrame bpp_solver_dpp_wrapper(DataFrame it, DataFrame bn);
RcppExport SEXP gbp_bpp_solver_dpp_wrapper(SEXP itSEXP, SEXP bnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type it(itSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type bn(bnSEXP);
    rcpp_result_gen = Rcpp::wrap(bpp_solver_dpp_wrapper(it, bn));
    return rcpp_result_gen;
END_RCPP
}
// gbp2d_solver_dpp_prep_create_p
arma::vec gbp2d_solver_dpp_prep_create_p(const arma::mat& ld, const arma::vec& m);
RcppExport SEXP gbp_gbp2d_solver_dpp_prep_create_p(SEXP ldSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type ld(ldSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(gbp2d_solver_dpp_prep_create_p(ld, m));
    return rcpp_result_gen;
END_RCPP
}
// gbp3d_solver_dpp_prep_create_p
arma::vec gbp3d_solver_dpp_prep_create_p(const arma::mat& ldh, const arma::vec& m);
RcppExport SEXP gbp_gbp3d_solver_dpp_prep_create_p(SEXP ldhSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type ldh(ldhSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(gbp3d_solver_dpp_prep_create_p(ldh, m));
    return rcpp_result_gen;
END_RCPP
}
// gbp4d_solver_dpp_prep_create_p
arma::vec gbp4d_solver_dpp_prep_create_p(const arma::mat& ldhw, const arma::vec& m);
RcppExport SEXP gbp_gbp4d_solver_dpp_prep_create_p(SEXP ldhwSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type ldhw(ldhwSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(gbp4d_solver_dpp_prep_create_p(ldhw, m));
    return rcpp_result_gen;
END_RCPP
}
