// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// upward_downward_BM
Rcpp::List upward_downward_BM(arma::mat const& data, arma::umat const& ed, arma::mat const& Delta, arma::mat const& Variance, arma::vec const& edge_length, Rcpp::List root_state_list);
RcppExport SEXP _PhylogeneticEM_upward_downward_BM(SEXP dataSEXP, SEXP edSEXP, SEXP DeltaSEXP, SEXP VarianceSEXP, SEXP edge_lengthSEXP, SEXP root_state_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat const& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::umat const& >::type ed(edSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Delta(DeltaSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Variance(VarianceSEXP);
    Rcpp::traits::input_parameter< arma::vec const& >::type edge_length(edge_lengthSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type root_state_list(root_state_listSEXP);
    rcpp_result_gen = Rcpp::wrap(upward_downward_BM(data, ed, Delta, Variance, edge_length, root_state_list));
    return rcpp_result_gen;
END_RCPP
}
// upward_downward_OU
Rcpp::List upward_downward_OU(arma::mat const& data, arma::umat const& ed, arma::mat const& Beta, arma::mat const& Stationary_Var, arma::vec const& edge_length, arma::mat const& Alpha, Rcpp::List root_state_list);
RcppExport SEXP _PhylogeneticEM_upward_downward_OU(SEXP dataSEXP, SEXP edSEXP, SEXP BetaSEXP, SEXP Stationary_VarSEXP, SEXP edge_lengthSEXP, SEXP AlphaSEXP, SEXP root_state_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat const& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::umat const& >::type ed(edSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Beta(BetaSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Stationary_Var(Stationary_VarSEXP);
    Rcpp::traits::input_parameter< arma::vec const& >::type edge_length(edge_lengthSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Alpha(AlphaSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type root_state_list(root_state_listSEXP);
    rcpp_result_gen = Rcpp::wrap(upward_downward_OU(data, ed, Beta, Stationary_Var, edge_length, Alpha, root_state_list));
    return rcpp_result_gen;
END_RCPP
}
// log_likelihood_BM
double log_likelihood_BM(arma::mat const& data, arma::umat const& ed, arma::mat const& Delta, arma::mat const& Variance, arma::vec const& edge_length, Rcpp::List root_state_list);
RcppExport SEXP _PhylogeneticEM_log_likelihood_BM(SEXP dataSEXP, SEXP edSEXP, SEXP DeltaSEXP, SEXP VarianceSEXP, SEXP edge_lengthSEXP, SEXP root_state_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat const& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::umat const& >::type ed(edSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Delta(DeltaSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Variance(VarianceSEXP);
    Rcpp::traits::input_parameter< arma::vec const& >::type edge_length(edge_lengthSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type root_state_list(root_state_listSEXP);
    rcpp_result_gen = Rcpp::wrap(log_likelihood_BM(data, ed, Delta, Variance, edge_length, root_state_list));
    return rcpp_result_gen;
END_RCPP
}
// log_likelihood_OU
double log_likelihood_OU(arma::mat const& data, arma::umat const& ed, arma::mat const& Beta, arma::mat const& Stationary_Var, arma::vec const& edge_length, arma::mat const& Alpha, Rcpp::List root_state_list);
RcppExport SEXP _PhylogeneticEM_log_likelihood_OU(SEXP dataSEXP, SEXP edSEXP, SEXP BetaSEXP, SEXP Stationary_VarSEXP, SEXP edge_lengthSEXP, SEXP AlphaSEXP, SEXP root_state_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat const& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::umat const& >::type ed(edSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Beta(BetaSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Stationary_Var(Stationary_VarSEXP);
    Rcpp::traits::input_parameter< arma::vec const& >::type edge_length(edge_lengthSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Alpha(AlphaSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type root_state_list(root_state_listSEXP);
    rcpp_result_gen = Rcpp::wrap(log_likelihood_OU(data, ed, Beta, Stationary_Var, edge_length, Alpha, root_state_list));
    return rcpp_result_gen;
END_RCPP
}
