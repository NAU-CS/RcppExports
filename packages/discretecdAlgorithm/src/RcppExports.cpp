// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// CD
List CD(int node, int dataSize, Eigen::Map<Eigen::MatrixXi> data, Eigen::Map<Eigen::VectorXi> nlevels, List obsIndex_R, int eor_nr, Eigen::Map<Eigen::MatrixXi> eor, Eigen::Map<Eigen::VectorXd> lambda_seq, int nlam, double eps, double convLb, double qtol, Eigen::Map<Eigen::MatrixXd> weights, double gamma, double upperbound, int threshold);
RcppExport SEXP _discretecdAlgorithm_CD(SEXP nodeSEXP, SEXP dataSizeSEXP, SEXP dataSEXP, SEXP nlevelsSEXP, SEXP obsIndex_RSEXP, SEXP eor_nrSEXP, SEXP eorSEXP, SEXP lambda_seqSEXP, SEXP nlamSEXP, SEXP epsSEXP, SEXP convLbSEXP, SEXP qtolSEXP, SEXP weightsSEXP, SEXP gammaSEXP, SEXP upperboundSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< int >::type dataSize(dataSizeSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXi> >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXi> >::type nlevels(nlevelsSEXP);
    Rcpp::traits::input_parameter< List >::type obsIndex_R(obsIndex_RSEXP);
    Rcpp::traits::input_parameter< int >::type eor_nr(eor_nrSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXi> >::type eor(eorSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXd> >::type lambda_seq(lambda_seqSEXP);
    Rcpp::traits::input_parameter< int >::type nlam(nlamSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< double >::type convLb(convLbSEXP);
    Rcpp::traits::input_parameter< double >::type qtol(qtolSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type upperbound(upperboundSEXP);
    Rcpp::traits::input_parameter< int >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(CD(node, dataSize, data, nlevels, obsIndex_R, eor_nr, eor, lambda_seq, nlam, eps, convLb, qtol, weights, gamma, upperbound, threshold));
    return rcpp_result_gen;
END_RCPP
}
// lambdaMax
double lambdaMax(int node, int dataSize, Eigen::Map<Eigen::MatrixXi> data, Eigen::Map<Eigen::VectorXi> nlevels, List obsIndex_R, Eigen::Map<Eigen::MatrixXd> weights, double gamma, double upperbound);
RcppExport SEXP _discretecdAlgorithm_lambdaMax(SEXP nodeSEXP, SEXP dataSizeSEXP, SEXP dataSEXP, SEXP nlevelsSEXP, SEXP obsIndex_RSEXP, SEXP weightsSEXP, SEXP gammaSEXP, SEXP upperboundSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< int >::type dataSize(dataSizeSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXi> >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXi> >::type nlevels(nlevelsSEXP);
    Rcpp::traits::input_parameter< List >::type obsIndex_R(obsIndex_RSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type upperbound(upperboundSEXP);
    rcpp_result_gen = Rcpp::wrap(lambdaMax(node, dataSize, data, nlevels, obsIndex_R, weights, gamma, upperbound));
    return rcpp_result_gen;
END_RCPP
}
// DatGen
IntegerMatrix DatGen(int maxdeg, int node, Eigen::Map<Eigen::MatrixXi> ordex, IntegerVector ts, int dataSize, List ivn, List ivn_vals, bool ivn_rand, IntegerVector coef_length, Eigen::Map<Eigen::VectorXi> nlevels, List coef);
RcppExport SEXP _discretecdAlgorithm_DatGen(SEXP maxdegSEXP, SEXP nodeSEXP, SEXP ordexSEXP, SEXP tsSEXP, SEXP dataSizeSEXP, SEXP ivnSEXP, SEXP ivn_valsSEXP, SEXP ivn_randSEXP, SEXP coef_lengthSEXP, SEXP nlevelsSEXP, SEXP coefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type maxdeg(maxdegSEXP);
    Rcpp::traits::input_parameter< int >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXi> >::type ordex(ordexSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ts(tsSEXP);
    Rcpp::traits::input_parameter< int >::type dataSize(dataSizeSEXP);
    Rcpp::traits::input_parameter< List >::type ivn(ivnSEXP);
    Rcpp::traits::input_parameter< List >::type ivn_vals(ivn_valsSEXP);
    Rcpp::traits::input_parameter< bool >::type ivn_rand(ivn_randSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type coef_length(coef_lengthSEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXi> >::type nlevels(nlevelsSEXP);
    Rcpp::traits::input_parameter< List >::type coef(coefSEXP);
    rcpp_result_gen = Rcpp::wrap(DatGen(maxdeg, node, ordex, ts, dataSize, ivn, ivn_vals, ivn_rand, coef_length, nlevels, coef));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_discretecdAlgorithm_CD", (DL_FUNC) &_discretecdAlgorithm_CD, 16},
    {"_discretecdAlgorithm_lambdaMax", (DL_FUNC) &_discretecdAlgorithm_lambdaMax, 8},
    {"_discretecdAlgorithm_DatGen", (DL_FUNC) &_discretecdAlgorithm_DatGen, 11},
    {NULL, NULL, 0}
};

RcppExport void R_init_discretecdAlgorithm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
