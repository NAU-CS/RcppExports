// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// avg_rank
NumericVector avg_rank(Rcpp::NumericVector x);
RcppExport SEXP _ModelMetrics_avg_rank(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(avg_rank(x));
    return rcpp_result_gen;
END_RCPP
}
// auc_
double auc_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_auc_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(auc_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// auc2_
double auc2_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_auc2_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(auc2_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// auc3_
double auc3_(NumericVector actual, NumericVector predicted, NumericVector ranks);
RcppExport SEXP _ModelMetrics_auc3_(SEXP actualSEXP, SEXP predictedSEXP, SEXP ranksSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ranks(ranksSEXP);
    rcpp_result_gen = Rcpp::wrap(auc3_(actual, predicted, ranks));
    return rcpp_result_gen;
END_RCPP
}
// confusionMatrix_
NumericMatrix confusionMatrix_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_confusionMatrix_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(confusionMatrix_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// ppv_
double ppv_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_ppv_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(ppv_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// npv_
double npv_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_npv_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(npv_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// tnr_
double tnr_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_tnr_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(tnr_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// recall_
double recall_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_recall_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(recall_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// fScore_
double fScore_(NumericVector actual, NumericVector predicted, double cutoff, double beta);
RcppExport SEXP _ModelMetrics_fScore_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(fScore_(actual, predicted, cutoff, beta));
    return rcpp_result_gen;
END_RCPP
}
// f1Score_
double f1Score_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_f1Score_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(f1Score_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// brier_
double brier_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_brier_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(brier_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// mcc_
double mcc_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_mcc_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(mcc_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// kappa_
double kappa_(NumericVector actual, NumericVector predicted, double cutoff);
RcppExport SEXP _ModelMetrics_kappa_(SEXP actualSEXP, SEXP predictedSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(kappa_(actual, predicted, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// mae_
double mae_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_mae_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(mae_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// ce_
double ce_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_ce_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(ce_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// mse_
double mse_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_mse_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(mse_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// msle_
double msle_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_msle_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(msle_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// rmsle_
double rmsle_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_rmsle_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(rmsle_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// rmse_
double rmse_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_rmse_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(rmse_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// gini_
double gini_(NumericVector actual);
RcppExport SEXP _ModelMetrics_gini_(SEXP actualSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    rcpp_result_gen = Rcpp::wrap(gini_(actual));
    return rcpp_result_gen;
END_RCPP
}
// logLoss_
double logLoss_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_logLoss_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(logLoss_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// mlogLoss_
double mlogLoss_(NumericVector actual, NumericMatrix predicted);
RcppExport SEXP _ModelMetrics_mlogLoss_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(mlogLoss_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}
// plogLoss_
double plogLoss_(NumericVector actual, NumericVector predicted);
RcppExport SEXP _ModelMetrics_plogLoss_(SEXP actualSEXP, SEXP predictedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type actual(actualSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type predicted(predictedSEXP);
    rcpp_result_gen = Rcpp::wrap(plogLoss_(actual, predicted));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ModelMetrics_avg_rank", (DL_FUNC) &_ModelMetrics_avg_rank, 1},
    {"_ModelMetrics_auc_", (DL_FUNC) &_ModelMetrics_auc_, 2},
    {"_ModelMetrics_auc2_", (DL_FUNC) &_ModelMetrics_auc2_, 2},
    {"_ModelMetrics_auc3_", (DL_FUNC) &_ModelMetrics_auc3_, 3},
    {"_ModelMetrics_confusionMatrix_", (DL_FUNC) &_ModelMetrics_confusionMatrix_, 3},
    {"_ModelMetrics_ppv_", (DL_FUNC) &_ModelMetrics_ppv_, 3},
    {"_ModelMetrics_npv_", (DL_FUNC) &_ModelMetrics_npv_, 3},
    {"_ModelMetrics_tnr_", (DL_FUNC) &_ModelMetrics_tnr_, 3},
    {"_ModelMetrics_recall_", (DL_FUNC) &_ModelMetrics_recall_, 3},
    {"_ModelMetrics_fScore_", (DL_FUNC) &_ModelMetrics_fScore_, 4},
    {"_ModelMetrics_f1Score_", (DL_FUNC) &_ModelMetrics_f1Score_, 3},
    {"_ModelMetrics_brier_", (DL_FUNC) &_ModelMetrics_brier_, 2},
    {"_ModelMetrics_mcc_", (DL_FUNC) &_ModelMetrics_mcc_, 3},
    {"_ModelMetrics_kappa_", (DL_FUNC) &_ModelMetrics_kappa_, 3},
    {"_ModelMetrics_mae_", (DL_FUNC) &_ModelMetrics_mae_, 2},
    {"_ModelMetrics_ce_", (DL_FUNC) &_ModelMetrics_ce_, 2},
    {"_ModelMetrics_mse_", (DL_FUNC) &_ModelMetrics_mse_, 2},
    {"_ModelMetrics_msle_", (DL_FUNC) &_ModelMetrics_msle_, 2},
    {"_ModelMetrics_rmsle_", (DL_FUNC) &_ModelMetrics_rmsle_, 2},
    {"_ModelMetrics_rmse_", (DL_FUNC) &_ModelMetrics_rmse_, 2},
    {"_ModelMetrics_gini_", (DL_FUNC) &_ModelMetrics_gini_, 1},
    {"_ModelMetrics_logLoss_", (DL_FUNC) &_ModelMetrics_logLoss_, 2},
    {"_ModelMetrics_mlogLoss_", (DL_FUNC) &_ModelMetrics_mlogLoss_, 2},
    {"_ModelMetrics_plogLoss_", (DL_FUNC) &_ModelMetrics_plogLoss_, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_ModelMetrics(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}