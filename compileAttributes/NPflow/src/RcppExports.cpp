// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// FmeasureC
double FmeasureC(NumericVector pred, NumericVector ref);
RcppExport SEXP _NPflow_FmeasureC(SEXP predSEXP, SEXP refSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ref(refSEXP);
    rcpp_result_gen = Rcpp::wrap(FmeasureC(pred, ref));
    return rcpp_result_gen;
END_RCPP
}
// FmeasureC_no0
double FmeasureC_no0(NumericVector pred, NumericVector ref);
RcppExport SEXP _NPflow_FmeasureC_no0(SEXP predSEXP, SEXP refSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ref(refSEXP);
    rcpp_result_gen = Rcpp::wrap(FmeasureC_no0(pred, ref));
    return rcpp_result_gen;
END_RCPP
}
// Fmeasure_costC
List Fmeasure_costC(arma::mat c);
RcppExport SEXP _NPflow_Fmeasure_costC(SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(Fmeasure_costC(c));
    return rcpp_result_gen;
END_RCPP
}
// NuMatParC
List NuMatParC(NumericVector c, arma::mat d);
RcppExport SEXP _NPflow_NuMatParC(SEXP cSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type c(cSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(NuMatParC(c, d));
    return rcpp_result_gen;
END_RCPP
}
// traceEpsC
NumericVector traceEpsC(arma::mat eps, arma::mat sigma);
RcppExport SEXP _NPflow_traceEpsC(SEXP epsSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(traceEpsC(eps, sigma));
    return rcpp_result_gen;
END_RCPP
}
// mmNiWpdfC
NumericMatrix mmNiWpdfC(arma::mat Mu, List Sigma, arma::mat U_Mu0, NumericVector U_Kappa0, NumericVector U_Nu0, List U_Sigma0, bool Log);
RcppExport SEXP _NPflow_mmNiWpdfC(SEXP MuSEXP, SEXP SigmaSEXP, SEXP U_Mu0SEXP, SEXP U_Kappa0SEXP, SEXP U_Nu0SEXP, SEXP U_Sigma0SEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Mu(MuSEXP);
    Rcpp::traits::input_parameter< List >::type Sigma(SigmaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type U_Mu0(U_Mu0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U_Kappa0(U_Kappa0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U_Nu0(U_Nu0SEXP);
    Rcpp::traits::input_parameter< List >::type U_Sigma0(U_Sigma0SEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(mmNiWpdfC(Mu, Sigma, U_Mu0, U_Kappa0, U_Nu0, U_Sigma0, Log));
    return rcpp_result_gen;
END_RCPP
}
// mmsNiWpdfC
NumericMatrix mmsNiWpdfC(arma::mat xi, arma::mat psi, List Sigma, arma::mat U_xi0, arma::mat U_psi0, List U_B0, List U_Sigma0, NumericVector U_df0, bool Log);
RcppExport SEXP _NPflow_mmsNiWpdfC(SEXP xiSEXP, SEXP psiSEXP, SEXP SigmaSEXP, SEXP U_xi0SEXP, SEXP U_psi0SEXP, SEXP U_B0SEXP, SEXP U_Sigma0SEXP, SEXP U_df0SEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< List >::type Sigma(SigmaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type U_xi0(U_xi0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type U_psi0(U_psi0SEXP);
    Rcpp::traits::input_parameter< List >::type U_B0(U_B0SEXP);
    Rcpp::traits::input_parameter< List >::type U_Sigma0(U_Sigma0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U_df0(U_df0SEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(mmsNiWpdfC(xi, psi, Sigma, U_xi0, U_psi0, U_B0, U_Sigma0, U_df0, Log));
    return rcpp_result_gen;
END_RCPP
}
// mmvnpdfC
NumericMatrix mmvnpdfC(arma::mat x, arma::mat mean, List varcovM, bool Log);
RcppExport SEXP _NPflow_mmvnpdfC(SEXP xSEXP, SEXP meanSEXP, SEXP varcovMSEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< List >::type varcovM(varcovMSEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(mmvnpdfC(x, mean, varcovM, Log));
    return rcpp_result_gen;
END_RCPP
}
// mmvsnpdfC
NumericMatrix mmvsnpdfC(arma::mat x, arma::mat xi, arma::mat psi, List sigma, bool Log);
RcppExport SEXP _NPflow_mmvsnpdfC(SEXP xSEXP, SEXP xiSEXP, SEXP psiSEXP, SEXP sigmaSEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< List >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(mmvsnpdfC(x, xi, psi, sigma, Log));
    return rcpp_result_gen;
END_RCPP
}
// mmvstpdfC
NumericMatrix mmvstpdfC(arma::mat x, arma::mat xi, arma::mat psi, List sigma, NumericVector df, bool Log);
RcppExport SEXP _NPflow_mmvstpdfC(SEXP xSEXP, SEXP xiSEXP, SEXP psiSEXP, SEXP sigmaSEXP, SEXP dfSEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< List >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type df(dfSEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(mmvstpdfC(x, xi, psi, sigma, df, Log));
    return rcpp_result_gen;
END_RCPP
}
// mmvtpdfC
NumericMatrix mmvtpdfC(NumericMatrix x, NumericMatrix mean, List varcovM, NumericVector df, bool Log);
RcppExport SEXP _NPflow_mmvtpdfC(SEXP xSEXP, SEXP meanSEXP, SEXP varcovMSEXP, SEXP dfSEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< List >::type varcovM(varcovMSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type df(dfSEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(mmvtpdfC(x, mean, varcovM, df, Log));
    return rcpp_result_gen;
END_RCPP
}
// mvnlikC
List mvnlikC(arma::mat x, arma::vec c, arma::vec clustval, arma::mat mu, List sigma, bool loglik);
RcppExport SEXP _NPflow_mvnlikC(SEXP xSEXP, SEXP cSEXP, SEXP clustvalSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP loglikSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type c(cSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type clustval(clustvalSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mu(muSEXP);
    Rcpp::traits::input_parameter< List >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< bool >::type loglik(loglikSEXP);
    rcpp_result_gen = Rcpp::wrap(mvnlikC(x, c, clustval, mu, sigma, loglik));
    return rcpp_result_gen;
END_RCPP
}
// mvnpdfC
NumericVector mvnpdfC(NumericMatrix x, NumericVector mean, NumericMatrix varcovM, bool Log);
RcppExport SEXP _NPflow_mvnpdfC(SEXP xSEXP, SEXP meanSEXP, SEXP varcovMSEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type varcovM(varcovMSEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(mvnpdfC(x, mean, varcovM, Log));
    return rcpp_result_gen;
END_RCPP
}
// mvsnlikC
List mvsnlikC(arma::mat x, arma::vec c, arma::vec clustval, arma::mat xi, arma::mat psi, List sigma, bool loglik);
RcppExport SEXP _NPflow_mvsnlikC(SEXP xSEXP, SEXP cSEXP, SEXP clustvalSEXP, SEXP xiSEXP, SEXP psiSEXP, SEXP sigmaSEXP, SEXP loglikSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type c(cSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type clustval(clustvalSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< List >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< bool >::type loglik(loglikSEXP);
    rcpp_result_gen = Rcpp::wrap(mvsnlikC(x, c, clustval, xi, psi, sigma, loglik));
    return rcpp_result_gen;
END_RCPP
}
// mvstlikC
List mvstlikC(arma::mat x, arma::vec c, arma::vec clustval, arma::mat xi, arma::mat psi, List sigma, NumericVector df, bool loglik);
RcppExport SEXP _NPflow_mvstlikC(SEXP xSEXP, SEXP cSEXP, SEXP clustvalSEXP, SEXP xiSEXP, SEXP psiSEXP, SEXP sigmaSEXP, SEXP dfSEXP, SEXP loglikSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type c(cSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type clustval(clustvalSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< List >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type df(dfSEXP);
    Rcpp::traits::input_parameter< bool >::type loglik(loglikSEXP);
    rcpp_result_gen = Rcpp::wrap(mvstlikC(x, c, clustval, xi, psi, sigma, df, loglik));
    return rcpp_result_gen;
END_RCPP
}
// sampleClassC
IntegerVector sampleClassC(arma::mat probMat, bool Log);
RcppExport SEXP _NPflow_sampleClassC(SEXP probMatSEXP, SEXP LogSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type probMat(probMatSEXP);
    Rcpp::traits::input_parameter< bool >::type Log(LogSEXP);
    rcpp_result_gen = Rcpp::wrap(sampleClassC(probMat, Log));
    return rcpp_result_gen;
END_RCPP
}
// similarityMatC
List similarityMatC(arma::mat cc);
RcppExport SEXP _NPflow_similarityMatC(SEXP ccSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type cc(ccSEXP);
    rcpp_result_gen = Rcpp::wrap(similarityMatC(cc));
    return rcpp_result_gen;
END_RCPP
}
// similarityMat_nocostC
List similarityMat_nocostC(arma::mat cc);
RcppExport SEXP _NPflow_similarityMat_nocostC(SEXP ccSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type cc(ccSEXP);
    rcpp_result_gen = Rcpp::wrap(similarityMat_nocostC(cc));
    return rcpp_result_gen;
END_RCPP
}
// vclust2mcoclustC
List vclust2mcoclustC(NumericVector c);
RcppExport SEXP _NPflow_vclust2mcoclustC(SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(vclust2mcoclustC(c));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_NPflow_FmeasureC", (DL_FUNC) &_NPflow_FmeasureC, 2},
    {"_NPflow_FmeasureC_no0", (DL_FUNC) &_NPflow_FmeasureC_no0, 2},
    {"_NPflow_Fmeasure_costC", (DL_FUNC) &_NPflow_Fmeasure_costC, 1},
    {"_NPflow_NuMatParC", (DL_FUNC) &_NPflow_NuMatParC, 2},
    {"_NPflow_traceEpsC", (DL_FUNC) &_NPflow_traceEpsC, 2},
    {"_NPflow_mmNiWpdfC", (DL_FUNC) &_NPflow_mmNiWpdfC, 7},
    {"_NPflow_mmsNiWpdfC", (DL_FUNC) &_NPflow_mmsNiWpdfC, 9},
    {"_NPflow_mmvnpdfC", (DL_FUNC) &_NPflow_mmvnpdfC, 4},
    {"_NPflow_mmvsnpdfC", (DL_FUNC) &_NPflow_mmvsnpdfC, 5},
    {"_NPflow_mmvstpdfC", (DL_FUNC) &_NPflow_mmvstpdfC, 6},
    {"_NPflow_mmvtpdfC", (DL_FUNC) &_NPflow_mmvtpdfC, 5},
    {"_NPflow_mvnlikC", (DL_FUNC) &_NPflow_mvnlikC, 6},
    {"_NPflow_mvnpdfC", (DL_FUNC) &_NPflow_mvnpdfC, 4},
    {"_NPflow_mvsnlikC", (DL_FUNC) &_NPflow_mvsnlikC, 7},
    {"_NPflow_mvstlikC", (DL_FUNC) &_NPflow_mvstlikC, 8},
    {"_NPflow_sampleClassC", (DL_FUNC) &_NPflow_sampleClassC, 2},
    {"_NPflow_similarityMatC", (DL_FUNC) &_NPflow_similarityMatC, 1},
    {"_NPflow_similarityMat_nocostC", (DL_FUNC) &_NPflow_similarityMat_nocostC, 1},
    {"_NPflow_vclust2mcoclustC", (DL_FUNC) &_NPflow_vclust2mcoclustC, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_NPflow(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
