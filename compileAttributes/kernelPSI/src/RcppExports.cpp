// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// adaFOHSIC
List adaFOHSIC(arma::field<arma::mat> K, arma::mat L);
RcppExport SEXP _kernelPSI_adaFOHSIC(SEXP KSEXP, SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat> >::type K(KSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type L(LSEXP);
    rcpp_result_gen = Rcpp::wrap(adaFOHSIC(K, L));
    return rcpp_result_gen;
END_RCPP
}
// adaQ
arma::field<arma::mat> adaQ(arma::field<arma::mat> K, IntegerVector select, int n);
RcppExport SEXP _kernelPSI_adaQ(SEXP KSEXP, SEXP selectSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat> >::type K(KSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type select(selectSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(adaQ(K, select, n));
    return rcpp_result_gen;
END_RCPP
}
// FOHSIC
IntegerVector FOHSIC(arma::field<arma::mat> K, arma::mat L, int mKernels);
RcppExport SEXP _kernelPSI_FOHSIC(SEXP KSEXP, SEXP LSEXP, SEXP mKernelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat> >::type K(KSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type mKernels(mKernelsSEXP);
    rcpp_result_gen = Rcpp::wrap(FOHSIC(K, L, mKernels));
    return rcpp_result_gen;
END_RCPP
}
// forwardQ
arma::field<arma::mat> forwardQ(arma::field<arma::mat> K, IntegerVector select);
RcppExport SEXP _kernelPSI_forwardQ(SEXP KSEXP, SEXP selectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat> >::type K(KSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type select(selectSEXP);
    rcpp_result_gen = Rcpp::wrap(forwardQ(K, select));
    return rcpp_result_gen;
END_RCPP
}
// HSIC
double HSIC(arma::mat K, arma::mat L);
RcppExport SEXP _kernelPSI_HSIC(SEXP KSEXP, SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type K(KSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type L(LSEXP);
    rcpp_result_gen = Rcpp::wrap(HSIC(K, L));
    return rcpp_result_gen;
END_RCPP
}
// quadHSIC
arma::mat quadHSIC(arma::mat K);
RcppExport SEXP _kernelPSI_quadHSIC(SEXP KSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type K(KSEXP);
    rcpp_result_gen = Rcpp::wrap(quadHSIC(K));
    return rcpp_result_gen;
END_RCPP
}
// sampleH
arma::mat sampleH(arma::field<arma::mat> A, NumericVector initial, int n_replicates, double mu, double sigma, int n_iter, int burn_in);
RcppExport SEXP _kernelPSI_sampleH(SEXP ASEXP, SEXP initialSEXP, SEXP n_replicatesSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP n_iterSEXP, SEXP burn_inSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat> >::type A(ASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type initial(initialSEXP);
    Rcpp::traits::input_parameter< int >::type n_replicates(n_replicatesSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< int >::type n_iter(n_iterSEXP);
    Rcpp::traits::input_parameter< int >::type burn_in(burn_inSEXP);
    rcpp_result_gen = Rcpp::wrap(sampleH(A, initial, n_replicates, mu, sigma, n_iter, burn_in));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_kernelPSI_adaFOHSIC", (DL_FUNC) &_kernelPSI_adaFOHSIC, 2},
    {"_kernelPSI_adaQ", (DL_FUNC) &_kernelPSI_adaQ, 3},
    {"_kernelPSI_FOHSIC", (DL_FUNC) &_kernelPSI_FOHSIC, 3},
    {"_kernelPSI_forwardQ", (DL_FUNC) &_kernelPSI_forwardQ, 2},
    {"_kernelPSI_HSIC", (DL_FUNC) &_kernelPSI_HSIC, 2},
    {"_kernelPSI_quadHSIC", (DL_FUNC) &_kernelPSI_quadHSIC, 1},
    {"_kernelPSI_sampleH", (DL_FUNC) &_kernelPSI_sampleH, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_kernelPSI(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}