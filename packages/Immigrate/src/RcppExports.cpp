// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// BIMCpp
List BIMCpp(Function oneboostImmigrate, NumericMatrix train_xx, NumericVector train_yy, int nIter, int max_iter, bool removesmall, double sigstart, double sigend);
RcppExport SEXP _Immigrate_BIMCpp(SEXP oneboostImmigrateSEXP, SEXP train_xxSEXP, SEXP train_yySEXP, SEXP nIterSEXP, SEXP max_iterSEXP, SEXP removesmallSEXP, SEXP sigstartSEXP, SEXP sigendSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Function >::type oneboostImmigrate(oneboostImmigrateSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type train_xx(train_xxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type train_yy(train_yySEXP);
    Rcpp::traits::input_parameter< int >::type nIter(nIterSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type removesmall(removesmallSEXP);
    Rcpp::traits::input_parameter< double >::type sigstart(sigstartSEXP);
    Rcpp::traits::input_parameter< double >::type sigend(sigendSEXP);
    rcpp_result_gen = Rcpp::wrap(BIMCpp(oneboostImmigrate, train_xx, train_yy, nIter, max_iter, removesmall, sigstart, sigend));
    return rcpp_result_gen;
END_RCPP
}
// BIMMACpp
List BIMMACpp(Function Immigrate, NumericMatrix train_xx, NumericVector train_yy, int nIter, int max_iter, bool removesmall, bool randomw0, double sigstart, double sigend);
RcppExport SEXP _Immigrate_BIMMACpp(SEXP ImmigrateSEXP, SEXP train_xxSEXP, SEXP train_yySEXP, SEXP nIterSEXP, SEXP max_iterSEXP, SEXP removesmallSEXP, SEXP randomw0SEXP, SEXP sigstartSEXP, SEXP sigendSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Function >::type Immigrate(ImmigrateSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type train_xx(train_xxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type train_yy(train_yySEXP);
    Rcpp::traits::input_parameter< int >::type nIter(nIterSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type removesmall(removesmallSEXP);
    Rcpp::traits::input_parameter< bool >::type randomw0(randomw0SEXP);
    Rcpp::traits::input_parameter< double >::type sigstart(sigstartSEXP);
    Rcpp::traits::input_parameter< double >::type sigend(sigendSEXP);
    rcpp_result_gen = Rcpp::wrap(BIMMACpp(Immigrate, train_xx, train_yy, nIter, max_iter, removesmall, randomw0, sigstart, sigend));
    return rcpp_result_gen;
END_RCPP
}
// IM4ECpp
List IM4ECpp(Function oneIM4E, NumericMatrix train_xx, NumericVector train_yy, double epsilon, double sig, double lambda, int max_iter, bool removesmall);
RcppExport SEXP _Immigrate_IM4ECpp(SEXP oneIM4ESEXP, SEXP train_xxSEXP, SEXP train_yySEXP, SEXP epsilonSEXP, SEXP sigSEXP, SEXP lambdaSEXP, SEXP max_iterSEXP, SEXP removesmallSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Function >::type oneIM4E(oneIM4ESEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type train_xx(train_xxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type train_yy(train_yySEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< double >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type removesmall(removesmallSEXP);
    rcpp_result_gen = Rcpp::wrap(IM4ECpp(oneIM4E, train_xx, train_yy, epsilon, sig, lambda, max_iter, removesmall));
    return rcpp_result_gen;
END_RCPP
}
// ImmigrateCpp
List ImmigrateCpp(Function oneImmigrate, NumericMatrix train_xx, NumericVector train_yy, NumericMatrix w0, double epsilon, double sig, int max_iter, bool removesmall);
RcppExport SEXP _Immigrate_ImmigrateCpp(SEXP oneImmigrateSEXP, SEXP train_xxSEXP, SEXP train_yySEXP, SEXP w0SEXP, SEXP epsilonSEXP, SEXP sigSEXP, SEXP max_iterSEXP, SEXP removesmallSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Function >::type oneImmigrate(oneImmigrateSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type train_xx(train_xxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type train_yy(train_yySEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type w0(w0SEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< double >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type removesmall(removesmallSEXP);
    rcpp_result_gen = Rcpp::wrap(ImmigrateCpp(oneImmigrate, train_xx, train_yy, w0, epsilon, sig, max_iter, removesmall));
    return rcpp_result_gen;
END_RCPP
}
// ImmigrateSampleCpp
List ImmigrateSampleCpp(Function onesampleImmigrate, NumericMatrix train_xx, NumericVector train_yy, NumericVector sample_wt, NumericMatrix W, double epsilon, double sig, int max_iter, bool removesmall);
RcppExport SEXP _Immigrate_ImmigrateSampleCpp(SEXP onesampleImmigrateSEXP, SEXP train_xxSEXP, SEXP train_yySEXP, SEXP sample_wtSEXP, SEXP WSEXP, SEXP epsilonSEXP, SEXP sigSEXP, SEXP max_iterSEXP, SEXP removesmallSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Function >::type onesampleImmigrate(onesampleImmigrateSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type train_xx(train_xxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type train_yy(train_yySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sample_wt(sample_wtSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type W(WSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< double >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type removesmall(removesmallSEXP);
    rcpp_result_gen = Rcpp::wrap(ImmigrateSampleCpp(onesampleImmigrate, train_xx, train_yy, sample_wt, W, epsilon, sig, max_iter, removesmall));
    return rcpp_result_gen;
END_RCPP
}
// imIM4ECpp
List imIM4ECpp(Function oneimIM4E, NumericMatrix train_xx, NumericVector train_yy, double epsilon, double sig, double rho, double lambda, int max_iter, bool removesmall);
RcppExport SEXP _Immigrate_imIM4ECpp(SEXP oneimIM4ESEXP, SEXP train_xxSEXP, SEXP train_yySEXP, SEXP epsilonSEXP, SEXP sigSEXP, SEXP rhoSEXP, SEXP lambdaSEXP, SEXP max_iterSEXP, SEXP removesmallSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Function >::type oneimIM4E(oneimIM4ESEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type train_xx(train_xxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type train_yy(train_yySEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< double >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type removesmall(removesmallSEXP);
    rcpp_result_gen = Rcpp::wrap(imIM4ECpp(oneimIM4E, train_xx, train_yy, epsilon, sig, rho, lambda, max_iter, removesmall));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Immigrate_BIMCpp", (DL_FUNC) &_Immigrate_BIMCpp, 8},
    {"_Immigrate_BIMMACpp", (DL_FUNC) &_Immigrate_BIMMACpp, 9},
    {"_Immigrate_IM4ECpp", (DL_FUNC) &_Immigrate_IM4ECpp, 8},
    {"_Immigrate_ImmigrateCpp", (DL_FUNC) &_Immigrate_ImmigrateCpp, 8},
    {"_Immigrate_ImmigrateSampleCpp", (DL_FUNC) &_Immigrate_ImmigrateSampleCpp, 9},
    {"_Immigrate_imIM4ECpp", (DL_FUNC) &_Immigrate_imIM4ECpp, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_Immigrate(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
