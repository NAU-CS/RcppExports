// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// CalcLikelihoodC
double CalcLikelihoodC(arma::sp_mat dtm, NumericMatrix phi, NumericMatrix theta);
RcppExport SEXP _textmineR_CalcLikelihoodC(SEXP dtmSEXP, SEXP phiSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type dtm(dtmSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcLikelihoodC(dtm, phi, theta));
    return rcpp_result_gen;
END_RCPP
}
// CalcSumSquares
NumericVector CalcSumSquares(arma::sp_mat dtm, NumericMatrix phi, NumericMatrix theta, NumericVector ybar);
RcppExport SEXP _textmineR_CalcSumSquares(SEXP dtmSEXP, SEXP phiSEXP, SEXP thetaSEXP, SEXP ybarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type dtm(dtmSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ybar(ybarSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcSumSquares(dtm, phi, theta, ybar));
    return rcpp_result_gen;
END_RCPP
}
// Dtm2DocsC
List Dtm2DocsC(arma::sp_mat dtm, std::vector< std::string> vocab);
RcppExport SEXP _textmineR_Dtm2DocsC(SEXP dtmSEXP, SEXP vocabSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type dtm(dtmSEXP);
    Rcpp::traits::input_parameter< std::vector< std::string> >::type vocab(vocabSEXP);
    rcpp_result_gen = Rcpp::wrap(Dtm2DocsC(dtm, vocab));
    return rcpp_result_gen;
END_RCPP
}
// HellingerMat
NumericMatrix HellingerMat(NumericMatrix A);
RcppExport SEXP _textmineR_HellingerMat(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(HellingerMat(A));
    return rcpp_result_gen;
END_RCPP
}
// Hellinger_cpp
double Hellinger_cpp(NumericVector p, NumericVector q);
RcppExport SEXP _textmineR_Hellinger_cpp(SEXP pSEXP, SEXP qSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type q(qSEXP);
    rcpp_result_gen = Rcpp::wrap(Hellinger_cpp(p, q));
    return rcpp_result_gen;
END_RCPP
}
// JSD_cpp
double JSD_cpp(NumericVector p, NumericVector q);
RcppExport SEXP _textmineR_JSD_cpp(SEXP pSEXP, SEXP qSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type q(qSEXP);
    rcpp_result_gen = Rcpp::wrap(JSD_cpp(p, q));
    return rcpp_result_gen;
END_RCPP
}
// JSDmat
NumericMatrix JSDmat(NumericMatrix A);
RcppExport SEXP _textmineR_JSDmat(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(JSDmat(A));
    return rcpp_result_gen;
END_RCPP
}
// dtm_to_lexicon_c
List dtm_to_lexicon_c(arma::sp_mat x);
RcppExport SEXP _textmineR_dtm_to_lexicon_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(dtm_to_lexicon_c(x));
    return rcpp_result_gen;
END_RCPP
}
// fit_lda_c
List fit_lda_c(List& docs, int& Nk, int& Nd, int& Nv, NumericVector alph, NumericMatrix& beta, int& iterations, int& burnin, bool& optimize_alpha, bool& calc_likelihood);
RcppExport SEXP _textmineR_fit_lda_c(SEXP docsSEXP, SEXP NkSEXP, SEXP NdSEXP, SEXP NvSEXP, SEXP alphSEXP, SEXP betaSEXP, SEXP iterationsSEXP, SEXP burninSEXP, SEXP optimize_alphaSEXP, SEXP calc_likelihoodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List& >::type docs(docsSEXP);
    Rcpp::traits::input_parameter< int& >::type Nk(NkSEXP);
    Rcpp::traits::input_parameter< int& >::type Nd(NdSEXP);
    Rcpp::traits::input_parameter< int& >::type Nv(NvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alph(alphSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< int& >::type iterations(iterationsSEXP);
    Rcpp::traits::input_parameter< int& >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< bool& >::type optimize_alpha(optimize_alphaSEXP);
    Rcpp::traits::input_parameter< bool& >::type calc_likelihood(calc_likelihoodSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_lda_c(docs, Nk, Nd, Nv, alph, beta, iterations, burnin, optimize_alpha, calc_likelihood));
    return rcpp_result_gen;
END_RCPP
}
// predict_lda_c
List predict_lda_c(List& docs, int& Nk, int& Nd, NumericVector& alpha, NumericMatrix& phi, int& iterations, int& burnin);
RcppExport SEXP _textmineR_predict_lda_c(SEXP docsSEXP, SEXP NkSEXP, SEXP NdSEXP, SEXP alphaSEXP, SEXP phiSEXP, SEXP iterationsSEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List& >::type docs(docsSEXP);
    Rcpp::traits::input_parameter< int& >::type Nk(NkSEXP);
    Rcpp::traits::input_parameter< int& >::type Nd(NdSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< int& >::type iterations(iterationsSEXP);
    Rcpp::traits::input_parameter< int& >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_lda_c(docs, Nk, Nd, alpha, phi, iterations, burnin));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_textmineR_CalcLikelihoodC", (DL_FUNC) &_textmineR_CalcLikelihoodC, 3},
    {"_textmineR_CalcSumSquares", (DL_FUNC) &_textmineR_CalcSumSquares, 4},
    {"_textmineR_Dtm2DocsC", (DL_FUNC) &_textmineR_Dtm2DocsC, 2},
    {"_textmineR_HellingerMat", (DL_FUNC) &_textmineR_HellingerMat, 1},
    {"_textmineR_Hellinger_cpp", (DL_FUNC) &_textmineR_Hellinger_cpp, 2},
    {"_textmineR_JSD_cpp", (DL_FUNC) &_textmineR_JSD_cpp, 2},
    {"_textmineR_JSDmat", (DL_FUNC) &_textmineR_JSDmat, 1},
    {"_textmineR_dtm_to_lexicon_c", (DL_FUNC) &_textmineR_dtm_to_lexicon_c, 1},
    {"_textmineR_fit_lda_c", (DL_FUNC) &_textmineR_fit_lda_c, 10},
    {"_textmineR_predict_lda_c", (DL_FUNC) &_textmineR_predict_lda_c, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_textmineR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
