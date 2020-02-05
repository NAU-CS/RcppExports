// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// badData
arma::mat badData(Rcpp::NumericMatrix X, NumericVector meds, NumericVector mads, double nMads, double t);
RcppExport SEXP _FIACH_badData(SEXP XSEXP, SEXP medsSEXP, SEXP madsSEXP, SEXP nMadsSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type meds(medsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mads(madsSEXP);
    Rcpp::traits::input_parameter< double >::type nMads(nMadsSEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(badData(X, meds, mads, nMads, t));
    return rcpp_result_gen;
END_RCPP
}
// colMedian
Rcpp::NumericVector colMedian(Rcpp::NumericMatrix X);
RcppExport SEXP _FIACH_colMedian(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(colMedian(X));
    return rcpp_result_gen;
END_RCPP
}
// colMad
Rcpp::NumericVector colMad(Rcpp::NumericMatrix X);
RcppExport SEXP _FIACH_colMad(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(colMad(X));
    return rcpp_result_gen;
END_RCPP
}
// colsd
Rcpp::NumericVector colsd(Rcpp::NumericMatrix X);
RcppExport SEXP _FIACH_colsd(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(colsd(X));
    return rcpp_result_gen;
END_RCPP
}
// convolve1d
arma::mat convolve1d(arma::mat x, arma::colvec fir, int Nfft, bool subtractMed);
RcppExport SEXP _FIACH_convolve1d(SEXP xSEXP, SEXP firSEXP, SEXP NfftSEXP, SEXP subtractMedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type fir(firSEXP);
    Rcpp::traits::input_parameter< int >::type Nfft(NfftSEXP);
    Rcpp::traits::input_parameter< bool >::type subtractMed(subtractMedSEXP);
    rcpp_result_gen = Rcpp::wrap(convolve1d(x, fir, Nfft, subtractMed));
    return rcpp_result_gen;
END_RCPP
}
// hampel
arma::mat hampel(arma::mat x, int k, double t0);
RcppExport SEXP _FIACH_hampel(SEXP xSEXP, SEXP kSEXP, SEXP t0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type t0(t0SEXP);
    rcpp_result_gen = Rcpp::wrap(hampel(x, k, t0));
    return rcpp_result_gen;
END_RCPP
}
// gmm
Rcpp::List gmm(Rcpp::NumericVector x, int k, Rcpp::NumericVector imeans, Rcpp::NumericVector isd, Rcpp::NumericVector ilambda, bool print, double tol, int maxit);
RcppExport SEXP _FIACH_gmm(SEXP xSEXP, SEXP kSEXP, SEXP imeansSEXP, SEXP isdSEXP, SEXP ilambdaSEXP, SEXP printSEXP, SEXP tolSEXP, SEXP maxitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type imeans(imeansSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type isd(isdSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ilambda(ilambdaSEXP);
    Rcpp::traits::input_parameter< bool >::type print(printSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    rcpp_result_gen = Rcpp::wrap(gmm(x, k, imeans, isd, ilambda, print, tol, maxit));
    return rcpp_result_gen;
END_RCPP
}
// fftN
arma::cx_mat fftN(arma::mat X, int N);
RcppExport SEXP _FIACH_fftN(SEXP XSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(fftN(X, N));
    return rcpp_result_gen;
END_RCPP
}
// pseudo
arma::mat pseudo(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y, bool residuals, bool keepMean, bool includeIntercept);
RcppExport SEXP _FIACH_pseudo(SEXP xSEXP, SEXP ySEXP, SEXP residualsSEXP, SEXP keepMeanSEXP, SEXP includeInterceptSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< bool >::type residuals(residualsSEXP);
    Rcpp::traits::input_parameter< bool >::type keepMean(keepMeanSEXP);
    Rcpp::traits::input_parameter< bool >::type includeIntercept(includeInterceptSEXP);
    rcpp_result_gen = Rcpp::wrap(pseudo(x, y, residuals, keepMean, includeIntercept));
    return rcpp_result_gen;
END_RCPP
}
// rowMad
Rcpp::NumericVector rowMad(Rcpp::NumericMatrix X);
RcppExport SEXP _FIACH_rowMad(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMad(X));
    return rcpp_result_gen;
END_RCPP
}
// rowMedian
Rcpp::NumericVector rowMedian(Rcpp::NumericMatrix X);
RcppExport SEXP _FIACH_rowMedian(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMedian(X));
    return rcpp_result_gen;
END_RCPP
}
// rowsd
Rcpp::NumericVector rowsd(Rcpp::NumericMatrix X);
RcppExport SEXP _FIACH_rowsd(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(rowsd(X));
    return rcpp_result_gen;
END_RCPP
}
// sepConvolve3d
arma::cube sepConvolve3d(NumericVector x, arma::colvec kernX, arma::colvec kernY, arma::colvec kernZ, int Nx, int Ny, int Nz);
RcppExport SEXP _FIACH_sepConvolve3d(SEXP xSEXP, SEXP kernXSEXP, SEXP kernYSEXP, SEXP kernZSEXP, SEXP NxSEXP, SEXP NySEXP, SEXP NzSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type kernX(kernXSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type kernY(kernYSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type kernZ(kernZSEXP);
    Rcpp::traits::input_parameter< int >::type Nx(NxSEXP);
    Rcpp::traits::input_parameter< int >::type Ny(NySEXP);
    Rcpp::traits::input_parameter< int >::type Nz(NzSEXP);
    rcpp_result_gen = Rcpp::wrap(sepConvolve3d(x, kernX, kernY, kernZ, Nx, Ny, Nz));
    return rcpp_result_gen;
END_RCPP
}
// zero_na
NumericVector zero_na(NumericVector input);
RcppExport SEXP _FIACH_zero_na(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(zero_na(input));
    return rcpp_result_gen;
END_RCPP
}
// dilate
Rcpp::NumericVector dilate(Rcpp::NumericVector input, int k);
RcppExport SEXP _FIACH_dilate(SEXP inputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type input(inputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(dilate(input, k));
    return rcpp_result_gen;
END_RCPP
}
// erode
Rcpp::NumericVector erode(Rcpp::NumericVector input, int k);
RcppExport SEXP _FIACH_erode(SEXP inputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type input(inputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(erode(input, k));
    return rcpp_result_gen;
END_RCPP
}
// dcombine
Rcpp::NumericVector dcombine(Rcpp::NumericVector X, Rcpp::IntegerVector dim);
RcppExport SEXP _FIACH_dcombine(SEXP XSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type X(XSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(dcombine(X, dim));
    return rcpp_result_gen;
END_RCPP
}
// icombine
IntegerVector icombine(Rcpp::IntegerVector X, Rcpp::IntegerVector dim);
RcppExport SEXP _FIACH_icombine(SEXP XSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type X(XSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(icombine(X, dim));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FIACH_badData", (DL_FUNC) &_FIACH_badData, 5},
    {"_FIACH_colMedian", (DL_FUNC) &_FIACH_colMedian, 1},
    {"_FIACH_colMad", (DL_FUNC) &_FIACH_colMad, 1},
    {"_FIACH_colsd", (DL_FUNC) &_FIACH_colsd, 1},
    {"_FIACH_convolve1d", (DL_FUNC) &_FIACH_convolve1d, 4},
    {"_FIACH_hampel", (DL_FUNC) &_FIACH_hampel, 3},
    {"_FIACH_gmm", (DL_FUNC) &_FIACH_gmm, 8},
    {"_FIACH_fftN", (DL_FUNC) &_FIACH_fftN, 2},
    {"_FIACH_pseudo", (DL_FUNC) &_FIACH_pseudo, 5},
    {"_FIACH_rowMad", (DL_FUNC) &_FIACH_rowMad, 1},
    {"_FIACH_rowMedian", (DL_FUNC) &_FIACH_rowMedian, 1},
    {"_FIACH_rowsd", (DL_FUNC) &_FIACH_rowsd, 1},
    {"_FIACH_sepConvolve3d", (DL_FUNC) &_FIACH_sepConvolve3d, 7},
    {"_FIACH_zero_na", (DL_FUNC) &_FIACH_zero_na, 1},
    {"_FIACH_dilate", (DL_FUNC) &_FIACH_dilate, 2},
    {"_FIACH_erode", (DL_FUNC) &_FIACH_erode, 2},
    {"_FIACH_dcombine", (DL_FUNC) &_FIACH_dcombine, 2},
    {"_FIACH_icombine", (DL_FUNC) &_FIACH_icombine, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_FIACH(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
