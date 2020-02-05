// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cxxfit
List cxxfit(NumericVector tht, NumericMatrix revents, NumericMatrix rpoly, NumericVector tperiod, double rinteg0, NumericMatrix ihess, int ndiv, double eps, bool verbose, int nthreads);
RcppExport SEXP _ETAS_cxxfit(SEXP thtSEXP, SEXP reventsSEXP, SEXP rpolySEXP, SEXP tperiodSEXP, SEXP rinteg0SEXP, SEXP ihessSEXP, SEXP ndivSEXP, SEXP epsSEXP, SEXP verboseSEXP, SEXP nthreadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tht(thtSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type revents(reventsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rpoly(rpolySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tperiod(tperiodSEXP);
    Rcpp::traits::input_parameter< double >::type rinteg0(rinteg0SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ihess(ihessSEXP);
    Rcpp::traits::input_parameter< int >::type ndiv(ndivSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< int >::type nthreads(nthreadsSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxfit(tht, revents, rpoly, tperiod, rinteg0, ihess, ndiv, eps, verbose, nthreads));
    return rcpp_result_gen;
END_RCPP
}
// cxxdeclust
List cxxdeclust(NumericVector param, NumericMatrix revents, NumericMatrix rpoly, NumericVector bwd, NumericVector tperiod, int ndiv);
RcppExport SEXP _ETAS_cxxdeclust(SEXP paramSEXP, SEXP reventsSEXP, SEXP rpolySEXP, SEXP bwdSEXP, SEXP tperiodSEXP, SEXP ndivSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type param(paramSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type revents(reventsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rpoly(rpolySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bwd(bwdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tperiod(tperiodSEXP);
    Rcpp::traits::input_parameter< int >::type ndiv(ndivSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxdeclust(param, revents, rpoly, bwd, tperiod, ndiv));
    return rcpp_result_gen;
END_RCPP
}
// cxxrates
List cxxrates(NumericVector param, NumericMatrix revents, NumericVector bwd, NumericVector tperiod, NumericVector gx, NumericVector gy);
RcppExport SEXP _ETAS_cxxrates(SEXP paramSEXP, SEXP reventsSEXP, SEXP bwdSEXP, SEXP tperiodSEXP, SEXP gxSEXP, SEXP gySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type param(paramSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type revents(reventsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bwd(bwdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tperiod(tperiodSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gx(gxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gy(gySEXP);
    rcpp_result_gen = Rcpp::wrap(cxxrates(param, revents, bwd, tperiod, gx, gy));
    return rcpp_result_gen;
END_RCPP
}
// cxxtimetrans
NumericVector cxxtimetrans(NumericVector theta, NumericMatrix revents, NumericMatrix rpoly, NumericVector tperiod, double integ0, int ndiv);
RcppExport SEXP _ETAS_cxxtimetrans(SEXP thetaSEXP, SEXP reventsSEXP, SEXP rpolySEXP, SEXP tperiodSEXP, SEXP integ0SEXP, SEXP ndivSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type revents(reventsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rpoly(rpolySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tperiod(tperiodSEXP);
    Rcpp::traits::input_parameter< double >::type integ0(integ0SEXP);
    Rcpp::traits::input_parameter< int >::type ndiv(ndivSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxtimetrans(theta, revents, rpoly, tperiod, integ0, ndiv));
    return rcpp_result_gen;
END_RCPP
}
// cxxlambdtemp
NumericVector cxxlambdtemp(NumericVector tg, NumericVector theta, NumericMatrix revents, NumericMatrix rpoly, NumericVector tperiod, double integ0, int ndiv);
RcppExport SEXP _ETAS_cxxlambdtemp(SEXP tgSEXP, SEXP thetaSEXP, SEXP reventsSEXP, SEXP rpolySEXP, SEXP tperiodSEXP, SEXP integ0SEXP, SEXP ndivSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tg(tgSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type revents(reventsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rpoly(rpolySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tperiod(tperiodSEXP);
    Rcpp::traits::input_parameter< double >::type integ0(integ0SEXP);
    Rcpp::traits::input_parameter< int >::type ndiv(ndivSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxlambdtemp(tg, theta, revents, rpoly, tperiod, integ0, ndiv));
    return rcpp_result_gen;
END_RCPP
}
// cxxlambspat
NumericVector cxxlambspat(NumericVector xg, NumericVector yg, NumericVector theta, NumericMatrix revents, NumericMatrix rpoly, NumericVector tperiod, NumericVector bwd);
RcppExport SEXP _ETAS_cxxlambspat(SEXP xgSEXP, SEXP ygSEXP, SEXP thetaSEXP, SEXP reventsSEXP, SEXP rpolySEXP, SEXP tperiodSEXP, SEXP bwdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xg(xgSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yg(ygSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type revents(reventsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rpoly(rpolySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tperiod(tperiodSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bwd(bwdSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxlambspat(xg, yg, theta, revents, rpoly, tperiod, bwd));
    return rcpp_result_gen;
END_RCPP
}
// cxxSmooth
List cxxSmooth(NumericVector x, NumericVector y, NumericVector bwd, NumericVector gx, NumericVector gy, bool expand);
RcppExport SEXP _ETAS_cxxSmooth(SEXP xSEXP, SEXP ySEXP, SEXP bwdSEXP, SEXP gxSEXP, SEXP gySEXP, SEXP expandSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bwd(bwdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gx(gxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gy(gySEXP);
    Rcpp::traits::input_parameter< bool >::type expand(expandSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxSmooth(x, y, bwd, gx, gy, expand));
    return rcpp_result_gen;
END_RCPP
}
// cxxstpoisstest
double cxxstpoisstest(NumericVector xrank, NumericVector yrank, NumericMatrix M);
RcppExport SEXP _ETAS_cxxstpoisstest(SEXP xrankSEXP, SEXP yrankSEXP, SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xrank(xrankSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yrank(yrankSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxstpoisstest(xrank, yrank, M));
    return rcpp_result_gen;
END_RCPP
}
// cxxstpoisstestMP
double cxxstpoisstestMP(NumericVector xrank, NumericVector yrank, NumericMatrix M, int nthreads);
RcppExport SEXP _ETAS_cxxstpoisstestMP(SEXP xrankSEXP, SEXP yrankSEXP, SEXP MSEXP, SEXP nthreadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xrank(xrankSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yrank(yrankSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type nthreads(nthreadsSEXP);
    rcpp_result_gen = Rcpp::wrap(cxxstpoisstestMP(xrank, yrank, M, nthreads));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP cdeclust(SEXP, SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP cfit(SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP clambdax(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_ETAS_cxxfit", (DL_FUNC) &_ETAS_cxxfit, 10},
    {"_ETAS_cxxdeclust", (DL_FUNC) &_ETAS_cxxdeclust, 6},
    {"_ETAS_cxxrates", (DL_FUNC) &_ETAS_cxxrates, 6},
    {"_ETAS_cxxtimetrans", (DL_FUNC) &_ETAS_cxxtimetrans, 6},
    {"_ETAS_cxxlambdtemp", (DL_FUNC) &_ETAS_cxxlambdtemp, 7},
    {"_ETAS_cxxlambspat", (DL_FUNC) &_ETAS_cxxlambspat, 7},
    {"_ETAS_cxxSmooth", (DL_FUNC) &_ETAS_cxxSmooth, 6},
    {"_ETAS_cxxstpoisstest", (DL_FUNC) &_ETAS_cxxstpoisstest, 3},
    {"_ETAS_cxxstpoisstestMP", (DL_FUNC) &_ETAS_cxxstpoisstestMP, 4},
    {"cdeclust", (DL_FUNC) &cdeclust, 5},
    {"cfit",     (DL_FUNC) &cfit,     4},
    {"clambdax", (DL_FUNC) &clambdax, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_ETAS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
