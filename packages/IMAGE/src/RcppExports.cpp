// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// AI
SEXP AI(SEXP Yin, SEXP Xin, SEXP numKin, SEXP Phiin, SEXP Din, SEXP tauin, SEXP fixtauin, SEXP tolin);
RcppExport SEXP _IMAGE_AI(SEXP YinSEXP, SEXP XinSEXP, SEXP numKinSEXP, SEXP PhiinSEXP, SEXP DinSEXP, SEXP tauinSEXP, SEXP fixtauinSEXP, SEXP tolinSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type Yin(YinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Xin(XinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type numKin(numKinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Phiin(PhiinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Din(DinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tauin(tauinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type fixtauin(fixtauinSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tolin(tolinSEXP);
    rcpp_result_gen = Rcpp::wrap(AI(Yin, Xin, numKin, Phiin, Din, tauin, fixtauin, tolin));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_hello_world
arma::mat rcpparma_hello_world();
RcppExport SEXP _IMAGE_rcpparma_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpparma_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_outerproduct
arma::mat rcpparma_outerproduct(const arma::colvec& x);
RcppExport SEXP _IMAGE_rcpparma_outerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_outerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_innerproduct
double rcpparma_innerproduct(const arma::colvec& x);
RcppExport SEXP _IMAGE_rcpparma_innerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_innerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_bothproducts
Rcpp::List rcpparma_bothproducts(const arma::colvec& x);
RcppExport SEXP _IMAGE_rcpparma_bothproducts(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_bothproducts(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_IMAGE_AI", (DL_FUNC) &_IMAGE_AI, 8},
    {"_IMAGE_rcpparma_hello_world", (DL_FUNC) &_IMAGE_rcpparma_hello_world, 0},
    {"_IMAGE_rcpparma_outerproduct", (DL_FUNC) &_IMAGE_rcpparma_outerproduct, 1},
    {"_IMAGE_rcpparma_innerproduct", (DL_FUNC) &_IMAGE_rcpparma_innerproduct, 1},
    {"_IMAGE_rcpparma_bothproducts", (DL_FUNC) &_IMAGE_rcpparma_bothproducts, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_IMAGE(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}