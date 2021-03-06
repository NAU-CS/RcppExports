// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// Center
arma::mat Center(arma::mat Y);
RcppExport SEXP iBATCGH_Center(SEXP YSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP );
        arma::mat __result = Center(Y);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// iBAT
Rcpp::List iBAT(arma::mat Y, arma::mat X, arma::colvec distance, double disfix, int intercept, arma::imat xi, arma::icolvec R, arma::mat tran, arma::colvec mu, arma::colvec sigma, double cmu, double c, double delta, double d, double e, double f, double alpha, arma::colvec deltak, arma::colvec tauk, arma::colvec upp_bounds, arma::colvec low_bounds, arma::colvec alpha_IG, arma::colvec beta_IG, arma::colvec low_IG, arma::rowvec a, int niter, int burnin, int Cout, double phi, float pR, int selectioncgh, float pXI, int indep);
RcppExport SEXP iBATCGH_iBAT(SEXP YSEXP, SEXP XSEXP, SEXP distanceSEXP, SEXP disfixSEXP, SEXP interceptSEXP, SEXP xiSEXP, SEXP RSEXP, SEXP tranSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP cmuSEXP, SEXP cSEXP, SEXP deltaSEXP, SEXP dSEXP, SEXP eSEXP, SEXP fSEXP, SEXP alphaSEXP, SEXP deltakSEXP, SEXP taukSEXP, SEXP upp_boundsSEXP, SEXP low_boundsSEXP, SEXP alpha_IGSEXP, SEXP beta_IGSEXP, SEXP low_IGSEXP, SEXP aSEXP, SEXP niterSEXP, SEXP burninSEXP, SEXP CoutSEXP, SEXP phiSEXP, SEXP pRSEXP, SEXP selectioncghSEXP, SEXP pXISEXP, SEXP indepSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP );
        Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type distance(distanceSEXP );
        Rcpp::traits::input_parameter< double >::type disfix(disfixSEXP );
        Rcpp::traits::input_parameter< int >::type intercept(interceptSEXP );
        Rcpp::traits::input_parameter< arma::imat >::type xi(xiSEXP );
        Rcpp::traits::input_parameter< arma::icolvec >::type R(RSEXP );
        Rcpp::traits::input_parameter< arma::mat >::type tran(tranSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type mu(muSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type sigma(sigmaSEXP );
        Rcpp::traits::input_parameter< double >::type cmu(cmuSEXP );
        Rcpp::traits::input_parameter< double >::type c(cSEXP );
        Rcpp::traits::input_parameter< double >::type delta(deltaSEXP );
        Rcpp::traits::input_parameter< double >::type d(dSEXP );
        Rcpp::traits::input_parameter< double >::type e(eSEXP );
        Rcpp::traits::input_parameter< double >::type f(fSEXP );
        Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type deltak(deltakSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type tauk(taukSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type upp_bounds(upp_boundsSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type low_bounds(low_boundsSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type alpha_IG(alpha_IGSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type beta_IG(beta_IGSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type low_IG(low_IGSEXP );
        Rcpp::traits::input_parameter< arma::rowvec >::type a(aSEXP );
        Rcpp::traits::input_parameter< int >::type niter(niterSEXP );
        Rcpp::traits::input_parameter< int >::type burnin(burninSEXP );
        Rcpp::traits::input_parameter< int >::type Cout(CoutSEXP );
        Rcpp::traits::input_parameter< double >::type phi(phiSEXP );
        Rcpp::traits::input_parameter< float >::type pR(pRSEXP );
        Rcpp::traits::input_parameter< int >::type selectioncgh(selectioncghSEXP );
        Rcpp::traits::input_parameter< float >::type pXI(pXISEXP );
        Rcpp::traits::input_parameter< int >::type indep(indepSEXP );
        Rcpp::List __result = iBAT(Y, X, distance, disfix, intercept, xi, R, tran, mu, sigma, cmu, c, delta, d, e, f, alpha, deltak, tauk, upp_bounds, low_bounds, alpha_IG, beta_IG, low_IG, a, niter, burnin, Cout, phi, pR, selectioncgh, pXI, indep);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// iBATProbit
Rcpp::List iBATProbit(arma::mat Y, arma::mat X, arma::colvec distance, double disfix, int intercept, arma::imat xi, arma::icolvec R, arma::mat tran, arma::colvec mu, arma::colvec sigma, double cmu, double c, double delta, double d, double alpha0, double alpha1, arma::colvec deltak, arma::colvec tauk, arma::colvec upp_bounds, arma::colvec low_bounds, arma::colvec alpha_IG, arma::colvec beta_IG, arma::colvec low_IG, arma::rowvec a, int niter, int burnin, int Cout, double phi, float pR, int selectioncgh, float pXI);
RcppExport SEXP iBATCGH_iBATProbit(SEXP YSEXP, SEXP XSEXP, SEXP distanceSEXP, SEXP disfixSEXP, SEXP interceptSEXP, SEXP xiSEXP, SEXP RSEXP, SEXP tranSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP cmuSEXP, SEXP cSEXP, SEXP deltaSEXP, SEXP dSEXP, SEXP alpha0SEXP, SEXP alpha1SEXP, SEXP deltakSEXP, SEXP taukSEXP, SEXP upp_boundsSEXP, SEXP low_boundsSEXP, SEXP alpha_IGSEXP, SEXP beta_IGSEXP, SEXP low_IGSEXP, SEXP aSEXP, SEXP niterSEXP, SEXP burninSEXP, SEXP CoutSEXP, SEXP phiSEXP, SEXP pRSEXP, SEXP selectioncghSEXP, SEXP pXISEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP );
        Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type distance(distanceSEXP );
        Rcpp::traits::input_parameter< double >::type disfix(disfixSEXP );
        Rcpp::traits::input_parameter< int >::type intercept(interceptSEXP );
        Rcpp::traits::input_parameter< arma::imat >::type xi(xiSEXP );
        Rcpp::traits::input_parameter< arma::icolvec >::type R(RSEXP );
        Rcpp::traits::input_parameter< arma::mat >::type tran(tranSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type mu(muSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type sigma(sigmaSEXP );
        Rcpp::traits::input_parameter< double >::type cmu(cmuSEXP );
        Rcpp::traits::input_parameter< double >::type c(cSEXP );
        Rcpp::traits::input_parameter< double >::type delta(deltaSEXP );
        Rcpp::traits::input_parameter< double >::type d(dSEXP );
        Rcpp::traits::input_parameter< double >::type alpha0(alpha0SEXP );
        Rcpp::traits::input_parameter< double >::type alpha1(alpha1SEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type deltak(deltakSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type tauk(taukSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type upp_bounds(upp_boundsSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type low_bounds(low_boundsSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type alpha_IG(alpha_IGSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type beta_IG(beta_IGSEXP );
        Rcpp::traits::input_parameter< arma::colvec >::type low_IG(low_IGSEXP );
        Rcpp::traits::input_parameter< arma::rowvec >::type a(aSEXP );
        Rcpp::traits::input_parameter< int >::type niter(niterSEXP );
        Rcpp::traits::input_parameter< int >::type burnin(burninSEXP );
        Rcpp::traits::input_parameter< int >::type Cout(CoutSEXP );
        Rcpp::traits::input_parameter< double >::type phi(phiSEXP );
        Rcpp::traits::input_parameter< float >::type pR(pRSEXP );
        Rcpp::traits::input_parameter< int >::type selectioncgh(selectioncghSEXP );
        Rcpp::traits::input_parameter< float >::type pXI(pXISEXP );
        Rcpp::List __result = iBATProbit(Y, X, distance, disfix, intercept, xi, R, tran, mu, sigma, cmu, c, delta, d, alpha0, alpha1, deltak, tauk, upp_bounds, low_bounds, alpha_IG, beta_IG, low_IG, a, niter, burnin, Cout, phi, pR, selectioncgh, pXI);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// RListToVector
arma::vec RListToVector(Rcpp::List xList, int G, int T);
RcppExport SEXP iBATCGH_RListToVector(SEXP xListSEXP, SEXP GSEXP, SEXP TSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::List >::type xList(xListSEXP );
        Rcpp::traits::input_parameter< int >::type G(GSEXP );
        Rcpp::traits::input_parameter< int >::type T(TSEXP );
        arma::vec __result = RListToVector(xList, G, T);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// Tran
arma::mat Tran(arma::mat xi);
RcppExport SEXP iBATCGH_Tran(SEXP xiSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< arma::mat >::type xi(xiSEXP );
        arma::mat __result = Tran(xi);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int iBATCGH_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP iBATCGH_RcppExport_registerCCallable() { 
    R_RegisterCCallable("iBATCGH", "iBATCGH_RcppExport_validate", (DL_FUNC)iBATCGH_RcppExport_validate);
    return R_NilValue;
}
