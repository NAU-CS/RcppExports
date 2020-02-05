// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// BayesBDbinary
Rcpp::List BayesBDbinary(SEXP& obs, SEXP& inimean, SEXP& nrun, SEXP& nburn, SEXP& J, SEXP& ordering, SEXP& slice, SEXP& outputAll);
RcppExport SEXP _BayesBD_BayesBDbinary(SEXP obsSEXP, SEXP inimeanSEXP, SEXP nrunSEXP, SEXP nburnSEXP, SEXP JSEXP, SEXP orderingSEXP, SEXP sliceSEXP, SEXP outputAllSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type inimean(inimeanSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type nrun(nrunSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type nburn(nburnSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type J(JSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ordering(orderingSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type slice(sliceSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type outputAll(outputAllSEXP);
    rcpp_result_gen = Rcpp::wrap(BayesBDbinary(obs, inimean, nrun, nburn, J, ordering, slice, outputAll));
    return rcpp_result_gen;
END_RCPP
}
// unisliceL
Rcpp::List unisliceL(SEXP& ix0, SEXP& igx0, SEXP& ii_J, SEXP& itauini, SEXP& ianini, SEXP& ialpha_a, SEXP& ibeta_a, SEXP& ilambdaini);
RcppExport SEXP _BayesBD_unisliceL(SEXP ix0SEXP, SEXP igx0SEXP, SEXP ii_JSEXP, SEXP itauiniSEXP, SEXP ianiniSEXP, SEXP ialpha_aSEXP, SEXP ibeta_aSEXP, SEXP ilambdainiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type ix0(ix0SEXP);
    Rcpp::traits::input_parameter< SEXP& >::type igx0(igx0SEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ii_J(ii_JSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type itauini(itauiniSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ianini(ianiniSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ialpha_a(ialpha_aSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ibeta_a(ibeta_aSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ilambdaini(ilambdainiSEXP);
    rcpp_result_gen = Rcpp::wrap(unisliceL(ix0, igx0, ii_J, itauini, ianini, ialpha_a, ibeta_a, ilambdaini));
    return rcpp_result_gen;
END_RCPP
}
// eigenfun
double eigenfun(SEXP& in, SEXP& ix);
RcppExport SEXP _BayesBD_eigenfun(SEXP inSEXP, SEXP ixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type in(inSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ix(ixSEXP);
    rcpp_result_gen = Rcpp::wrap(eigenfun(in, ix));
    return rcpp_result_gen;
END_RCPP
}
// BayesBDnormal
Rcpp::List BayesBDnormal(SEXP& obs, SEXP& inimean, SEXP& nrun, SEXP& nburn, SEXP& J, SEXP& ordering_mu, SEXP& ordering_sigma, SEXP& slice, SEXP& outputAll);
RcppExport SEXP _BayesBD_BayesBDnormal(SEXP obsSEXP, SEXP inimeanSEXP, SEXP nrunSEXP, SEXP nburnSEXP, SEXP JSEXP, SEXP ordering_muSEXP, SEXP ordering_sigmaSEXP, SEXP sliceSEXP, SEXP outputAllSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type inimean(inimeanSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type nrun(nrunSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type nburn(nburnSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type J(JSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ordering_mu(ordering_muSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type ordering_sigma(ordering_sigmaSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type slice(sliceSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type outputAll(outputAllSEXP);
    rcpp_result_gen = Rcpp::wrap(BayesBDnormal(obs, inimean, nrun, nburn, J, ordering_mu, ordering_sigma, slice, outputAll));
    return rcpp_result_gen;
END_RCPP
}