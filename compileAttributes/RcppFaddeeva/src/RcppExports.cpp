// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Faddeeva_w
std::vector< std::complex<double> > Faddeeva_w(const std::vector< std::complex<double> >& z, double relerr);
RcppExport SEXP _RcppFaddeeva_Faddeeva_w(SEXP zSEXP, SEXP relerrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector< std::complex<double> >& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type relerr(relerrSEXP);
    rcpp_result_gen = Rcpp::wrap(Faddeeva_w(z, relerr));
    return rcpp_result_gen;
END_RCPP
}
// erfcx
std::vector< std::complex<double> > erfcx(const std::vector< std::complex<double> >& z, double relerr);
RcppExport SEXP _RcppFaddeeva_erfcx(SEXP zSEXP, SEXP relerrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector< std::complex<double> >& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type relerr(relerrSEXP);
    rcpp_result_gen = Rcpp::wrap(erfcx(z, relerr));
    return rcpp_result_gen;
END_RCPP
}
// erf
std::vector< std::complex<double> > erf(const std::vector< std::complex<double> >& z, double relerr);
RcppExport SEXP _RcppFaddeeva_erf(SEXP zSEXP, SEXP relerrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector< std::complex<double> >& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type relerr(relerrSEXP);
    rcpp_result_gen = Rcpp::wrap(erf(z, relerr));
    return rcpp_result_gen;
END_RCPP
}
// erfi
std::vector< std::complex<double> > erfi(const std::vector< std::complex<double> >& z, double relerr);
RcppExport SEXP _RcppFaddeeva_erfi(SEXP zSEXP, SEXP relerrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector< std::complex<double> >& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type relerr(relerrSEXP);
    rcpp_result_gen = Rcpp::wrap(erfi(z, relerr));
    return rcpp_result_gen;
END_RCPP
}
// erfc
std::vector< std::complex<double> > erfc(const std::vector< std::complex<double> >& z, double relerr);
RcppExport SEXP _RcppFaddeeva_erfc(SEXP zSEXP, SEXP relerrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector< std::complex<double> >& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type relerr(relerrSEXP);
    rcpp_result_gen = Rcpp::wrap(erfc(z, relerr));
    return rcpp_result_gen;
END_RCPP
}
// Dawson
std::vector< std::complex<double> > Dawson(const std::vector< std::complex<double> >& z, double relerr);
RcppExport SEXP _RcppFaddeeva_Dawson(SEXP zSEXP, SEXP relerrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector< std::complex<double> >& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type relerr(relerrSEXP);
    rcpp_result_gen = Rcpp::wrap(Dawson(z, relerr));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RcppFaddeeva_Faddeeva_w", (DL_FUNC) &_RcppFaddeeva_Faddeeva_w, 2},
    {"_RcppFaddeeva_erfcx", (DL_FUNC) &_RcppFaddeeva_erfcx, 2},
    {"_RcppFaddeeva_erf", (DL_FUNC) &_RcppFaddeeva_erf, 2},
    {"_RcppFaddeeva_erfi", (DL_FUNC) &_RcppFaddeeva_erfi, 2},
    {"_RcppFaddeeva_erfc", (DL_FUNC) &_RcppFaddeeva_erfc, 2},
    {"_RcppFaddeeva_Dawson", (DL_FUNC) &_RcppFaddeeva_Dawson, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_RcppFaddeeva(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
