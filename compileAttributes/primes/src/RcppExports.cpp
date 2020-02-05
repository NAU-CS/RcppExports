// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// is_prime_vector
std::vector < bool > is_prime_vector(std::vector < int > x);
RcppExport SEXP _primes_is_prime_vector(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector < int > >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_prime_vector(x));
    return rcpp_result_gen;
END_RCPP
}
// generate_primes_
std::vector < int > generate_primes_(int min, int max);
RcppExport SEXP _primes_generate_primes_(SEXP minSEXP, SEXP maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type min(minSEXP);
    Rcpp::traits::input_parameter< int >::type max(maxSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_primes_(min, max));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_primes_is_prime_vector", (DL_FUNC) &_primes_is_prime_vector, 1},
    {"_primes_generate_primes_", (DL_FUNC) &_primes_generate_primes_, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_primes(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
