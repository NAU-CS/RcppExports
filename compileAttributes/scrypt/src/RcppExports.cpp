// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// hashPassword
CharacterVector hashPassword(const std::string& passwd, double maxmem, double maxtime);
RcppExport SEXP _scrypt_hashPassword(SEXP passwdSEXP, SEXP maxmemSEXP, SEXP maxtimeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type passwd(passwdSEXP);
    Rcpp::traits::input_parameter< double >::type maxmem(maxmemSEXP);
    Rcpp::traits::input_parameter< double >::type maxtime(maxtimeSEXP);
    rcpp_result_gen = Rcpp::wrap(hashPassword(passwd, maxmem, maxtime));
    return rcpp_result_gen;
END_RCPP
}
// verifyPassword
bool verifyPassword(const std::string& hash, const std::string& passwd);
RcppExport SEXP _scrypt_verifyPassword(SEXP hashSEXP, SEXP passwdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type hash(hashSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type passwd(passwdSEXP);
    rcpp_result_gen = Rcpp::wrap(verifyPassword(hash, passwd));
    return rcpp_result_gen;
END_RCPP
}
// scrypt
RawVector scrypt(RawVector passwd, RawVector salt, uint32_t n, uint32_t r, uint32_t p, uint32_t length);
RcppExport SEXP _scrypt_scrypt(SEXP passwdSEXP, SEXP saltSEXP, SEXP nSEXP, SEXP rSEXP, SEXP pSEXP, SEXP lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type passwd(passwdSEXP);
    Rcpp::traits::input_parameter< RawVector >::type salt(saltSEXP);
    Rcpp::traits::input_parameter< uint32_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< uint32_t >::type r(rSEXP);
    Rcpp::traits::input_parameter< uint32_t >::type p(pSEXP);
    Rcpp::traits::input_parameter< uint32_t >::type length(lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(scrypt(passwd, salt, n, r, p, length));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_scrypt_hashPassword", (DL_FUNC) &_scrypt_hashPassword, 3},
    {"_scrypt_verifyPassword", (DL_FUNC) &_scrypt_verifyPassword, 2},
    {"_scrypt_scrypt", (DL_FUNC) &_scrypt_scrypt, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_scrypt(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}