// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/fastrtext.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// add_prefix
CharacterVector add_prefix(const CharacterVector& texts, CharacterVector prefix);
static SEXP _fastrtext_add_prefix_try(SEXP textsSEXP, SEXP prefixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const CharacterVector& >::type texts(textsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type prefix(prefixSEXP);
    rcpp_result_gen = Rcpp::wrap(add_prefix(texts, prefix));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _fastrtext_add_prefix(SEXP textsSEXP, SEXP prefixSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_fastrtext_add_prefix_try(textsSEXP, prefixSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// add_pr
std::string add_pr(const std::string& line, const std::string& prefix);
static SEXP _fastrtext_add_pr_try(SEXP lineSEXP, SEXP prefixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type line(lineSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type prefix(prefixSEXP);
    rcpp_result_gen = Rcpp::wrap(add_pr(line, prefix));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _fastrtext_add_pr(SEXP lineSEXP, SEXP prefixSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_fastrtext_add_pr_try(lineSEXP, prefixSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _fastrtext_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("CharacterVector(*add_prefix)(const CharacterVector&,CharacterVector)");
        signatures.insert("std::string(*add_pr)(const std::string&,const std::string&)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _fastrtext_RcppExport_registerCCallable() { 
    R_RegisterCCallable("fastrtext", "_fastrtext_add_prefix", (DL_FUNC)_fastrtext_add_prefix_try);
    R_RegisterCCallable("fastrtext", "_fastrtext_add_pr", (DL_FUNC)_fastrtext_add_pr_try);
    R_RegisterCCallable("fastrtext", "_fastrtext_RcppExport_validate", (DL_FUNC)_fastrtext_RcppExport_validate);
    return R_NilValue;
}

RcppExport SEXP _rcpp_module_boot_FASTRTEXT_MODULE();

static const R_CallMethodDef CallEntries[] = {
    {"_fastrtext_add_prefix", (DL_FUNC) &_fastrtext_add_prefix, 2},
    {"_fastrtext_add_pr", (DL_FUNC) &_fastrtext_add_pr, 2},
    {"_rcpp_module_boot_FASTRTEXT_MODULE", (DL_FUNC) &_rcpp_module_boot_FASTRTEXT_MODULE, 0},
    {"_fastrtext_RcppExport_registerCCallable", (DL_FUNC) &_fastrtext_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastrtext(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}