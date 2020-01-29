// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// anytime_cpp
Rcpp::NumericVector anytime_cpp(SEXP x, const std::string& tz, const bool asUTC, const bool asDate, const bool useR, const bool oldHeuristic);
RcppExport SEXP _anytime_anytime_cpp(SEXP xSEXP, SEXP tzSEXP, SEXP asUTCSEXP, SEXP asDateSEXP, SEXP useRSEXP, SEXP oldHeuristicSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type tz(tzSEXP);
    Rcpp::traits::input_parameter< const bool >::type asUTC(asUTCSEXP);
    Rcpp::traits::input_parameter< const bool >::type asDate(asDateSEXP);
    Rcpp::traits::input_parameter< const bool >::type useR(useRSEXP);
    Rcpp::traits::input_parameter< const bool >::type oldHeuristic(oldHeuristicSEXP);
    rcpp_result_gen = Rcpp::wrap(anytime_cpp(x, tz, asUTC, asDate, useR, oldHeuristic));
    return rcpp_result_gen;
END_RCPP
}
// getFormats
std::vector<std::string> getFormats();
RcppExport SEXP _anytime_getFormats() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getFormats());
    return rcpp_result_gen;
END_RCPP
}
// addFormats
void addFormats(Rcpp::CharacterVector fmt);
RcppExport SEXP _anytime_addFormats(SEXP fmtSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type fmt(fmtSEXP);
    addFormats(fmt);
    return R_NilValue;
END_RCPP
}
// removeFormats
void removeFormats(Rcpp::CharacterVector fmt);
RcppExport SEXP _anytime_removeFormats(SEXP fmtSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type fmt(fmtSEXP);
    removeFormats(fmt);
    return R_NilValue;
END_RCPP
}
// testFormat_impl
Rcpp::NumericVector testFormat_impl(const std::string fmt, const std::string s, const std::string tz);
RcppExport SEXP _anytime_testFormat_impl(SEXP fmtSEXP, SEXP sSEXP, SEXP tzSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string >::type fmt(fmtSEXP);
    Rcpp::traits::input_parameter< const std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< const std::string >::type tz(tzSEXP);
    rcpp_result_gen = Rcpp::wrap(testFormat_impl(fmt, s, tz));
    return rcpp_result_gen;
END_RCPP
}
// testOutput_impl
std::string testOutput_impl(const std::string fmt, const std::string s);
RcppExport SEXP _anytime_testOutput_impl(SEXP fmtSEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string >::type fmt(fmtSEXP);
    Rcpp::traits::input_parameter< const std::string >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(testOutput_impl(fmt, s));
    return rcpp_result_gen;
END_RCPP
}
// setDebug
bool setDebug(const bool mode);
RcppExport SEXP _anytime_setDebug(SEXP modeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const bool >::type mode(modeSEXP);
    rcpp_result_gen = Rcpp::wrap(setDebug(mode));
    return rcpp_result_gen;
END_RCPP
}
// fmt
std::vector<std::string> fmt(Rcpp::NumericVector x);
RcppExport SEXP _anytime_fmt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fmt(x));
    return rcpp_result_gen;
END_RCPP
}
// setMaxIntAsYYYYMMDD
void setMaxIntAsYYYYMMDD(const int val);
RcppExport SEXP _anytime_setMaxIntAsYYYYMMDD(SEXP valSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type val(valSEXP);
    setMaxIntAsYYYYMMDD(val);
    return R_NilValue;
END_RCPP
}
// setMaxIntAsDate
void setMaxIntAsDate(const int val);
RcppExport SEXP _anytime_setMaxIntAsDate(SEXP valSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type val(valSEXP);
    setMaxIntAsDate(val);
    return R_NilValue;
END_RCPP
}
// setInitialTZ
void setInitialTZ(std::string tz);
RcppExport SEXP _anytime_setInitialTZ(SEXP tzSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type tz(tzSEXP);
    setInitialTZ(tz);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_anytime_anytime_cpp", (DL_FUNC) &_anytime_anytime_cpp, 6},
    {"_anytime_getFormats", (DL_FUNC) &_anytime_getFormats, 0},
    {"_anytime_addFormats", (DL_FUNC) &_anytime_addFormats, 1},
    {"_anytime_removeFormats", (DL_FUNC) &_anytime_removeFormats, 1},
    {"_anytime_testFormat_impl", (DL_FUNC) &_anytime_testFormat_impl, 3},
    {"_anytime_testOutput_impl", (DL_FUNC) &_anytime_testOutput_impl, 2},
    {"_anytime_setDebug", (DL_FUNC) &_anytime_setDebug, 1},
    {"_anytime_fmt", (DL_FUNC) &_anytime_fmt, 1},
    {"_anytime_setMaxIntAsYYYYMMDD", (DL_FUNC) &_anytime_setMaxIntAsYYYYMMDD, 1},
    {"_anytime_setMaxIntAsDate", (DL_FUNC) &_anytime_setMaxIntAsDate, 1},
    {"_anytime_setInitialTZ", (DL_FUNC) &_anytime_setInitialTZ, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_anytime(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}