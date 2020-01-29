// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// UTest1
Rcpp::List UTest1();
RcppExport SEXP _RMixtCompIO_UTest1() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(UTest1());
    return rcpp_result_gen;
END_RCPP
}
// UTest2
bool UTest2();
RcppExport SEXP _RMixtCompIO_UTest2() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(UTest2());
    return rcpp_result_gen;
END_RCPP
}
// UTest3
std::string UTest3();
RcppExport SEXP _RMixtCompIO_UTest3() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(UTest3());
    return rcpp_result_gen;
END_RCPP
}
// UTest4
Rcpp::List UTest4(const Rcpp::List& l);
RcppExport SEXP _RMixtCompIO_UTest4(SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(UTest4(l));
    return rcpp_result_gen;
END_RCPP
}
// UTest5
Rcpp::List UTest5(const Rcpp::List& l);
RcppExport SEXP _RMixtCompIO_UTest5(SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(UTest5(l));
    return rcpp_result_gen;
END_RCPP
}
// UTest6
bool UTest6();
RcppExport SEXP _RMixtCompIO_UTest6() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(UTest6());
    return rcpp_result_gen;
END_RCPP
}
// dummyTest
SEXP dummyTest();
RcppExport SEXP _RMixtCompIO_dummyTest() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(dummyTest());
    return rcpp_result_gen;
END_RCPP
}
// rmc
Rcpp::List rmc(Rcpp::List algoR, Rcpp::List dataR, Rcpp::List descR, Rcpp::List resLearnR);
RcppExport SEXP _RMixtCompIO_rmc(SEXP algoRSEXP, SEXP dataRSEXP, SEXP descRSEXP, SEXP resLearnRSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type algoR(algoRSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type dataR(dataRSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type descR(descRSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type resLearnR(resLearnRSEXP);
    rcpp_result_gen = Rcpp::wrap(rmc(algoR, dataR, descR, resLearnR));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RMixtCompIO_UTest1", (DL_FUNC) &_RMixtCompIO_UTest1, 0},
    {"_RMixtCompIO_UTest2", (DL_FUNC) &_RMixtCompIO_UTest2, 0},
    {"_RMixtCompIO_UTest3", (DL_FUNC) &_RMixtCompIO_UTest3, 0},
    {"_RMixtCompIO_UTest4", (DL_FUNC) &_RMixtCompIO_UTest4, 1},
    {"_RMixtCompIO_UTest5", (DL_FUNC) &_RMixtCompIO_UTest5, 1},
    {"_RMixtCompIO_UTest6", (DL_FUNC) &_RMixtCompIO_UTest6, 0},
    {"_RMixtCompIO_dummyTest", (DL_FUNC) &_RMixtCompIO_dummyTest, 0},
    {"_RMixtCompIO_rmc", (DL_FUNC) &_RMixtCompIO_rmc, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_RMixtCompIO(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}