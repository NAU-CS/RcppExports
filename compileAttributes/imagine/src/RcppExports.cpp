// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// engine1
NumericMatrix engine1(NumericMatrix data, NumericMatrix kernel);
RcppExport SEXP _imagine_engine1(SEXP dataSEXP, SEXP kernelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type kernel(kernelSEXP);
    rcpp_result_gen = Rcpp::wrap(engine1(data, kernel));
    return rcpp_result_gen;
END_RCPP
}
// engine2
NumericMatrix engine2(NumericMatrix data, NumericMatrix kernel, double probs, int naVal);
RcppExport SEXP _imagine_engine2(SEXP dataSEXP, SEXP kernelSEXP, SEXP probsSEXP, SEXP naValSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type kernel(kernelSEXP);
    Rcpp::traits::input_parameter< double >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< int >::type naVal(naValSEXP);
    rcpp_result_gen = Rcpp::wrap(engine2(data, kernel, probs, naVal));
    return rcpp_result_gen;
END_RCPP
}
// engine3
NumericMatrix engine3(NumericMatrix data, int radius);
RcppExport SEXP _imagine_engine3(SEXP dataSEXP, SEXP radiusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type radius(radiusSEXP);
    rcpp_result_gen = Rcpp::wrap(engine3(data, radius));
    return rcpp_result_gen;
END_RCPP
}
// engine4
NumericMatrix engine4(NumericMatrix data, int radius, double probs, int naVal);
RcppExport SEXP _imagine_engine4(SEXP dataSEXP, SEXP radiusSEXP, SEXP probsSEXP, SEXP naValSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< int >::type naVal(naValSEXP);
    rcpp_result_gen = Rcpp::wrap(engine4(data, radius, probs, naVal));
    return rcpp_result_gen;
END_RCPP
}
// engine5
NumericMatrix engine5(NumericMatrix data, int naVal);
RcppExport SEXP _imagine_engine5(SEXP dataSEXP, SEXP naValSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type naVal(naValSEXP);
    rcpp_result_gen = Rcpp::wrap(engine5(data, naVal));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_imagine_engine1", (DL_FUNC) &_imagine_engine1, 2},
    {"_imagine_engine2", (DL_FUNC) &_imagine_engine2, 4},
    {"_imagine_engine3", (DL_FUNC) &_imagine_engine3, 2},
    {"_imagine_engine4", (DL_FUNC) &_imagine_engine4, 4},
    {"_imagine_engine5", (DL_FUNC) &_imagine_engine5, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_imagine(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}