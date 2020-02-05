// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// melt_dataframe
List melt_dataframe(const DataFrame& data, const IntegerVector& id_ind, const IntegerVector& measure_ind, String variable_name, String value_name, SEXP measure_attributes, bool factorsAsStrings, bool valueAsFactor);
RcppExport SEXP _reshape2_melt_dataframe(SEXP dataSEXP, SEXP id_indSEXP, SEXP measure_indSEXP, SEXP variable_nameSEXP, SEXP value_nameSEXP, SEXP measure_attributesSEXP, SEXP factorsAsStringsSEXP, SEXP valueAsFactorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DataFrame& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type id_ind(id_indSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type measure_ind(measure_indSEXP);
    Rcpp::traits::input_parameter< String >::type variable_name(variable_nameSEXP);
    Rcpp::traits::input_parameter< String >::type value_name(value_nameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type measure_attributes(measure_attributesSEXP);
    Rcpp::traits::input_parameter< bool >::type factorsAsStrings(factorsAsStringsSEXP);
    Rcpp::traits::input_parameter< bool >::type valueAsFactor(valueAsFactorSEXP);
    rcpp_result_gen = Rcpp::wrap(melt_dataframe(data, id_ind, measure_ind, variable_name, value_name, measure_attributes, factorsAsStrings, valueAsFactor));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_reshape2_melt_dataframe", (DL_FUNC) &_reshape2_melt_dataframe, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_reshape2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}