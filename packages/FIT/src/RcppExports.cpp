// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// initParamsAndDevs
Rcpp::List initParamsAndDevs(Rcpp::NumericMatrix const exprs, Rcpp::NumericMatrix const weights, Rcpp::DataFrame const attribute_data, Rcpp::DataFrame const weather_data, Rcpp::CharacterVector const env_factors, Rcpp::List const grid_coordinates, Rcpp::IntegerVector const data_step, Rcpp::IntegerVector const time_step);
RcppExport SEXP _FIT_initParamsAndDevs(SEXP exprsSEXP, SEXP weightsSEXP, SEXP attribute_dataSEXP, SEXP weather_dataSEXP, SEXP env_factorsSEXP, SEXP grid_coordinatesSEXP, SEXP data_stepSEXP, SEXP time_stepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix const >::type exprs(exprsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix const >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type attribute_data(attribute_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type weather_data(weather_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector const >::type env_factors(env_factorsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List const >::type grid_coordinates(grid_coordinatesSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type data_step(data_stepSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type time_step(time_stepSEXP);
    rcpp_result_gen = Rcpp::wrap(initParamsAndDevs(exprs, weights, attribute_data, weather_data, env_factors, grid_coordinates, data_step, time_step));
    return rcpp_result_gen;
END_RCPP
}
// inputVars
Rcpp::NumericMatrix inputVars(Rcpp::NumericVector const params, Rcpp::CharacterVector const env, Rcpp::DataFrame const attribute_data, Rcpp::DataFrame const weather_data, Rcpp::IntegerVector const data_step, Rcpp::IntegerVector const time_step);
RcppExport SEXP _FIT_inputVars(SEXP paramsSEXP, SEXP envSEXP, SEXP attribute_dataSEXP, SEXP weather_dataSEXP, SEXP data_stepSEXP, SEXP time_stepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector const >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector const >::type env(envSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type attribute_data(attribute_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type weather_data(weather_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type data_step(data_stepSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type time_step(time_stepSEXP);
    rcpp_result_gen = Rcpp::wrap(inputVars(params, env, attribute_data, weather_data, data_step, time_step));
    return rcpp_result_gen;
END_RCPP
}
// devLm
Rcpp::NumericVector devLm(Rcpp::NumericVector const params, Rcpp::CharacterVector const env, Rcpp::NumericVector const expr, Rcpp::NumericVector const weight, Rcpp::DataFrame const attribute_data, Rcpp::DataFrame const weather_data, Rcpp::IntegerVector const data_step, Rcpp::IntegerVector const time_step);
RcppExport SEXP _FIT_devLm(SEXP paramsSEXP, SEXP envSEXP, SEXP exprSEXP, SEXP weightSEXP, SEXP attribute_dataSEXP, SEXP weather_dataSEXP, SEXP data_stepSEXP, SEXP time_stepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector const >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector const >::type env(envSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector const >::type expr(exprSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector const >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type attribute_data(attribute_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type weather_data(weather_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type data_step(data_stepSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type time_step(time_stepSEXP);
    rcpp_result_gen = Rcpp::wrap(devLm(params, env, expr, weight, attribute_data, weather_data, data_step, time_step));
    return rcpp_result_gen;
END_RCPP
}
// coefsLm
Rcpp::NumericVector coefsLm(Rcpp::NumericVector const params, Rcpp::CharacterVector const env, Rcpp::NumericVector const expr, Rcpp::NumericVector const weight, Rcpp::DataFrame const attribute_data, Rcpp::DataFrame const weather_data, Rcpp::IntegerVector const data_step, Rcpp::IntegerVector const time_step);
RcppExport SEXP _FIT_coefsLm(SEXP paramsSEXP, SEXP envSEXP, SEXP exprSEXP, SEXP weightSEXP, SEXP attribute_dataSEXP, SEXP weather_dataSEXP, SEXP data_stepSEXP, SEXP time_stepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector const >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector const >::type env(envSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector const >::type expr(exprSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector const >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type attribute_data(attribute_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame const >::type weather_data(weather_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type data_step(data_stepSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector const >::type time_step(time_stepSEXP);
    rcpp_result_gen = Rcpp::wrap(coefsLm(params, env, expr, weight, attribute_data, weather_data, data_step, time_step));
    return rcpp_result_gen;
END_RCPP
}
// zzzRcppExportBug
Rcpp::NumericMatrix zzzRcppExportBug();
RcppExport SEXP _FIT_zzzRcppExportBug() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(zzzRcppExportBug());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FIT_initParamsAndDevs", (DL_FUNC) &_FIT_initParamsAndDevs, 8},
    {"_FIT_inputVars", (DL_FUNC) &_FIT_inputVars, 6},
    {"_FIT_devLm", (DL_FUNC) &_FIT_devLm, 8},
    {"_FIT_coefsLm", (DL_FUNC) &_FIT_coefsLm, 8},
    {"_FIT_zzzRcppExportBug", (DL_FUNC) &_FIT_zzzRcppExportBug, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_FIT(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}