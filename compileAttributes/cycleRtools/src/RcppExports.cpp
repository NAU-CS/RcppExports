// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Wbal_
std::vector<double> Wbal_(NumericVector t, NumericVector P, double CP);
RcppExport SEXP _cycleRtools_Wbal_(SEXP tSEXP, SEXP PSEXP, SEXP CPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type P(PSEXP);
    Rcpp::traits::input_parameter< double >::type CP(CPSEXP);
    rcpp_result_gen = Rcpp::wrap(Wbal_(t, P, CP));
    return rcpp_result_gen;
END_RCPP
}
// diff_section
std::vector<double> diff_section(NumericVector x, int br);
RcppExport SEXP _cycleRtools_diff_section(SEXP xSEXP, SEXP brSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type br(brSEXP);
    rcpp_result_gen = Rcpp::wrap(diff_section(x, br));
    return rcpp_result_gen;
END_RCPP
}
// Diff
std::vector<double> Diff(NumericVector x);
RcppExport SEXP _cycleRtools_Diff(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Diff(x));
    return rcpp_result_gen;
END_RCPP
}
// mmv2
std::vector<double> mmv2(NumericVector x, NumericVector windows);
RcppExport SEXP _cycleRtools_mmv2(SEXP xSEXP, SEXP windowsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type windows(windowsSEXP);
    rcpp_result_gen = Rcpp::wrap(mmv2(x, windows));
    return rcpp_result_gen;
END_RCPP
}
// na_split
std::vector<double> na_split(NumericVector x);
RcppExport SEXP _cycleRtools_na_split(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(na_split(x));
    return rcpp_result_gen;
END_RCPP
}
// ema_weights
std::vector<double> ema_weights(double len);
RcppExport SEXP _cycleRtools_ema_weights(SEXP lenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type len(lenSEXP);
    rcpp_result_gen = Rcpp::wrap(ema_weights(len));
    return rcpp_result_gen;
END_RCPP
}
// mean_weights
std::vector<double> mean_weights(double len);
RcppExport SEXP _cycleRtools_mean_weights(SEXP lenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type len(lenSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_weights(len));
    return rcpp_result_gen;
END_RCPP
}
// rollmean_
std::vector<double> rollmean_(NumericVector x, double window, bool ema, bool narm);
RcppExport SEXP _cycleRtools_rollmean_(SEXP xSEXP, SEXP windowSEXP, SEXP emaSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type window(windowSEXP);
    Rcpp::traits::input_parameter< bool >::type ema(emaSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    rcpp_result_gen = Rcpp::wrap(rollmean_(x, window, ema, narm));
    return rcpp_result_gen;
END_RCPP
}
// rollmean_nunif
std::vector<double> rollmean_nunif(NumericVector x, NumericVector t, double window);
RcppExport SEXP _cycleRtools_rollmean_nunif(SEXP xSEXP, SEXP tSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< double >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(rollmean_nunif(x, t, window));
    return rcpp_result_gen;
END_RCPP
}
// zone_index_
std::vector<double> zone_index_(NumericVector x, NumericVector zb);
RcppExport SEXP _cycleRtools_zone_index_(SEXP xSEXP, SEXP zbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type zb(zbSEXP);
    rcpp_result_gen = Rcpp::wrap(zone_index_(x, zb));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cycleRtools_Wbal_", (DL_FUNC) &_cycleRtools_Wbal_, 3},
    {"_cycleRtools_diff_section", (DL_FUNC) &_cycleRtools_diff_section, 2},
    {"_cycleRtools_Diff", (DL_FUNC) &_cycleRtools_Diff, 1},
    {"_cycleRtools_mmv2", (DL_FUNC) &_cycleRtools_mmv2, 2},
    {"_cycleRtools_na_split", (DL_FUNC) &_cycleRtools_na_split, 1},
    {"_cycleRtools_ema_weights", (DL_FUNC) &_cycleRtools_ema_weights, 1},
    {"_cycleRtools_mean_weights", (DL_FUNC) &_cycleRtools_mean_weights, 1},
    {"_cycleRtools_rollmean_", (DL_FUNC) &_cycleRtools_rollmean_, 4},
    {"_cycleRtools_rollmean_nunif", (DL_FUNC) &_cycleRtools_rollmean_nunif, 3},
    {"_cycleRtools_zone_index_", (DL_FUNC) &_cycleRtools_zone_index_, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_cycleRtools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}