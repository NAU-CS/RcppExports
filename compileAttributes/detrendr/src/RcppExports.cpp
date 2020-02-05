// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// int_anyNA
bool int_anyNA(IntegerVector x);
RcppExport SEXP _detrendr_int_anyNA(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(int_anyNA(x));
    return rcpp_result_gen;
END_RCPP
}
// dbl_anyNA
bool dbl_anyNA(NumericVector x);
RcppExport SEXP _detrendr_dbl_anyNA(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(dbl_anyNA(x));
    return rcpp_result_gen;
END_RCPP
}
// brightness_cols_
NumericVector brightness_cols_(IntegerMatrix cols);
RcppExport SEXP _detrendr_brightness_cols_(SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(brightness_cols_(cols));
    return rcpp_result_gen;
END_RCPP
}
// brightness_cols_given_mean_
NumericVector brightness_cols_given_mean_(IntegerMatrix cols, NumericVector means);
RcppExport SEXP _detrendr_brightness_cols_given_mean_(SEXP colsSEXP, SEXP meansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type means(meansSEXP);
    rcpp_result_gen = Rcpp::wrap(brightness_cols_given_mean_(cols, means));
    return rcpp_result_gen;
END_RCPP
}
// mean_cols_
NumericVector mean_cols_(IntegerMatrix cols);
RcppExport SEXP _detrendr_mean_cols_(SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_cols_(cols));
    return rcpp_result_gen;
END_RCPP
}
// var_cols_given_mean_
NumericVector var_cols_given_mean_(IntegerMatrix cols, NumericVector means);
RcppExport SEXP _detrendr_var_cols_given_mean_(SEXP colsSEXP, SEXP meansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type means(meansSEXP);
    rcpp_result_gen = Rcpp::wrap(var_cols_given_mean_(cols, means));
    return rcpp_result_gen;
END_RCPP
}
// sum_cols_
NumericVector sum_cols_(IntegerMatrix cols);
RcppExport SEXP _detrendr_sum_cols_(SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_cols_(cols));
    return rcpp_result_gen;
END_RCPP
}
// mean_frames_
NumericVector mean_frames_(NumericVector arr3d);
RcppExport SEXP _detrendr_mean_frames_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_frames_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// sum_frames_
NumericVector sum_frames_(NumericVector arr3d);
RcppExport SEXP _detrendr_sum_frames_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_frames_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// int_sum_frames_na_omit
NumericVector int_sum_frames_na_omit(IntegerVector arr3d);
RcppExport SEXP _detrendr_int_sum_frames_na_omit(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(int_sum_frames_na_omit(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// dbl_sum_frames_na_omit
NumericVector dbl_sum_frames_na_omit(NumericVector arr3d);
RcppExport SEXP _detrendr_dbl_sum_frames_na_omit(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(dbl_sum_frames_na_omit(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// int_mean_frames_na_omit
NumericVector int_mean_frames_na_omit(IntegerVector arr3d);
RcppExport SEXP _detrendr_int_mean_frames_na_omit(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(int_mean_frames_na_omit(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// dbl_mean_frames_na_omit
NumericVector dbl_mean_frames_na_omit(NumericVector arr3d);
RcppExport SEXP _detrendr_dbl_mean_frames_na_omit(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(dbl_mean_frames_na_omit(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// int_anyNA_pillars
LogicalMatrix int_anyNA_pillars(IntegerVector arr3d);
RcppExport SEXP _detrendr_int_anyNA_pillars(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(int_anyNA_pillars(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// dbl_anyNA_pillars
LogicalMatrix dbl_anyNA_pillars(NumericVector arr3d);
RcppExport SEXP _detrendr_dbl_anyNA_pillars(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(dbl_anyNA_pillars(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// sum_pillars_
NumericMatrix sum_pillars_(NumericVector arr3d);
RcppExport SEXP _detrendr_sum_pillars_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_pillars_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// mean_pillars_
NumericMatrix mean_pillars_(NumericVector arr3d);
RcppExport SEXP _detrendr_mean_pillars_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_pillars_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// var_pillars_
NumericMatrix var_pillars_(NumericVector arr3d);
RcppExport SEXP _detrendr_var_pillars_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(var_pillars_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// median_pillars_
NumericMatrix median_pillars_(NumericVector arr3d);
RcppExport SEXP _detrendr_median_pillars_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(median_pillars_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// brightness_pillars_
NumericMatrix brightness_pillars_(NumericVector arr3d);
RcppExport SEXP _detrendr_brightness_pillars_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(brightness_pillars_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// pillars_to_cols_
NumericMatrix pillars_to_cols_(NumericVector arr3d);
RcppExport SEXP _detrendr_pillars_to_cols_(SEXP arr3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr3d(arr3dSEXP);
    rcpp_result_gen = Rcpp::wrap(pillars_to_cols_(arr3d));
    return rcpp_result_gen;
END_RCPP
}
// cols_to_pillars_
NumericVector cols_to_pillars_(NumericMatrix mat, IntegerVector output_dim);
RcppExport SEXP _detrendr_cols_to_pillars_(SEXP matSEXP, SEXP output_dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type output_dim(output_dimSEXP);
    rcpp_result_gen = Rcpp::wrap(cols_to_pillars_(mat, output_dim));
    return rcpp_result_gen;
END_RCPP
}
// myrbernoulli_
IntegerVector myrbernoulli_(NumericVector p, int seed);
RcppExport SEXP _detrendr_myrbernoulli_(SEXP pSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(myrbernoulli_(p, seed));
    return rcpp_result_gen;
END_RCPP
}
// rfromboxes_
IntegerVector rfromboxes_(double n, IntegerVector balls, NumericVector weights, int seed, LogicalVector quick);
RcppExport SEXP _detrendr_rfromboxes_(SEXP nSEXP, SEXP ballsSEXP, SEXP weightsSEXP, SEXP seedSEXP, SEXP quickSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type balls(ballsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type quick(quickSEXP);
    rcpp_result_gen = Rcpp::wrap(rfromboxes_(n, balls, weights, seed, quick));
    return rcpp_result_gen;
END_RCPP
}
// rtoboxes_
IntegerVector rtoboxes_(double n, double boxes, NumericVector weights, IntegerVector capacities, int seed, LogicalVector quick);
RcppExport SEXP _detrendr_rtoboxes_(SEXP nSEXP, SEXP boxesSEXP, SEXP weightsSEXP, SEXP capacitiesSEXP, SEXP seedSEXP, SEXP quickSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type boxes(boxesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type capacities(capacitiesSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type quick(quickSEXP);
    rcpp_result_gen = Rcpp::wrap(rtoboxes_(n, boxes, weights, capacities, seed, quick));
    return rcpp_result_gen;
END_RCPP
}
// px_take_arr3d
IntegerVector px_take_arr3d(IntegerVector arr3d, IntegerVector frames_losing, int seed);
RcppExport SEXP _detrendr_px_take_arr3d(SEXP arr3dSEXP, SEXP frames_losingSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type arr3d(arr3dSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type frames_losing(frames_losingSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(px_take_arr3d(arr3d, frames_losing, seed));
    return rcpp_result_gen;
END_RCPP
}
// px_take_mat
NumericVector px_take_mat(NumericMatrix mat, NumericMatrix mat_orig, NumericVector frames_losing, int seed);
RcppExport SEXP _detrendr_px_take_mat(SEXP matSEXP, SEXP mat_origSEXP, SEXP frames_losingSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_orig(mat_origSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type frames_losing(frames_losingSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(px_take_mat(mat, mat_orig, frames_losing, seed));
    return rcpp_result_gen;
END_RCPP
}
// brightness_rows_
NumericVector brightness_rows_(IntegerMatrix rows);
RcppExport SEXP _detrendr_brightness_rows_(SEXP rowsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type rows(rowsSEXP);
    rcpp_result_gen = Rcpp::wrap(brightness_rows_(rows));
    return rcpp_result_gen;
END_RCPP
}
// brightness_rows_given_mean_
NumericVector brightness_rows_given_mean_(IntegerMatrix rows, NumericVector means);
RcppExport SEXP _detrendr_brightness_rows_given_mean_(SEXP rowsSEXP, SEXP meansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type means(meansSEXP);
    rcpp_result_gen = Rcpp::wrap(brightness_rows_given_mean_(rows, means));
    return rcpp_result_gen;
END_RCPP
}
// mean_rows_
NumericVector mean_rows_(IntegerMatrix rows);
RcppExport SEXP _detrendr_mean_rows_(SEXP rowsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type rows(rowsSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_rows_(rows));
    return rcpp_result_gen;
END_RCPP
}
// var_rows_given_mean_
NumericVector var_rows_given_mean_(IntegerMatrix rows, NumericVector means);
RcppExport SEXP _detrendr_var_rows_given_mean_(SEXP rowsSEXP, SEXP meansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type means(meansSEXP);
    rcpp_result_gen = Rcpp::wrap(var_rows_given_mean_(rows, means));
    return rcpp_result_gen;
END_RCPP
}
// sum_rows_
NumericVector sum_rows_(IntegerMatrix rows);
RcppExport SEXP _detrendr_sum_rows_(SEXP rowsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type rows(rowsSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_rows_(rows));
    return rcpp_result_gen;
END_RCPP
}
// myrpois_
IntegerVector myrpois_(NumericVector means, int seed);
RcppExport SEXP _detrendr_myrpois_(SEXP meansSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type means(meansSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(myrpois_(means, seed));
    return rcpp_result_gen;
END_RCPP
}
// myrpois_frames_
IntegerMatrix myrpois_frames_(NumericVector means, std::size_t frame_length, int seed);
RcppExport SEXP _detrendr_myrpois_frames_(SEXP meansSEXP, SEXP frame_lengthSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type means(meansSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type frame_length(frame_lengthSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(myrpois_frames_(means, frame_length, seed));
    return rcpp_result_gen;
END_RCPP
}
// myrpois_frames_t_
IntegerMatrix myrpois_frames_t_(NumericVector means, std::size_t frame_length, int seed);
RcppExport SEXP _detrendr_myrpois_frames_t_(SEXP meansSEXP, SEXP frame_lengthSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type means(meansSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type frame_length(frame_lengthSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(myrpois_frames_t_(means, frame_length, seed));
    return rcpp_result_gen;
END_RCPP
}
// boxcar_smooth
NumericVector boxcar_smooth(NumericVector vec, std::size_t l);
RcppExport SEXP _detrendr_boxcar_smooth(SEXP vecSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(boxcar_smooth(vec, l));
    return rcpp_result_gen;
END_RCPP
}
// weighted_smooth
NumericVector weighted_smooth(NumericVector vec, NumericVector weights);
RcppExport SEXP _detrendr_weighted_smooth(SEXP vecSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_smooth(vec, weights));
    return rcpp_result_gen;
END_RCPP
}
// exp_smooth
NumericVector exp_smooth(NumericVector vec, double tau, std::size_t l);
RcppExport SEXP _detrendr_exp_smooth(SEXP vecSEXP, SEXP tauSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(exp_smooth(vec, tau, l));
    return rcpp_result_gen;
END_RCPP
}
// boxcar_smooth_rows_
NumericMatrix boxcar_smooth_rows_(NumericMatrix mat, std::size_t l);
RcppExport SEXP _detrendr_boxcar_smooth_rows_(SEXP matSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(boxcar_smooth_rows_(mat, l));
    return rcpp_result_gen;
END_RCPP
}
// boxcar_smooth_pillars_
NumericVector boxcar_smooth_pillars_(NumericVector arr, std::size_t l);
RcppExport SEXP _detrendr_boxcar_smooth_pillars_(SEXP arrSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(boxcar_smooth_pillars_(arr, l));
    return rcpp_result_gen;
END_RCPP
}
// exp_smooth_rows_
NumericMatrix exp_smooth_rows_(NumericMatrix mat, double tau, std::size_t l);
RcppExport SEXP _detrendr_exp_smooth_rows_(SEXP matSEXP, SEXP tauSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(exp_smooth_rows_(mat, tau, l));
    return rcpp_result_gen;
END_RCPP
}
// exp_smooth_pillars_
NumericVector exp_smooth_pillars_(NumericVector arr, double tau, int l);
RcppExport SEXP _detrendr_exp_smooth_pillars_(SEXP arrSEXP, SEXP tauSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(exp_smooth_pillars_(arr, tau, l));
    return rcpp_result_gen;
END_RCPP
}
// square_root_
NumericVector square_root_(NumericVector x);
RcppExport SEXP _detrendr_square_root_(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(square_root_(x));
    return rcpp_result_gen;
END_RCPP
}
// mat_add1s
IntegerMatrix mat_add1s(IntegerMatrix mat, IntegerMatrix add_pos);
RcppExport SEXP _detrendr_mat_add1s(SEXP matSEXP, SEXP add_posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type add_pos(add_posSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_add1s(mat, add_pos));
    return rcpp_result_gen;
END_RCPP
}
// vec_add1s
IntegerVector vec_add1s(IntegerVector vec, IntegerVector add_pos);
RcppExport SEXP _detrendr_vec_add1s(SEXP vecSEXP, SEXP add_posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type add_pos(add_posSEXP);
    rcpp_result_gen = Rcpp::wrap(vec_add1s(vec, add_pos));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_detrendr_int_anyNA", (DL_FUNC) &_detrendr_int_anyNA, 1},
    {"_detrendr_dbl_anyNA", (DL_FUNC) &_detrendr_dbl_anyNA, 1},
    {"_detrendr_brightness_cols_", (DL_FUNC) &_detrendr_brightness_cols_, 1},
    {"_detrendr_brightness_cols_given_mean_", (DL_FUNC) &_detrendr_brightness_cols_given_mean_, 2},
    {"_detrendr_mean_cols_", (DL_FUNC) &_detrendr_mean_cols_, 1},
    {"_detrendr_var_cols_given_mean_", (DL_FUNC) &_detrendr_var_cols_given_mean_, 2},
    {"_detrendr_sum_cols_", (DL_FUNC) &_detrendr_sum_cols_, 1},
    {"_detrendr_mean_frames_", (DL_FUNC) &_detrendr_mean_frames_, 1},
    {"_detrendr_sum_frames_", (DL_FUNC) &_detrendr_sum_frames_, 1},
    {"_detrendr_int_sum_frames_na_omit", (DL_FUNC) &_detrendr_int_sum_frames_na_omit, 1},
    {"_detrendr_dbl_sum_frames_na_omit", (DL_FUNC) &_detrendr_dbl_sum_frames_na_omit, 1},
    {"_detrendr_int_mean_frames_na_omit", (DL_FUNC) &_detrendr_int_mean_frames_na_omit, 1},
    {"_detrendr_dbl_mean_frames_na_omit", (DL_FUNC) &_detrendr_dbl_mean_frames_na_omit, 1},
    {"_detrendr_int_anyNA_pillars", (DL_FUNC) &_detrendr_int_anyNA_pillars, 1},
    {"_detrendr_dbl_anyNA_pillars", (DL_FUNC) &_detrendr_dbl_anyNA_pillars, 1},
    {"_detrendr_sum_pillars_", (DL_FUNC) &_detrendr_sum_pillars_, 1},
    {"_detrendr_mean_pillars_", (DL_FUNC) &_detrendr_mean_pillars_, 1},
    {"_detrendr_var_pillars_", (DL_FUNC) &_detrendr_var_pillars_, 1},
    {"_detrendr_median_pillars_", (DL_FUNC) &_detrendr_median_pillars_, 1},
    {"_detrendr_brightness_pillars_", (DL_FUNC) &_detrendr_brightness_pillars_, 1},
    {"_detrendr_pillars_to_cols_", (DL_FUNC) &_detrendr_pillars_to_cols_, 1},
    {"_detrendr_cols_to_pillars_", (DL_FUNC) &_detrendr_cols_to_pillars_, 2},
    {"_detrendr_myrbernoulli_", (DL_FUNC) &_detrendr_myrbernoulli_, 2},
    {"_detrendr_rfromboxes_", (DL_FUNC) &_detrendr_rfromboxes_, 5},
    {"_detrendr_rtoboxes_", (DL_FUNC) &_detrendr_rtoboxes_, 6},
    {"_detrendr_px_take_arr3d", (DL_FUNC) &_detrendr_px_take_arr3d, 3},
    {"_detrendr_px_take_mat", (DL_FUNC) &_detrendr_px_take_mat, 4},
    {"_detrendr_brightness_rows_", (DL_FUNC) &_detrendr_brightness_rows_, 1},
    {"_detrendr_brightness_rows_given_mean_", (DL_FUNC) &_detrendr_brightness_rows_given_mean_, 2},
    {"_detrendr_mean_rows_", (DL_FUNC) &_detrendr_mean_rows_, 1},
    {"_detrendr_var_rows_given_mean_", (DL_FUNC) &_detrendr_var_rows_given_mean_, 2},
    {"_detrendr_sum_rows_", (DL_FUNC) &_detrendr_sum_rows_, 1},
    {"_detrendr_myrpois_", (DL_FUNC) &_detrendr_myrpois_, 2},
    {"_detrendr_myrpois_frames_", (DL_FUNC) &_detrendr_myrpois_frames_, 3},
    {"_detrendr_myrpois_frames_t_", (DL_FUNC) &_detrendr_myrpois_frames_t_, 3},
    {"_detrendr_boxcar_smooth", (DL_FUNC) &_detrendr_boxcar_smooth, 2},
    {"_detrendr_weighted_smooth", (DL_FUNC) &_detrendr_weighted_smooth, 2},
    {"_detrendr_exp_smooth", (DL_FUNC) &_detrendr_exp_smooth, 3},
    {"_detrendr_boxcar_smooth_rows_", (DL_FUNC) &_detrendr_boxcar_smooth_rows_, 2},
    {"_detrendr_boxcar_smooth_pillars_", (DL_FUNC) &_detrendr_boxcar_smooth_pillars_, 2},
    {"_detrendr_exp_smooth_rows_", (DL_FUNC) &_detrendr_exp_smooth_rows_, 3},
    {"_detrendr_exp_smooth_pillars_", (DL_FUNC) &_detrendr_exp_smooth_pillars_, 3},
    {"_detrendr_square_root_", (DL_FUNC) &_detrendr_square_root_, 1},
    {"_detrendr_mat_add1s", (DL_FUNC) &_detrendr_mat_add1s, 2},
    {"_detrendr_vec_add1s", (DL_FUNC) &_detrendr_vec_add1s, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_detrendr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
