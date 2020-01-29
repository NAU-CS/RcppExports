// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// dilateImage
Rcpp::IntegerMatrix dilateImage(Rcpp::IntegerMatrix mat, int kernel, int niter);
RcppExport SEXP StereoMorph_dilateImage(SEXP matSEXP, SEXP kernelSEXP, SEXP niterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< int >::type kernel(kernelSEXP);
    Rcpp::traits::input_parameter< int >::type niter(niterSEXP);
    rcpp_result_gen = Rcpp::wrap(dilateImage(mat, kernel, niter));
    return rcpp_result_gen;
END_RCPP
}
// drawRectangle
Rcpp::IntegerMatrix drawRectangle(Rcpp::IntegerMatrix mat, Rcpp::IntegerVector corner1, Rcpp::IntegerVector corner2, int value, int thickness);
RcppExport SEXP StereoMorph_drawRectangle(SEXP matSEXP, SEXP corner1SEXP, SEXP corner2SEXP, SEXP valueSEXP, SEXP thicknessSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type corner1(corner1SEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type corner2(corner2SEXP);
    Rcpp::traits::input_parameter< int >::type value(valueSEXP);
    Rcpp::traits::input_parameter< int >::type thickness(thicknessSEXP);
    rcpp_result_gen = Rcpp::wrap(drawRectangle(mat, corner1, corner2, value, thickness));
    return rcpp_result_gen;
END_RCPP
}
// equalizeImageHist
Rcpp::NumericMatrix equalizeImageHist(Rcpp::NumericMatrix mat);
RcppExport SEXP StereoMorph_equalizeImageHist(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(equalizeImageHist(mat));
    return rcpp_result_gen;
END_RCPP
}
// erodeImage
Rcpp::IntegerMatrix erodeImage(Rcpp::IntegerMatrix mat, int kernel, int niter);
RcppExport SEXP StereoMorph_erodeImage(SEXP matSEXP, SEXP kernelSEXP, SEXP niterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< int >::type kernel(kernelSEXP);
    Rcpp::traits::input_parameter< int >::type niter(niterSEXP);
    rcpp_result_gen = Rcpp::wrap(erodeImage(mat, kernel, niter));
    return rcpp_result_gen;
END_RCPP
}
// findBoundaryPoints
Rcpp::IntegerMatrix findBoundaryPoints(Rcpp::IntegerMatrix mat);
RcppExport SEXP StereoMorph_findBoundaryPoints(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(findBoundaryPoints(mat));
    return rcpp_result_gen;
END_RCPP
}
// findCornerSubPix
Rcpp::NumericMatrix findCornerSubPix(Rcpp::NumericMatrix image, Rcpp::IntegerMatrix corners, int win, int max_iter, double criteria);
RcppExport SEXP StereoMorph_findCornerSubPix(SEXP imageSEXP, SEXP cornersSEXP, SEXP winSEXP, SEXP max_iterSEXP, SEXP criteriaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type image(imageSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type corners(cornersSEXP);
    Rcpp::traits::input_parameter< int >::type win(winSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< double >::type criteria(criteriaSEXP);
    rcpp_result_gen = Rcpp::wrap(findCornerSubPix(image, corners, win, max_iter, criteria));
    return rcpp_result_gen;
END_RCPP
}
// generateQuads
Rcpp::IntegerMatrix generateQuads(Rcpp::IntegerMatrix binary_mat, Rcpp::IntegerMatrix edge_mat, int perim_min, int perim_max, double quad_fit_max, double poly_cont_min, double poly_cont_max, double poly_asp_min, int approx_thresh);
RcppExport SEXP StereoMorph_generateQuads(SEXP binary_matSEXP, SEXP edge_matSEXP, SEXP perim_minSEXP, SEXP perim_maxSEXP, SEXP quad_fit_maxSEXP, SEXP poly_cont_minSEXP, SEXP poly_cont_maxSEXP, SEXP poly_asp_minSEXP, SEXP approx_threshSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type binary_mat(binary_matSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type edge_mat(edge_matSEXP);
    Rcpp::traits::input_parameter< int >::type perim_min(perim_minSEXP);
    Rcpp::traits::input_parameter< int >::type perim_max(perim_maxSEXP);
    Rcpp::traits::input_parameter< double >::type quad_fit_max(quad_fit_maxSEXP);
    Rcpp::traits::input_parameter< double >::type poly_cont_min(poly_cont_minSEXP);
    Rcpp::traits::input_parameter< double >::type poly_cont_max(poly_cont_maxSEXP);
    Rcpp::traits::input_parameter< double >::type poly_asp_min(poly_asp_minSEXP);
    Rcpp::traits::input_parameter< int >::type approx_thresh(approx_threshSEXP);
    rcpp_result_gen = Rcpp::wrap(generateQuads(binary_mat, edge_mat, perim_min, perim_max, quad_fit_max, poly_cont_min, poly_cont_max, poly_asp_min, approx_thresh));
    return rcpp_result_gen;
END_RCPP
}
// intCornersFromQuads
Rcpp::IntegerMatrix intCornersFromQuads(Rcpp::IntegerMatrix quads, int max_dist);
RcppExport SEXP StereoMorph_intCornersFromQuads(SEXP quadsSEXP, SEXP max_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type quads(quadsSEXP);
    Rcpp::traits::input_parameter< int >::type max_dist(max_distSEXP);
    rcpp_result_gen = Rcpp::wrap(intCornersFromQuads(quads, max_dist));
    return rcpp_result_gen;
END_RCPP
}
// orderCorners
Rcpp::IntegerMatrix orderCorners(Rcpp::IntegerMatrix int_corners, int nx, int ny);
RcppExport SEXP StereoMorph_orderCorners(SEXP int_cornersSEXP, SEXP nxSEXP, SEXP nySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type int_corners(int_cornersSEXP);
    Rcpp::traits::input_parameter< int >::type nx(nxSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    rcpp_result_gen = Rcpp::wrap(orderCorners(int_corners, nx, ny));
    return rcpp_result_gen;
END_RCPP
}
// meanBlurImage
Rcpp::NumericMatrix meanBlurImage(Rcpp::NumericMatrix mat, int kernel);
RcppExport SEXP StereoMorph_meanBlurImage(SEXP matSEXP, SEXP kernelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< int >::type kernel(kernelSEXP);
    rcpp_result_gen = Rcpp::wrap(meanBlurImage(mat, kernel));
    return rcpp_result_gen;
END_RCPP
}
// rgbToGray
Rcpp::NumericMatrix rgbToGray(Rcpp::NumericMatrix ch1, Rcpp::NumericMatrix ch2, Rcpp::NumericMatrix ch3);
RcppExport SEXP StereoMorph_rgbToGray(SEXP ch1SEXP, SEXP ch2SEXP, SEXP ch3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type ch1(ch1SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type ch2(ch2SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type ch3(ch3SEXP);
    rcpp_result_gen = Rcpp::wrap(rgbToGray(ch1, ch2, ch3));
    return rcpp_result_gen;
END_RCPP
}
// thresholdImageMatrix
Rcpp::IntegerMatrix thresholdImageMatrix(Rcpp::NumericMatrix mat, Rcpp::NumericMatrix thresh_mat, double delta, int type);
RcppExport SEXP StereoMorph_thresholdImageMatrix(SEXP matSEXP, SEXP thresh_matSEXP, SEXP deltaSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type thresh_mat(thresh_matSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(thresholdImageMatrix(mat, thresh_mat, delta, type));
    return rcpp_result_gen;
END_RCPP
}