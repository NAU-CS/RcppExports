// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// simil_A
double simil_A(arma::rowvec spx_vec1, arma::rowvec spx_vec2, int wL, int wA, int wB);
RcppExport SEXP _SuperpixelImageSegmentation_simil_A(SEXP spx_vec1SEXP, SEXP spx_vec2SEXP, SEXP wLSEXP, SEXP wASEXP, SEXP wBSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::rowvec >::type spx_vec1(spx_vec1SEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type spx_vec2(spx_vec2SEXP);
    Rcpp::traits::input_parameter< int >::type wL(wLSEXP);
    Rcpp::traits::input_parameter< int >::type wA(wASEXP);
    Rcpp::traits::input_parameter< int >::type wB(wBSEXP);
    rcpp_result_gen = Rcpp::wrap(simil_A(spx_vec1, spx_vec2, wL, wA, wB));
    return rcpp_result_gen;
END_RCPP
}
// apply_rcpp
arma::mat apply_rcpp(arma::cube& input);
RcppExport SEXP _SuperpixelImageSegmentation_apply_rcpp(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube& >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(apply_rcpp(input));
    return rcpp_result_gen;
END_RCPP
}
// NAs_matrix
arma::uvec NAs_matrix(arma::mat& x);
RcppExport SEXP _SuperpixelImageSegmentation_NAs_matrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(NAs_matrix(x));
    return rcpp_result_gen;
END_RCPP
}
// is_mt_finite
bool is_mt_finite(arma::mat x);
RcppExport SEXP _SuperpixelImageSegmentation_is_mt_finite(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_mt_finite(x));
    return rcpp_result_gen;
END_RCPP
}
// image_segmentation
Rcpp::List image_segmentation(arma::cube input_image, std::string method, int num_superpixel, std::string kmeans_method, bool AP_data, bool use_median, int minib_kmeans_batch, double minib_kmeans_init_fraction, int kmeans_num_init, int kmeans_max_iters, std::string kmeans_initializer, std::string colour_type, double compactness_factor, bool adjust_centroids_and_return_masks, bool sim_normalize, int sim_wL, int sim_wA, int sim_wB, int sim_color_radius, bool verbose);
RcppExport SEXP _SuperpixelImageSegmentation_image_segmentation(SEXP input_imageSEXP, SEXP methodSEXP, SEXP num_superpixelSEXP, SEXP kmeans_methodSEXP, SEXP AP_dataSEXP, SEXP use_medianSEXP, SEXP minib_kmeans_batchSEXP, SEXP minib_kmeans_init_fractionSEXP, SEXP kmeans_num_initSEXP, SEXP kmeans_max_itersSEXP, SEXP kmeans_initializerSEXP, SEXP colour_typeSEXP, SEXP compactness_factorSEXP, SEXP adjust_centroids_and_return_masksSEXP, SEXP sim_normalizeSEXP, SEXP sim_wLSEXP, SEXP sim_wASEXP, SEXP sim_wBSEXP, SEXP sim_color_radiusSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type input_image(input_imageSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type num_superpixel(num_superpixelSEXP);
    Rcpp::traits::input_parameter< std::string >::type kmeans_method(kmeans_methodSEXP);
    Rcpp::traits::input_parameter< bool >::type AP_data(AP_dataSEXP);
    Rcpp::traits::input_parameter< bool >::type use_median(use_medianSEXP);
    Rcpp::traits::input_parameter< int >::type minib_kmeans_batch(minib_kmeans_batchSEXP);
    Rcpp::traits::input_parameter< double >::type minib_kmeans_init_fraction(minib_kmeans_init_fractionSEXP);
    Rcpp::traits::input_parameter< int >::type kmeans_num_init(kmeans_num_initSEXP);
    Rcpp::traits::input_parameter< int >::type kmeans_max_iters(kmeans_max_itersSEXP);
    Rcpp::traits::input_parameter< std::string >::type kmeans_initializer(kmeans_initializerSEXP);
    Rcpp::traits::input_parameter< std::string >::type colour_type(colour_typeSEXP);
    Rcpp::traits::input_parameter< double >::type compactness_factor(compactness_factorSEXP);
    Rcpp::traits::input_parameter< bool >::type adjust_centroids_and_return_masks(adjust_centroids_and_return_masksSEXP);
    Rcpp::traits::input_parameter< bool >::type sim_normalize(sim_normalizeSEXP);
    Rcpp::traits::input_parameter< int >::type sim_wL(sim_wLSEXP);
    Rcpp::traits::input_parameter< int >::type sim_wA(sim_wASEXP);
    Rcpp::traits::input_parameter< int >::type sim_wB(sim_wBSEXP);
    Rcpp::traits::input_parameter< int >::type sim_color_radius(sim_color_radiusSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(image_segmentation(input_image, method, num_superpixel, kmeans_method, AP_data, use_median, minib_kmeans_batch, minib_kmeans_init_fraction, kmeans_num_init, kmeans_max_iters, kmeans_initializer, colour_type, compactness_factor, adjust_centroids_and_return_masks, sim_normalize, sim_wL, sim_wA, sim_wB, sim_color_radius, verbose));
    return rcpp_result_gen;
END_RCPP
}