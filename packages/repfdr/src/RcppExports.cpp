// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_main
List rcpp_main(NumericVector Sizes, List pdf_binned_z_list_index0, List pdf_binned_z_list_index1, List pdf_binned_z_list_index2, List binned_z_mat_list, List cluster_ind_list, List repfdr_Pi_list);
RcppExport SEXP _repfdr_rcpp_main(SEXP SizesSEXP, SEXP pdf_binned_z_list_index0SEXP, SEXP pdf_binned_z_list_index1SEXP, SEXP pdf_binned_z_list_index2SEXP, SEXP binned_z_mat_listSEXP, SEXP cluster_ind_listSEXP, SEXP repfdr_Pi_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Sizes(SizesSEXP);
    Rcpp::traits::input_parameter< List >::type pdf_binned_z_list_index0(pdf_binned_z_list_index0SEXP);
    Rcpp::traits::input_parameter< List >::type pdf_binned_z_list_index1(pdf_binned_z_list_index1SEXP);
    Rcpp::traits::input_parameter< List >::type pdf_binned_z_list_index2(pdf_binned_z_list_index2SEXP);
    Rcpp::traits::input_parameter< List >::type binned_z_mat_list(binned_z_mat_listSEXP);
    Rcpp::traits::input_parameter< List >::type cluster_ind_list(cluster_ind_listSEXP);
    Rcpp::traits::input_parameter< List >::type repfdr_Pi_list(repfdr_Pi_listSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_main(Sizes, pdf_binned_z_list_index0, pdf_binned_z_list_index1, pdf_binned_z_list_index2, binned_z_mat_list, cluster_ind_list, repfdr_Pi_list));
    return rcpp_result_gen;
END_RCPP
}
