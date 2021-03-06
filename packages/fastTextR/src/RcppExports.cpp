// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Rft_dict_get_nwords
int Rft_dict_get_nwords(SEXP r_dict);
RcppExport SEXP fastTextR_Rft_dict_get_nwords(SEXP r_dictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_nwords(r_dict));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_nlabels
int Rft_dict_get_nlabels(SEXP r_dict);
RcppExport SEXP fastTextR_Rft_dict_get_nlabels(SEXP r_dictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_nlabels(r_dict));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_ntokens
int Rft_dict_get_ntokens(SEXP r_dict);
RcppExport SEXP fastTextR_Rft_dict_get_ntokens(SEXP r_dictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_ntokens(r_dict));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_word
std::string Rft_dict_get_word(SEXP r_dict, int i);
RcppExport SEXP fastTextR_Rft_dict_get_word(SEXP r_dictSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_word(r_dict, i));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_words
std::vector<std::string> Rft_dict_get_words(SEXP r_dict, std::vector<int> ind);
RcppExport SEXP fastTextR_Rft_dict_get_words(SEXP r_dictSEXP, SEXP indSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ind(indSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_words(r_dict, ind));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_all_words
std::vector<std::string> Rft_dict_get_all_words(SEXP r_dict);
RcppExport SEXP fastTextR_Rft_dict_get_all_words(SEXP r_dictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_all_words(r_dict));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_label
std::string Rft_dict_get_label(SEXP r_dict, int i);
RcppExport SEXP fastTextR_Rft_dict_get_label(SEXP r_dictSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_label(r_dict, i));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_labels
std::vector<std::string> Rft_dict_get_labels(SEXP r_dict, std::vector<int> ind);
RcppExport SEXP fastTextR_Rft_dict_get_labels(SEXP r_dictSEXP, SEXP indSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ind(indSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_labels(r_dict, ind));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_get_all_labels
std::vector<std::string> Rft_dict_get_all_labels(SEXP r_dict);
RcppExport SEXP fastTextR_Rft_dict_get_all_labels(SEXP r_dictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_get_all_labels(r_dict));
    return rcpp_result_gen;
END_RCPP
}
// Rft_dict_read_from_file
SEXP Rft_dict_read_from_file(SEXP r_dict, std::string file_name);
RcppExport SEXP fastTextR_Rft_dict_read_from_file(SEXP r_dictSEXP, SEXP file_nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_dict(r_dictSEXP);
    Rcpp::traits::input_parameter< std::string >::type file_name(file_nameSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_dict_read_from_file(r_dict, file_name));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_dim_input_matrix
SEXP Rft_ft_dim_input_matrix(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_dim_input_matrix(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_dim_input_matrix(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_dim_output_matrix
SEXP Rft_ft_dim_output_matrix(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_dim_output_matrix(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_dim_output_matrix(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_get_args
SEXP Rft_ft_get_args(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_get_args(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_get_args(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_get_dict
SEXP Rft_ft_get_dict(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_get_dict(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_get_dict(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_get_input_matrix
SEXP Rft_ft_get_input_matrix(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_get_input_matrix(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_get_input_matrix(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_get_output_matrix
NumericMatrix Rft_ft_get_output_matrix(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_get_output_matrix(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_get_output_matrix(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_get_model
SEXP Rft_ft_get_model(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_get_model(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_get_model(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_get_token_count
double Rft_ft_get_token_count(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_get_token_count(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_get_token_count(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_ft_get_model_type
std::string Rft_ft_get_model_type(SEXP r_ft);
RcppExport SEXP fastTextR_Rft_ft_get_model_type(SEXP r_ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type r_ft(r_ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_ft_get_model_type(r_ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_new_model
SEXP Rft_new_model();
RcppExport SEXP fastTextR_Rft_new_model() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(Rft_new_model());
    return rcpp_result_gen;
END_RCPP
}
// Rft_new_args
SEXP Rft_new_args();
RcppExport SEXP fastTextR_Rft_new_args() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(Rft_new_args());
    return rcpp_result_gen;
END_RCPP
}
// Rft_new_dict
SEXP Rft_new_dict(SEXP control);
RcppExport SEXP fastTextR_Rft_new_dict(SEXP controlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type control(controlSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_new_dict(control));
    return rcpp_result_gen;
END_RCPP
}
// Rft_load_model
SEXP Rft_load_model(std::string file_name);
RcppExport SEXP fastTextR_Rft_load_model(SEXP file_nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type file_name(file_nameSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_load_model(file_name));
    return rcpp_result_gen;
END_RCPP
}
// Rft_save_model
SEXP Rft_save_model(SEXP ft, std::string file_name);
RcppExport SEXP fastTextR_Rft_save_model(SEXP ftSEXP, SEXP file_nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::string >::type file_name(file_nameSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_save_model(ft, file_name));
    return rcpp_result_gen;
END_RCPP
}
// Rft_args_to_list
List Rft_args_to_list(SEXP args);
RcppExport SEXP fastTextR_Rft_args_to_list(SEXP argsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type args(argsSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_args_to_list(args));
    return rcpp_result_gen;
END_RCPP
}
// Rft_train
SEXP Rft_train(SEXP control, int save_to_file);
RcppExport SEXP fastTextR_Rft_train(SEXP controlSEXP, SEXP save_to_fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type control(controlSEXP);
    Rcpp::traits::input_parameter< int >::type save_to_file(save_to_fileSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_train(control, save_to_file));
    return rcpp_result_gen;
END_RCPP
}
// Rft_test
std::vector<double> Rft_test(SEXP ft, std::string test_file, int k_most_likely);
RcppExport SEXP fastTextR_Rft_test(SEXP ftSEXP, SEXP test_fileSEXP, SEXP k_most_likelySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::string >::type test_file(test_fileSEXP);
    Rcpp::traits::input_parameter< int >::type k_most_likely(k_most_likelySEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_test(ft, test_file, k_most_likely));
    return rcpp_result_gen;
END_RCPP
}
// Rft_predict_to_file
SEXP Rft_predict_to_file(SEXP ft, std::string newdata_file, std::string result_file, int k_most_likely, bool print_prob);
RcppExport SEXP fastTextR_Rft_predict_to_file(SEXP ftSEXP, SEXP newdata_fileSEXP, SEXP result_fileSEXP, SEXP k_most_likelySEXP, SEXP print_probSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::string >::type newdata_file(newdata_fileSEXP);
    Rcpp::traits::input_parameter< std::string >::type result_file(result_fileSEXP);
    Rcpp::traits::input_parameter< int >::type k_most_likely(k_most_likelySEXP);
    Rcpp::traits::input_parameter< bool >::type print_prob(print_probSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_predict_to_file(ft, newdata_file, result_file, k_most_likely, print_prob));
    return rcpp_result_gen;
END_RCPP
}
// Rft_predict
SEXP Rft_predict(SEXP ft, std::string newdata_file, int k_most_likely, bool print_prob);
RcppExport SEXP fastTextR_Rft_predict(SEXP ftSEXP, SEXP newdata_fileSEXP, SEXP k_most_likelySEXP, SEXP print_probSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::string >::type newdata_file(newdata_fileSEXP);
    Rcpp::traits::input_parameter< int >::type k_most_likely(k_most_likelySEXP);
    Rcpp::traits::input_parameter< bool >::type print_prob(print_probSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_predict(ft, newdata_file, k_most_likely, print_prob));
    return rcpp_result_gen;
END_RCPP
}
// Rft_vec_predict
SEXP Rft_vec_predict(SEXP ft, std::vector<std::string> newdata, int k_most_likely, bool print_prob);
RcppExport SEXP fastTextR_Rft_vec_predict(SEXP ftSEXP, SEXP newdataSEXP, SEXP k_most_likelySEXP, SEXP print_probSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type newdata(newdataSEXP);
    Rcpp::traits::input_parameter< int >::type k_most_likely(k_most_likelySEXP);
    Rcpp::traits::input_parameter< bool >::type print_prob(print_probSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_vec_predict(ft, newdata, k_most_likely, print_prob));
    return rcpp_result_gen;
END_RCPP
}
// Rft_get_word_vectors
std::vector<double> Rft_get_word_vectors(SEXP ft, std::vector<std::string> words);
RcppExport SEXP fastTextR_Rft_get_word_vectors(SEXP ftSEXP, SEXP wordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type words(wordsSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_get_word_vectors(ft, words));
    return rcpp_result_gen;
END_RCPP
}
// Rft_get_all_word_vectors
SEXP Rft_get_all_word_vectors(SEXP ft);
RcppExport SEXP fastTextR_Rft_get_all_word_vectors(SEXP ftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_get_all_word_vectors(ft));
    return rcpp_result_gen;
END_RCPP
}
// Rft_similarity
double Rft_similarity(SEXP ft, std::string word_a, std::string word_b);
RcppExport SEXP fastTextR_Rft_similarity(SEXP ftSEXP, SEXP word_aSEXP, SEXP word_bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::string >::type word_a(word_aSEXP);
    Rcpp::traits::input_parameter< std::string >::type word_b(word_bSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_similarity(ft, word_a, word_b));
    return rcpp_result_gen;
END_RCPP
}
// Rft_k_most_silmilar
SEXP Rft_k_most_silmilar(SEXP ft, std::vector<std::string> words, int k);
RcppExport SEXP fastTextR_Rft_k_most_silmilar(SEXP ftSEXP, SEXP wordsSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ft(ftSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type words(wordsSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(Rft_k_most_silmilar(ft, words, k));
    return rcpp_result_gen;
END_RCPP
}
// clean_text
std::vector<std::string> clean_text(std::vector<std::string> x);
RcppExport SEXP fastTextR_clean_text(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(clean_text(x));
    return rcpp_result_gen;
END_RCPP
}
