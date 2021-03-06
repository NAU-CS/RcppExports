// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// native_cpp_set_dev_mode_
int native_cpp_set_dev_mode_(bool dev_mode);
RcppExport SEXP _ncpen_native_cpp_set_dev_mode_(SEXP dev_modeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type dev_mode(dev_modeSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_set_dev_mode_(dev_mode));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_nr_fun_
arma::vec native_cpp_nr_fun_(std::string fam, arma::vec& y_vec, arma::mat& x_mat, double iter_max, double b_eps);
RcppExport SEXP _ncpen_native_cpp_nr_fun_(SEXP famSEXP, SEXP y_vecSEXP, SEXP x_matSEXP, SEXP iter_maxSEXP, SEXP b_epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type fam(famSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type y_vec(y_vecSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type x_mat(x_matSEXP);
    Rcpp::traits::input_parameter< double >::type iter_max(iter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type b_eps(b_epsSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_nr_fun_(fam, y_vec, x_mat, iter_max, b_eps));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_pen_fun_
arma::vec native_cpp_pen_fun_(std::string name, arma::vec& b_vec, double lam, double gam, double tau);
RcppExport SEXP _ncpen_native_cpp_pen_fun_(SEXP nameSEXP, SEXP b_vecSEXP, SEXP lamSEXP, SEXP gamSEXP, SEXP tauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type b_vec(b_vecSEXP);
    Rcpp::traits::input_parameter< double >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< double >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_pen_fun_(name, b_vec, lam, gam, tau));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_pen_grad_fun_
arma::vec native_cpp_pen_grad_fun_(std::string name, arma::vec& b_vec, double lam, double gam, double tau);
RcppExport SEXP _ncpen_native_cpp_pen_grad_fun_(SEXP nameSEXP, SEXP b_vecSEXP, SEXP lamSEXP, SEXP gamSEXP, SEXP tauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type b_vec(b_vecSEXP);
    Rcpp::traits::input_parameter< double >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< double >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_pen_grad_fun_(name, b_vec, lam, gam, tau));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_obj_fun_
double native_cpp_obj_fun_(std::string name, arma::vec& y_vec, arma::mat& x_mat, arma::vec& b_vec);
RcppExport SEXP _ncpen_native_cpp_obj_fun_(SEXP nameSEXP, SEXP y_vecSEXP, SEXP x_matSEXP, SEXP b_vecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type y_vec(y_vecSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type x_mat(x_matSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type b_vec(b_vecSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_obj_fun_(name, y_vec, x_mat, b_vec));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_obj_grad_fun_
arma::vec native_cpp_obj_grad_fun_(std::string name, arma::vec& y_vec, arma::mat& x_mat, arma::vec& b_vec);
RcppExport SEXP _ncpen_native_cpp_obj_grad_fun_(SEXP nameSEXP, SEXP y_vecSEXP, SEXP x_matSEXP, SEXP b_vecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type y_vec(y_vecSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type x_mat(x_matSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type b_vec(b_vecSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_obj_grad_fun_(name, y_vec, x_mat, b_vec));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_obj_hess_fun_
arma::mat native_cpp_obj_hess_fun_(std::string name, arma::vec& y_vec, arma::mat& x_mat, arma::vec& b_vec);
RcppExport SEXP _ncpen_native_cpp_obj_hess_fun_(SEXP nameSEXP, SEXP y_vecSEXP, SEXP x_matSEXP, SEXP b_vecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type y_vec(y_vecSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type x_mat(x_matSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type b_vec(b_vecSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_obj_hess_fun_(name, y_vec, x_mat, b_vec));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_qlasso_fun_
Rcpp::List native_cpp_qlasso_fun_(arma::mat& q_mat, arma::vec& l_vec, arma::vec& b_vec0, arma::vec& w_vec, double lam, double iter_max, double iiter_max, double b_eps, double k_eps, arma::uword p_eff, arma::uword q_rank, bool cut, double c_eps);
RcppExport SEXP _ncpen_native_cpp_qlasso_fun_(SEXP q_matSEXP, SEXP l_vecSEXP, SEXP b_vec0SEXP, SEXP w_vecSEXP, SEXP lamSEXP, SEXP iter_maxSEXP, SEXP iiter_maxSEXP, SEXP b_epsSEXP, SEXP k_epsSEXP, SEXP p_effSEXP, SEXP q_rankSEXP, SEXP cutSEXP, SEXP c_epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type q_mat(q_matSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type l_vec(l_vecSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type b_vec0(b_vec0SEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type w_vec(w_vecSEXP);
    Rcpp::traits::input_parameter< double >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< double >::type iter_max(iter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type iiter_max(iiter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type b_eps(b_epsSEXP);
    Rcpp::traits::input_parameter< double >::type k_eps(k_epsSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type p_eff(p_effSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type q_rank(q_rankSEXP);
    Rcpp::traits::input_parameter< bool >::type cut(cutSEXP);
    Rcpp::traits::input_parameter< double >::type c_eps(c_epsSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_qlasso_fun_(q_mat, l_vec, b_vec0, w_vec, lam, iter_max, iiter_max, b_eps, k_eps, p_eff, q_rank, cut, c_eps));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_p_ncpen_fun_
Rcpp::List native_cpp_p_ncpen_fun_(arma::vec& y_vec, arma::mat& x_mat, arma::vec& b_vec, arma::vec& w_vec, double lam, double gam, double tau, double alp, double iter_max, double qiter_max, double qiiter_max, double b_eps, double k_eps, arma::uword p_eff, bool cut, double c_eps, SEXP family, SEXP penalty);
RcppExport SEXP _ncpen_native_cpp_p_ncpen_fun_(SEXP y_vecSEXP, SEXP x_matSEXP, SEXP b_vecSEXP, SEXP w_vecSEXP, SEXP lamSEXP, SEXP gamSEXP, SEXP tauSEXP, SEXP alpSEXP, SEXP iter_maxSEXP, SEXP qiter_maxSEXP, SEXP qiiter_maxSEXP, SEXP b_epsSEXP, SEXP k_epsSEXP, SEXP p_effSEXP, SEXP cutSEXP, SEXP c_epsSEXP, SEXP familySEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type y_vec(y_vecSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type x_mat(x_matSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type b_vec(b_vecSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type w_vec(w_vecSEXP);
    Rcpp::traits::input_parameter< double >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< double >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< double >::type alp(alpSEXP);
    Rcpp::traits::input_parameter< double >::type iter_max(iter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type qiter_max(qiter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type qiiter_max(qiiter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type b_eps(b_epsSEXP);
    Rcpp::traits::input_parameter< double >::type k_eps(k_epsSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type p_eff(p_effSEXP);
    Rcpp::traits::input_parameter< bool >::type cut(cutSEXP);
    Rcpp::traits::input_parameter< double >::type c_eps(c_epsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type family(familySEXP);
    Rcpp::traits::input_parameter< SEXP >::type penalty(penaltySEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_p_ncpen_fun_(y_vec, x_mat, b_vec, w_vec, lam, gam, tau, alp, iter_max, qiter_max, qiiter_max, b_eps, k_eps, p_eff, cut, c_eps, family, penalty));
    return rcpp_result_gen;
END_RCPP
}
// native_cpp_ncpen_fun_
Rcpp::List native_cpp_ncpen_fun_(arma::vec& y_vec, arma::mat& x_mat0, arma::vec& w_vec0, arma::vec& lam_vec0, double gam, double tau, double alp, arma::uword d_max, double iter_max, double qiter_max, double qiiter_max, double b_eps, double k_eps, arma::uword p_eff, bool cut, double c_eps, arma::uword add, SEXP family, SEXP penalty, bool loc, arma::vec& ob_vec, int div);
RcppExport SEXP _ncpen_native_cpp_ncpen_fun_(SEXP y_vecSEXP, SEXP x_mat0SEXP, SEXP w_vec0SEXP, SEXP lam_vec0SEXP, SEXP gamSEXP, SEXP tauSEXP, SEXP alpSEXP, SEXP d_maxSEXP, SEXP iter_maxSEXP, SEXP qiter_maxSEXP, SEXP qiiter_maxSEXP, SEXP b_epsSEXP, SEXP k_epsSEXP, SEXP p_effSEXP, SEXP cutSEXP, SEXP c_epsSEXP, SEXP addSEXP, SEXP familySEXP, SEXP penaltySEXP, SEXP locSEXP, SEXP ob_vecSEXP, SEXP divSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type y_vec(y_vecSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type x_mat0(x_mat0SEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type w_vec0(w_vec0SEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type lam_vec0(lam_vec0SEXP);
    Rcpp::traits::input_parameter< double >::type gam(gamSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< double >::type alp(alpSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type d_max(d_maxSEXP);
    Rcpp::traits::input_parameter< double >::type iter_max(iter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type qiter_max(qiter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type qiiter_max(qiiter_maxSEXP);
    Rcpp::traits::input_parameter< double >::type b_eps(b_epsSEXP);
    Rcpp::traits::input_parameter< double >::type k_eps(k_epsSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type p_eff(p_effSEXP);
    Rcpp::traits::input_parameter< bool >::type cut(cutSEXP);
    Rcpp::traits::input_parameter< double >::type c_eps(c_epsSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type add(addSEXP);
    Rcpp::traits::input_parameter< SEXP >::type family(familySEXP);
    Rcpp::traits::input_parameter< SEXP >::type penalty(penaltySEXP);
    Rcpp::traits::input_parameter< bool >::type loc(locSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type ob_vec(ob_vecSEXP);
    Rcpp::traits::input_parameter< int >::type div(divSEXP);
    rcpp_result_gen = Rcpp::wrap(native_cpp_ncpen_fun_(y_vec, x_mat0, w_vec0, lam_vec0, gam, tau, alp, d_max, iter_max, qiter_max, qiiter_max, b_eps, k_eps, p_eff, cut, c_eps, add, family, penalty, loc, ob_vec, div));
    return rcpp_result_gen;
END_RCPP
}
