// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// FIM_2par_exp_censor1
Rcpp::NumericMatrix FIM_2par_exp_censor1(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param, const double tcensor);
RcppExport SEXP _ICAOD_FIM_2par_exp_censor1(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP, SEXP tcensorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    Rcpp::traits::input_parameter< const double >::type tcensor(tcensorSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_2par_exp_censor1(x, w, param, tcensor));
    return rcpp_result_gen;
END_RCPP
}
// FIM_2par_exp_censor2
Rcpp::NumericMatrix FIM_2par_exp_censor2(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param, const double tcensor);
RcppExport SEXP _ICAOD_FIM_2par_exp_censor2(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP, SEXP tcensorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    Rcpp::traits::input_parameter< const double >::type tcensor(tcensorSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_2par_exp_censor2(x, w, param, tcensor));
    return rcpp_result_gen;
END_RCPP
}
// FIM_3par_exp_censor1
Rcpp::NumericMatrix FIM_3par_exp_censor1(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param, const double tcensor);
RcppExport SEXP _ICAOD_FIM_3par_exp_censor1(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP, SEXP tcensorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    Rcpp::traits::input_parameter< const double >::type tcensor(tcensorSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_3par_exp_censor1(x, w, param, tcensor));
    return rcpp_result_gen;
END_RCPP
}
// FIM_3par_exp_censor2
Rcpp::NumericMatrix FIM_3par_exp_censor2(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param, const double tcensor);
RcppExport SEXP _ICAOD_FIM_3par_exp_censor2(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP, SEXP tcensorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    Rcpp::traits::input_parameter< const double >::type tcensor(tcensorSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_3par_exp_censor2(x, w, param, tcensor));
    return rcpp_result_gen;
END_RCPP
}
// FIM_exp_2par
Rcpp::NumericMatrix FIM_exp_2par(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_exp_2par(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_exp_2par(x, w, param));
    return rcpp_result_gen;
END_RCPP
}
// FIM_kinetics_alcohol
Eigen::MatrixXd FIM_kinetics_alcohol(const std::vector<double> x1, const std::vector<double> x2, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_kinetics_alcohol(SEXP x1SEXP, SEXP x2SEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_kinetics_alcohol(x1, x2, w, param));
    return rcpp_result_gen;
END_RCPP
}
// FIM_logistic
Rcpp::NumericMatrix FIM_logistic(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_logistic(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_logistic(x, w, param));
    return rcpp_result_gen;
END_RCPP
}
// FIM_logistic_2pred
Rcpp::NumericMatrix FIM_logistic_2pred(const std::vector<double> x1, const std::vector<double> x2, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_logistic_2pred(SEXP x1SEXP, SEXP x2SEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_logistic_2pred(x1, x2, w, param));
    return rcpp_result_gen;
END_RCPP
}
// FIM_logistic_4par
Eigen::MatrixXd FIM_logistic_4par(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_logistic_4par(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_logistic_4par(x, w, param));
    return rcpp_result_gen;
END_RCPP
}
// FIM_loglin
Eigen::MatrixXd FIM_loglin(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_loglin(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_loglin(x, w, param));
    return rcpp_result_gen;
END_RCPP
}
// FIM_mixed_inhibition
Eigen::MatrixXd FIM_mixed_inhibition(const std::vector<double> S, const std::vector<double> I, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_mixed_inhibition(SEXP SSEXP, SEXP ISEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type S(SSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type I(ISEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_mixed_inhibition(S, I, w, param));
    return rcpp_result_gen;
END_RCPP
}
// FIM_power_logistic
Rcpp::NumericMatrix FIM_power_logistic(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param, const double s);
RcppExport SEXP _ICAOD_FIM_power_logistic(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    Rcpp::traits::input_parameter< const double >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_power_logistic(x, w, param, s));
    return rcpp_result_gen;
END_RCPP
}
// FIM_sig_emax
Eigen::MatrixXd FIM_sig_emax(const std::vector<double> x, const std::vector<double> w, const std::vector<double> param);
RcppExport SEXP _ICAOD_FIM_sig_emax(SEXP xSEXP, SEXP wSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type w(wSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(FIM_sig_emax(x, w, param));
    return rcpp_result_gen;
END_RCPP
}
// det2
double det2(const Eigen::Map<Eigen::MatrixXd> mat, const bool logarithm);
RcppExport SEXP _ICAOD_det2(SEXP matSEXP, SEXP logarithmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type mat(matSEXP);
    Rcpp::traits::input_parameter< const bool >::type logarithm(logarithmSEXP);
    rcpp_result_gen = Rcpp::wrap(det2(mat, logarithm));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ICAOD_FIM_2par_exp_censor1", (DL_FUNC) &_ICAOD_FIM_2par_exp_censor1, 4},
    {"_ICAOD_FIM_2par_exp_censor2", (DL_FUNC) &_ICAOD_FIM_2par_exp_censor2, 4},
    {"_ICAOD_FIM_3par_exp_censor1", (DL_FUNC) &_ICAOD_FIM_3par_exp_censor1, 4},
    {"_ICAOD_FIM_3par_exp_censor2", (DL_FUNC) &_ICAOD_FIM_3par_exp_censor2, 4},
    {"_ICAOD_FIM_exp_2par", (DL_FUNC) &_ICAOD_FIM_exp_2par, 3},
    {"_ICAOD_FIM_kinetics_alcohol", (DL_FUNC) &_ICAOD_FIM_kinetics_alcohol, 4},
    {"_ICAOD_FIM_logistic", (DL_FUNC) &_ICAOD_FIM_logistic, 3},
    {"_ICAOD_FIM_logistic_2pred", (DL_FUNC) &_ICAOD_FIM_logistic_2pred, 4},
    {"_ICAOD_FIM_logistic_4par", (DL_FUNC) &_ICAOD_FIM_logistic_4par, 3},
    {"_ICAOD_FIM_loglin", (DL_FUNC) &_ICAOD_FIM_loglin, 3},
    {"_ICAOD_FIM_mixed_inhibition", (DL_FUNC) &_ICAOD_FIM_mixed_inhibition, 4},
    {"_ICAOD_FIM_power_logistic", (DL_FUNC) &_ICAOD_FIM_power_logistic, 4},
    {"_ICAOD_FIM_sig_emax", (DL_FUNC) &_ICAOD_FIM_sig_emax, 3},
    {"_ICAOD_det2", (DL_FUNC) &_ICAOD_det2, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_ICAOD(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}