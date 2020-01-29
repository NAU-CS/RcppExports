// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rcppClusteringEMGlasso
IntegerVector rcppClusteringEMGlasso(List InputList, double l, double r);
RcppExport SEXP _SelvarMix_rcppClusteringEMGlasso(SEXP InputListSEXP, SEXP lSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type InputList(InputListSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppClusteringEMGlasso(InputList, l, r));
    return rcpp_result_gen;
END_RCPP
}
// rcppDiscriminantAnalysisGlasso
IntegerVector rcppDiscriminantAnalysisGlasso(NumericMatrix X_, IntegerVector labels_, const int nbClust, double l, double r);
RcppExport SEXP _SelvarMix_rcppDiscriminantAnalysisGlasso(SEXP X_SEXP, SEXP labels_SEXP, SEXP nbClustSEXP, SEXP lSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X_(X_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type labels_(labels_SEXP);
    Rcpp::traits::input_parameter< const int >::type nbClust(nbClustSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppDiscriminantAnalysisGlasso(X_, labels_, nbClust, l, r));
    return rcpp_result_gen;
END_RCPP
}
// rcppSelectS
List rcppSelectS(NumericMatrix X, std::vector<int> Order, const int nbCluster, S4 CovForm, const int packSize, std::string Crit, IntegerVector knownlabels, IntegerVector DA);
RcppExport SEXP _SelvarMix_rcppSelectS(SEXP XSEXP, SEXP OrderSEXP, SEXP nbClusterSEXP, SEXP CovFormSEXP, SEXP packSizeSEXP, SEXP CritSEXP, SEXP knownlabelsSEXP, SEXP DASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type Order(OrderSEXP);
    Rcpp::traits::input_parameter< const int >::type nbCluster(nbClusterSEXP);
    Rcpp::traits::input_parameter< S4 >::type CovForm(CovFormSEXP);
    Rcpp::traits::input_parameter< const int >::type packSize(packSizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type Crit(CritSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type knownlabels(knownlabelsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type DA(DASEXP);
    rcpp_result_gen = Rcpp::wrap(rcppSelectS(X, Order, nbCluster, CovForm, packSize, Crit, knownlabels, DA));
    return rcpp_result_gen;
END_RCPP
}
// rcppSelectW
IntegerVector rcppSelectW(NumericMatrix X, std::vector<int> Order, std::vector<int> OtherVar, const int packSize);
RcppExport SEXP _SelvarMix_rcppSelectW(SEXP XSEXP, SEXP OrderSEXP, SEXP OtherVarSEXP, SEXP packSizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type Order(OrderSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type OtherVar(OtherVarSEXP);
    Rcpp::traits::input_parameter< const int >::type packSize(packSizeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppSelectW(X, Order, OtherVar, packSize));
    return rcpp_result_gen;
END_RCPP
}
// rcppSelectR
IntegerVector rcppSelectR(NumericMatrix X, std::vector<int> S, std::vector<int> U, std::string regmodel);
RcppExport SEXP _SelvarMix_rcppSelectR(SEXP XSEXP, SEXP SSEXP, SEXP USEXP, SEXP regmodelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type S(SSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type U(USEXP);
    Rcpp::traits::input_parameter< std::string >::type regmodel(regmodelSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppSelectR(X, S, U, regmodel));
    return rcpp_result_gen;
END_RCPP
}
// rcppCrit
List rcppCrit(NumericMatrix X, List MyList, std::vector<std::string> rgm, std::vector<std::string> idm);
RcppExport SEXP _SelvarMix_rcppCrit(SEXP XSEXP, SEXP MyListSEXP, SEXP rgmSEXP, SEXP idmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< List >::type MyList(MyListSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type rgm(rgmSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type idm(idmSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppCrit(X, MyList, rgm, idm));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SelvarMix_rcppClusteringEMGlasso", (DL_FUNC) &_SelvarMix_rcppClusteringEMGlasso, 3},
    {"_SelvarMix_rcppDiscriminantAnalysisGlasso", (DL_FUNC) &_SelvarMix_rcppDiscriminantAnalysisGlasso, 5},
    {"_SelvarMix_rcppSelectS", (DL_FUNC) &_SelvarMix_rcppSelectS, 8},
    {"_SelvarMix_rcppSelectW", (DL_FUNC) &_SelvarMix_rcppSelectW, 4},
    {"_SelvarMix_rcppSelectR", (DL_FUNC) &_SelvarMix_rcppSelectR, 4},
    {"_SelvarMix_rcppCrit", (DL_FUNC) &_SelvarMix_rcppCrit, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_SelvarMix(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
