// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/porridge.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// ridgePgen
arma::mat ridgePgen(const arma::mat& S, const arma::mat& lambda, const arma::mat& target, const int& nInit, const double& minSuccDiff);
static SEXP _porridge_ridgePgen_try(SEXP SSEXP, SEXP lambdaSEXP, SEXP targetSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type S(SSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type target(targetSEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    rcpp_result_gen = Rcpp::wrap(ridgePgen(S, lambda, target, nInit, minSuccDiff));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_ridgePgen(SEXP SSEXP, SEXP lambdaSEXP, SEXP targetSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_ridgePgen_try(SSEXP, lambdaSEXP, targetSEXP, nInitSEXP, minSuccDiffSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// kcvlossR
double kcvlossR(arma::mat& lambda, arma::mat& Y, arma::mat& target, Rcpp::List& folds, const int& nInit, const double& minSuccDiff);
static SEXP _porridge_kcvlossR_try(SEXP lambdaSEXP, SEXP YSEXP, SEXP targetSEXP, SEXP foldsSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type target(targetSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type folds(foldsSEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    rcpp_result_gen = Rcpp::wrap(kcvlossR(lambda, Y, target, folds, nInit, minSuccDiff));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_kcvlossR(SEXP lambdaSEXP, SEXP YSEXP, SEXP targetSEXP, SEXP foldsSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_kcvlossR_try(lambdaSEXP, YSEXP, targetSEXP, foldsSEXP, nInitSEXP, minSuccDiffSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// penaltyPgen_banded
arma::mat penaltyPgen_banded(double& lambda, int p, arma::uvec& zerosR, arma::uvec& zerosC, bool penalize_diag);
static SEXP _porridge_penaltyPgen_banded_try(SEXP lambdaSEXP, SEXP pSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosR(zerosRSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosC(zerosCSEXP);
    Rcpp::traits::input_parameter< bool >::type penalize_diag(penalize_diagSEXP);
    rcpp_result_gen = Rcpp::wrap(penaltyPgen_banded(lambda, p, zerosR, zerosC, penalize_diag));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_penaltyPgen_banded(SEXP lambdaSEXP, SEXP pSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_penaltyPgen_banded_try(lambdaSEXP, pSEXP, zerosRSEXP, zerosCSEXP, penalize_diagSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// penaltyPgen_groups
arma::mat penaltyPgen_groups(arma::vec& lambda, arma::vec& groups, arma::uvec& zerosR, arma::uvec& zerosC, bool penalize_diag);
static SEXP _porridge_penaltyPgen_groups_try(SEXP lambdaSEXP, SEXP groupsSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type groups(groupsSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosR(zerosRSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosC(zerosCSEXP);
    Rcpp::traits::input_parameter< bool >::type penalize_diag(penalize_diagSEXP);
    rcpp_result_gen = Rcpp::wrap(penaltyPgen_groups(lambda, groups, zerosR, zerosC, penalize_diag));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_penaltyPgen_groups(SEXP lambdaSEXP, SEXP groupsSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_penaltyPgen_groups_try(lambdaSEXP, groupsSEXP, zerosRSEXP, zerosCSEXP, penalize_diagSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// kcvlossR_groups
double kcvlossR_groups(arma::vec& lambdaGrps, arma::mat& Y, arma::mat& target, Rcpp::List& folds, arma::vec& groups, arma::uvec& zerosR, arma::uvec& zerosC, bool penalize_diag, const int& nInit, const double& minSuccDiff);
static SEXP _porridge_kcvlossR_groups_try(SEXP lambdaGrpsSEXP, SEXP YSEXP, SEXP targetSEXP, SEXP foldsSEXP, SEXP groupsSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type lambdaGrps(lambdaGrpsSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type target(targetSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type folds(foldsSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type groups(groupsSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosR(zerosRSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosC(zerosCSEXP);
    Rcpp::traits::input_parameter< bool >::type penalize_diag(penalize_diagSEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    rcpp_result_gen = Rcpp::wrap(kcvlossR_groups(lambdaGrps, Y, target, folds, groups, zerosR, zerosC, penalize_diag, nInit, minSuccDiff));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_kcvlossR_groups(SEXP lambdaGrpsSEXP, SEXP YSEXP, SEXP targetSEXP, SEXP foldsSEXP, SEXP groupsSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_kcvlossR_groups_try(lambdaGrpsSEXP, YSEXP, targetSEXP, foldsSEXP, groupsSEXP, zerosRSEXP, zerosCSEXP, penalize_diagSEXP, nInitSEXP, minSuccDiffSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// kcvlossR_banded
double kcvlossR_banded(double& lambda, arma::mat& Y, arma::mat& target, Rcpp::List& folds, arma::uvec& zerosR, arma::uvec& zerosC, bool penalize_diag, const int& nInit, const double& minSuccDiff);
static SEXP _porridge_kcvlossR_banded_try(SEXP lambdaSEXP, SEXP YSEXP, SEXP targetSEXP, SEXP foldsSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double& >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type target(targetSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type folds(foldsSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosR(zerosRSEXP);
    Rcpp::traits::input_parameter< arma::uvec& >::type zerosC(zerosCSEXP);
    Rcpp::traits::input_parameter< bool >::type penalize_diag(penalize_diagSEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    rcpp_result_gen = Rcpp::wrap(kcvlossR_banded(lambda, Y, target, folds, zerosR, zerosC, penalize_diag, nInit, minSuccDiff));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_kcvlossR_banded(SEXP lambdaSEXP, SEXP YSEXP, SEXP targetSEXP, SEXP foldsSEXP, SEXP zerosRSEXP, SEXP zerosCSEXP, SEXP penalize_diagSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_kcvlossR_banded_try(lambdaSEXP, YSEXP, targetSEXP, foldsSEXP, zerosRSEXP, zerosCSEXP, penalize_diagSEXP, nInitSEXP, minSuccDiffSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ridgeGGMmixture
Rcpp::List ridgeGGMmixture(const arma::mat& Y, const int K, const double lambda, const arma::mat& target, const arma::mat& iWeights, const int& nInit, const double& minSuccDiff, const double& minMixProp);
static SEXP _porridge_ridgeGGMmixture_try(SEXP YSEXP, SEXP KSEXP, SEXP lambdaSEXP, SEXP targetSEXP, SEXP iWeightsSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP minMixPropSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const int >::type K(KSEXP);
    Rcpp::traits::input_parameter< const double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type target(targetSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type iWeights(iWeightsSEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    Rcpp::traits::input_parameter< const double& >::type minMixProp(minMixPropSEXP);
    rcpp_result_gen = Rcpp::wrap(ridgeGGMmixture(Y, K, lambda, target, iWeights, nInit, minSuccDiff, minMixProp));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_ridgeGGMmixture(SEXP YSEXP, SEXP KSEXP, SEXP lambdaSEXP, SEXP targetSEXP, SEXP iWeightsSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP minMixPropSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_ridgeGGMmixture_try(YSEXP, KSEXP, lambdaSEXP, targetSEXP, iWeightsSEXP, nInitSEXP, minSuccDiffSEXP, minMixPropSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// kcvlGGMmixture
double kcvlGGMmixture(double lambda, const arma::mat& Y, const int K, const arma::mat& target, const arma::mat& iWeights, const int& nInit, const double& minSuccDiff, Rcpp::List folds, const double& minMixProp);
static SEXP _porridge_kcvlGGMmixture_try(SEXP lambdaSEXP, SEXP YSEXP, SEXP KSEXP, SEXP targetSEXP, SEXP iWeightsSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP foldsSEXP, SEXP minMixPropSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const int >::type K(KSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type target(targetSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type iWeights(iWeightsSEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type folds(foldsSEXP);
    Rcpp::traits::input_parameter< const double& >::type minMixProp(minMixPropSEXP);
    rcpp_result_gen = Rcpp::wrap(kcvlGGMmixture(lambda, Y, K, target, iWeights, nInit, minSuccDiff, folds, minMixProp));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_kcvlGGMmixture(SEXP lambdaSEXP, SEXP YSEXP, SEXP KSEXP, SEXP targetSEXP, SEXP iWeightsSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP foldsSEXP, SEXP minMixPropSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_kcvlGGMmixture_try(lambdaSEXP, YSEXP, KSEXP, targetSEXP, iWeightsSEXP, nInitSEXP, minSuccDiffSEXP, foldsSEXP, minMixPropSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ridgePrepEM
Rcpp::List ridgePrepEM(arma::mat Y, arma::ivec ids, const double lambdaZ, const double lambdaE, const arma::mat& targetZ, const arma::mat& targetE, const int& nInit, const double& minSuccDiff);
static SEXP _porridge_ridgePrepEM_try(SEXP YSEXP, SEXP idsSEXP, SEXP lambdaZSEXP, SEXP lambdaESEXP, SEXP targetZSEXP, SEXP targetESEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::ivec >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< const double >::type lambdaZ(lambdaZSEXP);
    Rcpp::traits::input_parameter< const double >::type lambdaE(lambdaESEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type targetZ(targetZSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type targetE(targetESEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    rcpp_result_gen = Rcpp::wrap(ridgePrepEM(Y, ids, lambdaZ, lambdaE, targetZ, targetE, nInit, minSuccDiff));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_ridgePrepEM(SEXP YSEXP, SEXP idsSEXP, SEXP lambdaZSEXP, SEXP lambdaESEXP, SEXP targetZSEXP, SEXP targetESEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_ridgePrepEM_try(YSEXP, idsSEXP, lambdaZSEXP, lambdaESEXP, targetZSEXP, targetESEXP, nInitSEXP, minSuccDiffSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ridgePrepKcvLL
double ridgePrepKcvLL(const arma::vec lambdaZE, const arma::mat& Y, const arma::ivec& ids, const arma::mat& targetZ, const arma::mat& targetE, const int& nInit, const double& minSuccDiff, Rcpp::List folds, std::string CVcrit);
static SEXP _porridge_ridgePrepKcvLL_try(SEXP lambdaZESEXP, SEXP YSEXP, SEXP idsSEXP, SEXP targetZSEXP, SEXP targetESEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP foldsSEXP, SEXP CVcritSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec >::type lambdaZE(lambdaZESEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::ivec& >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type targetZ(targetZSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type targetE(targetESEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type folds(foldsSEXP);
    Rcpp::traits::input_parameter< std::string >::type CVcrit(CVcritSEXP);
    rcpp_result_gen = Rcpp::wrap(ridgePrepKcvLL(lambdaZE, Y, ids, targetZ, targetE, nInit, minSuccDiff, folds, CVcrit));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_ridgePrepKcvLL(SEXP lambdaZESEXP, SEXP YSEXP, SEXP idsSEXP, SEXP targetZSEXP, SEXP targetESEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP foldsSEXP, SEXP CVcritSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_ridgePrepKcvLL_try(lambdaZESEXP, YSEXP, idsSEXP, targetZSEXP, targetESEXP, nInitSEXP, minSuccDiffSEXP, foldsSEXP, CVcritSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ridgePrepEMdiag
Rcpp::List ridgePrepEMdiag(arma::mat Y, arma::ivec ids, const double lambdaZ, const arma::mat& targetZ, const unsigned int& nInit, const double& minSuccDiff);
static SEXP _porridge_ridgePrepEMdiag_try(SEXP YSEXP, SEXP idsSEXP, SEXP lambdaZSEXP, SEXP targetZSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::ivec >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< const double >::type lambdaZ(lambdaZSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type targetZ(targetZSEXP);
    Rcpp::traits::input_parameter< const unsigned int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    rcpp_result_gen = Rcpp::wrap(ridgePrepEMdiag(Y, ids, lambdaZ, targetZ, nInit, minSuccDiff));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_ridgePrepEMdiag(SEXP YSEXP, SEXP idsSEXP, SEXP lambdaZSEXP, SEXP targetZSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_ridgePrepEMdiag_try(YSEXP, idsSEXP, lambdaZSEXP, targetZSEXP, nInitSEXP, minSuccDiffSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ridgePrepKcvLLdiag
double ridgePrepKcvLLdiag(const double lambdaZ, const arma::mat& Y, const arma::ivec& ids, const arma::mat& targetZ, const int& nInit, const double& minSuccDiff, Rcpp::List folds, std::string CVcrit);
static SEXP _porridge_ridgePrepKcvLLdiag_try(SEXP lambdaZSEXP, SEXP YSEXP, SEXP idsSEXP, SEXP targetZSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP foldsSEXP, SEXP CVcritSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double >::type lambdaZ(lambdaZSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::ivec& >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type targetZ(targetZSEXP);
    Rcpp::traits::input_parameter< const int& >::type nInit(nInitSEXP);
    Rcpp::traits::input_parameter< const double& >::type minSuccDiff(minSuccDiffSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type folds(foldsSEXP);
    Rcpp::traits::input_parameter< std::string >::type CVcrit(CVcritSEXP);
    rcpp_result_gen = Rcpp::wrap(ridgePrepKcvLLdiag(lambdaZ, Y, ids, targetZ, nInit, minSuccDiff, folds, CVcrit));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _porridge_ridgePrepKcvLLdiag(SEXP lambdaZSEXP, SEXP YSEXP, SEXP idsSEXP, SEXP targetZSEXP, SEXP nInitSEXP, SEXP minSuccDiffSEXP, SEXP foldsSEXP, SEXP CVcritSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_porridge_ridgePrepKcvLLdiag_try(lambdaZSEXP, YSEXP, idsSEXP, targetZSEXP, nInitSEXP, minSuccDiffSEXP, foldsSEXP, CVcritSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _porridge_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("arma::mat(*.armaRidgePgen)(const arma::mat&,const arma::mat&,const arma::mat&,const int&,const double&)");
        signatures.insert("double(*.armaKcvlossR)(arma::mat&,arma::mat&,arma::mat&,Rcpp::List&,const int&,const double&)");
        signatures.insert("arma::mat(*.armaPenaltyPgen_banded)(double&,int,arma::uvec&,arma::uvec&,bool)");
        signatures.insert("arma::mat(*.armaPenaltyPgen_groups)(arma::vec&,arma::vec&,arma::uvec&,arma::uvec&,bool)");
        signatures.insert("double(*.armaKCVlossR_groups)(arma::vec&,arma::mat&,arma::mat&,Rcpp::List&,arma::vec&,arma::uvec&,arma::uvec&,bool,const int&,const double&)");
        signatures.insert("double(*.armaKCVlossR_banded)(double&,arma::mat&,arma::mat&,Rcpp::List&,arma::uvec&,arma::uvec&,bool,const int&,const double&)");
        signatures.insert("Rcpp::List(*.armaRidgeGGMmixture)(const arma::mat&,const int,const double,const arma::mat&,const arma::mat&,const int&,const double&,const double&)");
        signatures.insert("double(*.armaKcvlGGMmixture)(double,const arma::mat&,const int,const arma::mat&,const arma::mat&,const int&,const double&,Rcpp::List,const double&)");
        signatures.insert("Rcpp::List(*.armaRidgePrepEM)(arma::mat,arma::ivec,const double,const double,const arma::mat&,const arma::mat&,const int&,const double&)");
        signatures.insert("double(*.armaRidgePrepKcvLL)(const arma::vec,const arma::mat&,const arma::ivec&,const arma::mat&,const arma::mat&,const int&,const double&,Rcpp::List,std::string)");
        signatures.insert("Rcpp::List(*.armaRidgePrepEMdiag)(arma::mat,arma::ivec,const double,const arma::mat&,const unsigned int&,const double&)");
        signatures.insert("double(*.armaRidgePrepKcvLLdiag)(const double,const arma::mat&,const arma::ivec&,const arma::mat&,const int&,const double&,Rcpp::List,std::string)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _porridge_RcppExport_registerCCallable() { 
    R_RegisterCCallable("porridge", "_porridge_.armaRidgePgen", (DL_FUNC)_porridge_ridgePgen_try);
    R_RegisterCCallable("porridge", "_porridge_.armaKcvlossR", (DL_FUNC)_porridge_kcvlossR_try);
    R_RegisterCCallable("porridge", "_porridge_.armaPenaltyPgen_banded", (DL_FUNC)_porridge_penaltyPgen_banded_try);
    R_RegisterCCallable("porridge", "_porridge_.armaPenaltyPgen_groups", (DL_FUNC)_porridge_penaltyPgen_groups_try);
    R_RegisterCCallable("porridge", "_porridge_.armaKCVlossR_groups", (DL_FUNC)_porridge_kcvlossR_groups_try);
    R_RegisterCCallable("porridge", "_porridge_.armaKCVlossR_banded", (DL_FUNC)_porridge_kcvlossR_banded_try);
    R_RegisterCCallable("porridge", "_porridge_.armaRidgeGGMmixture", (DL_FUNC)_porridge_ridgeGGMmixture_try);
    R_RegisterCCallable("porridge", "_porridge_.armaKcvlGGMmixture", (DL_FUNC)_porridge_kcvlGGMmixture_try);
    R_RegisterCCallable("porridge", "_porridge_.armaRidgePrepEM", (DL_FUNC)_porridge_ridgePrepEM_try);
    R_RegisterCCallable("porridge", "_porridge_.armaRidgePrepKcvLL", (DL_FUNC)_porridge_ridgePrepKcvLL_try);
    R_RegisterCCallable("porridge", "_porridge_.armaRidgePrepEMdiag", (DL_FUNC)_porridge_ridgePrepEMdiag_try);
    R_RegisterCCallable("porridge", "_porridge_.armaRidgePrepKcvLLdiag", (DL_FUNC)_porridge_ridgePrepKcvLLdiag_try);
    R_RegisterCCallable("porridge", "_porridge_RcppExport_validate", (DL_FUNC)_porridge_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_porridge_ridgePgen", (DL_FUNC) &_porridge_ridgePgen, 5},
    {"_porridge_kcvlossR", (DL_FUNC) &_porridge_kcvlossR, 6},
    {"_porridge_penaltyPgen_banded", (DL_FUNC) &_porridge_penaltyPgen_banded, 5},
    {"_porridge_penaltyPgen_groups", (DL_FUNC) &_porridge_penaltyPgen_groups, 5},
    {"_porridge_kcvlossR_groups", (DL_FUNC) &_porridge_kcvlossR_groups, 10},
    {"_porridge_kcvlossR_banded", (DL_FUNC) &_porridge_kcvlossR_banded, 9},
    {"_porridge_ridgeGGMmixture", (DL_FUNC) &_porridge_ridgeGGMmixture, 8},
    {"_porridge_kcvlGGMmixture", (DL_FUNC) &_porridge_kcvlGGMmixture, 9},
    {"_porridge_ridgePrepEM", (DL_FUNC) &_porridge_ridgePrepEM, 8},
    {"_porridge_ridgePrepKcvLL", (DL_FUNC) &_porridge_ridgePrepKcvLL, 9},
    {"_porridge_ridgePrepEMdiag", (DL_FUNC) &_porridge_ridgePrepEMdiag, 6},
    {"_porridge_ridgePrepKcvLLdiag", (DL_FUNC) &_porridge_ridgePrepKcvLLdiag, 8},
    {"_porridge_RcppExport_registerCCallable", (DL_FUNC) &_porridge_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_porridge(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
