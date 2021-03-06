// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/PoissonBinomial.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// dpb_conv
NumericVector dpb_conv(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_dpb_conv_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_conv(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_conv(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_conv_try(obsSEXP, probsSEXP));
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
// ppb_conv
NumericVector ppb_conv(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_ppb_conv_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_conv(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_conv(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_conv_try(obsSEXP, probsSEXP));
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
// dpb_dc
NumericVector dpb_dc(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_dpb_dc_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_dc(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_dc(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_dc_try(obsSEXP, probsSEXP));
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
// ppb_dc
NumericVector ppb_dc(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_ppb_dc_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_dc(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_dc(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_dc_try(obsSEXP, probsSEXP));
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
// dpb_dftcf
NumericVector dpb_dftcf(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_dpb_dftcf_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_dftcf(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_dftcf(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_dftcf_try(obsSEXP, probsSEXP));
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
// ppb_dftcf
NumericVector ppb_dftcf(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_ppb_dftcf_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_dftcf(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_dftcf(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_dftcf_try(obsSEXP, probsSEXP));
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
// dpb_rf
NumericVector dpb_rf(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_dpb_rf_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_rf(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_rf(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_rf_try(obsSEXP, probsSEXP));
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
// ppb_rf
NumericVector ppb_rf(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_ppb_rf_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_rf(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_rf(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_rf_try(obsSEXP, probsSEXP));
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
// dpb_mean
NumericVector dpb_mean(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_dpb_mean_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_mean(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_mean(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_mean_try(obsSEXP, probsSEXP));
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
// ppb_mean
NumericVector ppb_mean(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_ppb_mean_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_mean(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_mean(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_mean_try(obsSEXP, probsSEXP));
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
// dpb_gmba
NumericVector dpb_gmba(IntegerVector obs, NumericVector probs, bool anti);
static SEXP _PoissonBinomial_dpb_gmba_try(SEXP obsSEXP, SEXP probsSEXP, SEXP antiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< bool >::type anti(antiSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_gmba(obs, probs, anti));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_gmba(SEXP obsSEXP, SEXP probsSEXP, SEXP antiSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_gmba_try(obsSEXP, probsSEXP, antiSEXP));
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
// ppb_gmba
NumericVector ppb_gmba(IntegerVector obs, NumericVector probs, bool anti);
static SEXP _PoissonBinomial_ppb_gmba_try(SEXP obsSEXP, SEXP probsSEXP, SEXP antiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< bool >::type anti(antiSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_gmba(obs, probs, anti));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_gmba(SEXP obsSEXP, SEXP probsSEXP, SEXP antiSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_gmba_try(obsSEXP, probsSEXP, antiSEXP));
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
// dpb_pa
NumericVector dpb_pa(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_dpb_pa_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_pa(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_pa(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_pa_try(obsSEXP, probsSEXP));
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
// ppb_pa
NumericVector ppb_pa(IntegerVector obs, NumericVector probs);
static SEXP _PoissonBinomial_ppb_pa_try(SEXP obsSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_pa(obs, probs));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_pa(SEXP obsSEXP, SEXP probsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_pa_try(obsSEXP, probsSEXP));
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
// dpb_na
NumericVector dpb_na(IntegerVector obs, NumericVector probs, bool refined);
static SEXP _PoissonBinomial_dpb_na_try(SEXP obsSEXP, SEXP probsSEXP, SEXP refinedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< bool >::type refined(refinedSEXP);
    rcpp_result_gen = Rcpp::wrap(dpb_na(obs, probs, refined));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_dpb_na(SEXP obsSEXP, SEXP probsSEXP, SEXP refinedSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_dpb_na_try(obsSEXP, probsSEXP, refinedSEXP));
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
// ppb_na
NumericVector ppb_na(IntegerVector obs, NumericVector probs, bool refined);
static SEXP _PoissonBinomial_ppb_na_try(SEXP obsSEXP, SEXP probsSEXP, SEXP refinedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type obs(obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< bool >::type refined(refinedSEXP);
    rcpp_result_gen = Rcpp::wrap(ppb_na(obs, probs, refined));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _PoissonBinomial_ppb_na(SEXP obsSEXP, SEXP probsSEXP, SEXP refinedSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_PoissonBinomial_ppb_na_try(obsSEXP, probsSEXP, refinedSEXP));
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
static int _PoissonBinomial_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("NumericVector(*dpb_conv)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*ppb_conv)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*dpb_dc)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*ppb_dc)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*dpb_dftcf)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*ppb_dftcf)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*dpb_rf)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*ppb_rf)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*dpb_mean)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*ppb_mean)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*dpb_gmba)(IntegerVector,NumericVector,bool)");
        signatures.insert("NumericVector(*ppb_gmba)(IntegerVector,NumericVector,bool)");
        signatures.insert("NumericVector(*dpb_pa)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*ppb_pa)(IntegerVector,NumericVector)");
        signatures.insert("NumericVector(*dpb_na)(IntegerVector,NumericVector,bool)");
        signatures.insert("NumericVector(*ppb_na)(IntegerVector,NumericVector,bool)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _PoissonBinomial_RcppExport_registerCCallable() { 
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_conv", (DL_FUNC)_PoissonBinomial_dpb_conv_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_conv", (DL_FUNC)_PoissonBinomial_ppb_conv_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_dc", (DL_FUNC)_PoissonBinomial_dpb_dc_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_dc", (DL_FUNC)_PoissonBinomial_ppb_dc_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_dftcf", (DL_FUNC)_PoissonBinomial_dpb_dftcf_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_dftcf", (DL_FUNC)_PoissonBinomial_ppb_dftcf_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_rf", (DL_FUNC)_PoissonBinomial_dpb_rf_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_rf", (DL_FUNC)_PoissonBinomial_ppb_rf_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_mean", (DL_FUNC)_PoissonBinomial_dpb_mean_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_mean", (DL_FUNC)_PoissonBinomial_ppb_mean_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_gmba", (DL_FUNC)_PoissonBinomial_dpb_gmba_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_gmba", (DL_FUNC)_PoissonBinomial_ppb_gmba_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_pa", (DL_FUNC)_PoissonBinomial_dpb_pa_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_pa", (DL_FUNC)_PoissonBinomial_ppb_pa_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_dpb_na", (DL_FUNC)_PoissonBinomial_dpb_na_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_ppb_na", (DL_FUNC)_PoissonBinomial_ppb_na_try);
    R_RegisterCCallable("PoissonBinomial", "_PoissonBinomial_RcppExport_validate", (DL_FUNC)_PoissonBinomial_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_PoissonBinomial_dpb_conv", (DL_FUNC) &_PoissonBinomial_dpb_conv, 2},
    {"_PoissonBinomial_ppb_conv", (DL_FUNC) &_PoissonBinomial_ppb_conv, 2},
    {"_PoissonBinomial_dpb_dc", (DL_FUNC) &_PoissonBinomial_dpb_dc, 2},
    {"_PoissonBinomial_ppb_dc", (DL_FUNC) &_PoissonBinomial_ppb_dc, 2},
    {"_PoissonBinomial_dpb_dftcf", (DL_FUNC) &_PoissonBinomial_dpb_dftcf, 2},
    {"_PoissonBinomial_ppb_dftcf", (DL_FUNC) &_PoissonBinomial_ppb_dftcf, 2},
    {"_PoissonBinomial_dpb_rf", (DL_FUNC) &_PoissonBinomial_dpb_rf, 2},
    {"_PoissonBinomial_ppb_rf", (DL_FUNC) &_PoissonBinomial_ppb_rf, 2},
    {"_PoissonBinomial_dpb_mean", (DL_FUNC) &_PoissonBinomial_dpb_mean, 2},
    {"_PoissonBinomial_ppb_mean", (DL_FUNC) &_PoissonBinomial_ppb_mean, 2},
    {"_PoissonBinomial_dpb_gmba", (DL_FUNC) &_PoissonBinomial_dpb_gmba, 3},
    {"_PoissonBinomial_ppb_gmba", (DL_FUNC) &_PoissonBinomial_ppb_gmba, 3},
    {"_PoissonBinomial_dpb_pa", (DL_FUNC) &_PoissonBinomial_dpb_pa, 2},
    {"_PoissonBinomial_ppb_pa", (DL_FUNC) &_PoissonBinomial_ppb_pa, 2},
    {"_PoissonBinomial_dpb_na", (DL_FUNC) &_PoissonBinomial_dpb_na, 3},
    {"_PoissonBinomial_ppb_na", (DL_FUNC) &_PoissonBinomial_ppb_na, 3},
    {"_PoissonBinomial_RcppExport_registerCCallable", (DL_FUNC) &_PoissonBinomial_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_PoissonBinomial(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
