// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// compare
Rcpp::List compare(SEXP db, SEXP numLoci, SEXP bigHit, SEXP trace, SEXP single, SEXP useWildcard, SEXP useWildcardEffect, SEXP useRallele);
RcppExport SEXP _DNAtools_compare(SEXP dbSEXP, SEXP numLociSEXP, SEXP bigHitSEXP, SEXP traceSEXP, SEXP singleSEXP, SEXP useWildcardSEXP, SEXP useWildcardEffectSEXP, SEXP useRalleleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type db(dbSEXP);
    Rcpp::traits::input_parameter< SEXP >::type numLoci(numLociSEXP);
    Rcpp::traits::input_parameter< SEXP >::type bigHit(bigHitSEXP);
    Rcpp::traits::input_parameter< SEXP >::type trace(traceSEXP);
    Rcpp::traits::input_parameter< SEXP >::type single(singleSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useWildcard(useWildcardSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useWildcardEffect(useWildcardEffectSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useRallele(useRalleleSEXP);
    rcpp_result_gen = Rcpp::wrap(compare(db, numLoci, bigHit, trace, single, useWildcard, useWildcardEffect, useRallele));
    return rcpp_result_gen;
END_RCPP
}
// score
Rcpp::IntegerVector score(SEXP prof1, SEXP prof2, SEXP numLoci, SEXP useWildCard, SEXP useRareAllele);
RcppExport SEXP _DNAtools_score(SEXP prof1SEXP, SEXP prof2SEXP, SEXP numLociSEXP, SEXP useWildCardSEXP, SEXP useRareAlleleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type prof1(prof1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type prof2(prof2SEXP);
    Rcpp::traits::input_parameter< SEXP >::type numLoci(numLociSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useWildCard(useWildCardSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useRareAllele(useRareAlleleSEXP);
    rcpp_result_gen = Rcpp::wrap(score(prof1, prof2, numLoci, useWildCard, useRareAllele));
    return rcpp_result_gen;
END_RCPP
}
// mcompare
Rcpp::List mcompare(SEXP db, SEXP numLoci, SEXP bigHit, SEXP trace, SEXP single, SEXP numThreads, SEXP useWildcard, SEXP useWildcardEffect, SEXP useRallele);
RcppExport SEXP _DNAtools_mcompare(SEXP dbSEXP, SEXP numLociSEXP, SEXP bigHitSEXP, SEXP traceSEXP, SEXP singleSEXP, SEXP numThreadsSEXP, SEXP useWildcardSEXP, SEXP useWildcardEffectSEXP, SEXP useRalleleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type db(dbSEXP);
    Rcpp::traits::input_parameter< SEXP >::type numLoci(numLociSEXP);
    Rcpp::traits::input_parameter< SEXP >::type bigHit(bigHitSEXP);
    Rcpp::traits::input_parameter< SEXP >::type trace(traceSEXP);
    Rcpp::traits::input_parameter< SEXP >::type single(singleSEXP);
    Rcpp::traits::input_parameter< SEXP >::type numThreads(numThreadsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useWildcard(useWildcardSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useWildcardEffect(useWildcardEffectSEXP);
    Rcpp::traits::input_parameter< SEXP >::type useRallele(useRalleleSEXP);
    rcpp_result_gen = Rcpp::wrap(mcompare(db, numLoci, bigHit, trace, single, numThreads, useWildcard, useWildcardEffect, useRallele));
    return rcpp_result_gen;
END_RCPP
}
// Prob
NumericVector Prob(CharacterVector vstrCombs, NumericVector q, NumericVector R, double r, double t);
RcppExport SEXP _DNAtools_Prob(SEXP vstrCombsSEXP, SEXP qSEXP, SEXP RSEXP, SEXP rSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type vstrCombs(vstrCombsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type q(qSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(Prob(vstrCombs, q, R, r, t));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP DNAtools_compare(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP DNAtools_mcompare(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP DNAtools_Prob(SEXP, SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP score(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_DNAtools_compare", (DL_FUNC) &_DNAtools_compare, 8},
    {"_DNAtools_score", (DL_FUNC) &_DNAtools_score, 5},
    {"_DNAtools_mcompare", (DL_FUNC) &_DNAtools_mcompare, 9},
    {"_DNAtools_Prob", (DL_FUNC) &_DNAtools_Prob, 5},
    {"DNAtools_compare",  (DL_FUNC) &DNAtools_compare,  8},
    {"DNAtools_mcompare", (DL_FUNC) &DNAtools_mcompare, 9},
    {"DNAtools_Prob",     (DL_FUNC) &DNAtools_Prob,     5},
    {"score",             (DL_FUNC) &score,             5},
    {NULL, NULL, 0}
};

RcppExport void R_init_DNAtools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
