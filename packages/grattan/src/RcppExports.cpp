// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// AnyWhich
int AnyWhich(IntegerVector x, int a, bool gt, bool lt, bool eq);
RcppExport SEXP _grattan_AnyWhich(SEXP xSEXP, SEXP aSEXP, SEXP gtSEXP, SEXP ltSEXP, SEXP eqSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< bool >::type gt(gtSEXP);
    Rcpp::traits::input_parameter< bool >::type lt(ltSEXP);
    Rcpp::traits::input_parameter< bool >::type eq(eqSEXP);
    rcpp_result_gen = Rcpp::wrap(AnyWhich(x, a, gt, lt, eq));
    return rcpp_result_gen;
END_RCPP
}
// IncomeTax
NumericVector IncomeTax(NumericVector x, NumericVector thresholds, NumericVector rates);
RcppExport SEXP _grattan_IncomeTax(SEXP xSEXP, SEXP thresholdsSEXP, SEXP ratesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thresholds(thresholdsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rates(ratesSEXP);
    rcpp_result_gen = Rcpp::wrap(IncomeTax(x, thresholds, rates));
    return rcpp_result_gen;
END_RCPP
}
// MedicareLevySingle
double MedicareLevySingle(double income, double lowerThreshold, double upperThreshold, double rate, double taper, double SpouseIncome, bool isFamily, int nDependants, double lowerFamilyThreshold, double upperFamilyThreshold, double lowerUpForEachChild);
RcppExport SEXP _grattan_MedicareLevySingle(SEXP incomeSEXP, SEXP lowerThresholdSEXP, SEXP upperThresholdSEXP, SEXP rateSEXP, SEXP taperSEXP, SEXP SpouseIncomeSEXP, SEXP isFamilySEXP, SEXP nDependantsSEXP, SEXP lowerFamilyThresholdSEXP, SEXP upperFamilyThresholdSEXP, SEXP lowerUpForEachChildSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type income(incomeSEXP);
    Rcpp::traits::input_parameter< double >::type lowerThreshold(lowerThresholdSEXP);
    Rcpp::traits::input_parameter< double >::type upperThreshold(upperThresholdSEXP);
    Rcpp::traits::input_parameter< double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< double >::type taper(taperSEXP);
    Rcpp::traits::input_parameter< double >::type SpouseIncome(SpouseIncomeSEXP);
    Rcpp::traits::input_parameter< bool >::type isFamily(isFamilySEXP);
    Rcpp::traits::input_parameter< int >::type nDependants(nDependantsSEXP);
    Rcpp::traits::input_parameter< double >::type lowerFamilyThreshold(lowerFamilyThresholdSEXP);
    Rcpp::traits::input_parameter< double >::type upperFamilyThreshold(upperFamilyThresholdSEXP);
    Rcpp::traits::input_parameter< double >::type lowerUpForEachChild(lowerUpForEachChildSEXP);
    rcpp_result_gen = Rcpp::wrap(MedicareLevySingle(income, lowerThreshold, upperThreshold, rate, taper, SpouseIncome, isFamily, nDependants, lowerFamilyThreshold, upperFamilyThreshold, lowerUpForEachChild));
    return rcpp_result_gen;
END_RCPP
}
// MedicareLevySaptoYear
NumericVector MedicareLevySaptoYear(NumericVector income, NumericVector SpouseIncome, IntegerVector NDependants, LogicalVector SaptoEligible, int yr);
RcppExport SEXP _grattan_MedicareLevySaptoYear(SEXP incomeSEXP, SEXP SpouseIncomeSEXP, SEXP NDependantsSEXP, SEXP SaptoEligibleSEXP, SEXP yrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type income(incomeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type SpouseIncome(SpouseIncomeSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type NDependants(NDependantsSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type SaptoEligible(SaptoEligibleSEXP);
    Rcpp::traits::input_parameter< int >::type yr(yrSEXP);
    rcpp_result_gen = Rcpp::wrap(MedicareLevySaptoYear(income, SpouseIncome, NDependants, SaptoEligible, yr));
    return rcpp_result_gen;
END_RCPP
}
// MedicareLevy
NumericVector MedicareLevy(NumericVector income, NumericVector lowerThreshold, NumericVector upperThreshold, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants, NumericVector lowerFamilyThreshold, NumericVector upperFamilyThreshold, NumericVector lowerUpForEachChild, NumericVector rate, NumericVector taper);
RcppExport SEXP _grattan_MedicareLevy(SEXP incomeSEXP, SEXP lowerThresholdSEXP, SEXP upperThresholdSEXP, SEXP SpouseIncomeSEXP, SEXP isFamilySEXP, SEXP NDependantsSEXP, SEXP lowerFamilyThresholdSEXP, SEXP upperFamilyThresholdSEXP, SEXP lowerUpForEachChildSEXP, SEXP rateSEXP, SEXP taperSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type income(incomeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lowerThreshold(lowerThresholdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type upperThreshold(upperThresholdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type SpouseIncome(SpouseIncomeSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type isFamily(isFamilySEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type NDependants(NDependantsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lowerFamilyThreshold(lowerFamilyThresholdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type upperFamilyThreshold(upperFamilyThresholdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lowerUpForEachChild(lowerUpForEachChildSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type taper(taperSEXP);
    rcpp_result_gen = Rcpp::wrap(MedicareLevy(income, lowerThreshold, upperThreshold, SpouseIncome, isFamily, NDependants, lowerFamilyThreshold, upperFamilyThreshold, lowerUpForEachChild, rate, taper));
    return rcpp_result_gen;
END_RCPP
}
// Offset
NumericVector Offset(NumericVector x, double y, double a, double m);
RcppExport SEXP _grattan_Offset(SEXP xSEXP, SEXP ySEXP, SEXP aSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(Offset(x, y, a, m));
    return rcpp_result_gen;
END_RCPP
}
// anyOutside
int anyOutside(IntegerVector x, int a, int b);
RcppExport SEXP _grattan_anyOutside(SEXP xSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(anyOutside(x, a, b));
    return rcpp_result_gen;
END_RCPP
}
// pmax3
NumericVector pmax3(NumericVector x, NumericVector y, NumericVector z);
RcppExport SEXP _grattan_pmax3(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(pmax3(x, y, z));
    return rcpp_result_gen;
END_RCPP
}
// pmaxC
NumericVector pmaxC(NumericVector x, double a);
RcppExport SEXP _grattan_pmaxC(SEXP xSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(pmaxC(x, a));
    return rcpp_result_gen;
END_RCPP
}
// pmaxCint
IntegerVector pmaxCint(IntegerVector x, int a);
RcppExport SEXP _grattan_pmaxCint(SEXP xSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(pmaxCint(x, a));
    return rcpp_result_gen;
END_RCPP
}
// pmax0
NumericVector pmax0(NumericVector x);
RcppExport SEXP _grattan_pmax0(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(pmax0(x));
    return rcpp_result_gen;
END_RCPP
}
// pmaxIPnum0
NumericVector pmaxIPnum0(NumericVector x);
RcppExport SEXP _grattan_pmaxIPnum0(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(pmaxIPnum0(x));
    return rcpp_result_gen;
END_RCPP
}
// pmaxIPint0
IntegerVector pmaxIPint0(IntegerVector x);
RcppExport SEXP _grattan_pmaxIPint0(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(pmaxIPint0(x));
    return rcpp_result_gen;
END_RCPP
}
// pmaxV
NumericVector pmaxV(NumericVector x, NumericVector y);
RcppExport SEXP _grattan_pmaxV(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(pmaxV(x, y));
    return rcpp_result_gen;
END_RCPP
}
// pminC
NumericVector pminC(NumericVector x, double a);
RcppExport SEXP _grattan_pminC(SEXP xSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(pminC(x, a));
    return rcpp_result_gen;
END_RCPP
}
// pmin0
NumericVector pmin0(NumericVector x);
RcppExport SEXP _grattan_pmin0(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(pmin0(x));
    return rcpp_result_gen;
END_RCPP
}
// pminV
NumericVector pminV(NumericVector x, NumericVector y);
RcppExport SEXP _grattan_pminV(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(pminV(x, y));
    return rcpp_result_gen;
END_RCPP
}
// do_sapto_rcpp
NumericVector do_sapto_rcpp(NumericVector RebateIncome, NumericVector MaxOffset, NumericVector LowerThreshold, NumericVector TaperRate, LogicalVector SaptoEligible, NumericVector SpouseIncome, LogicalVector IsMarried);
RcppExport SEXP _grattan_do_sapto_rcpp(SEXP RebateIncomeSEXP, SEXP MaxOffsetSEXP, SEXP LowerThresholdSEXP, SEXP TaperRateSEXP, SEXP SaptoEligibleSEXP, SEXP SpouseIncomeSEXP, SEXP IsMarriedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type RebateIncome(RebateIncomeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type MaxOffset(MaxOffsetSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type LowerThreshold(LowerThresholdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type TaperRate(TaperRateSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type SaptoEligible(SaptoEligibleSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type SpouseIncome(SpouseIncomeSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type IsMarried(IsMarriedSEXP);
    rcpp_result_gen = Rcpp::wrap(do_sapto_rcpp(RebateIncome, MaxOffset, LowerThreshold, TaperRate, SaptoEligible, SpouseIncome, IsMarried));
    return rcpp_result_gen;
END_RCPP
}
// do_sapto_rcpp2
NumericVector do_sapto_rcpp2(NumericVector RebateIncome, double maxOffsetSingle, double maxOffsetMarried, double lowerThresholdSingle, double lowerThresholdMarried, double taperRateSingle, double taperRateMarried, LogicalVector SaptoEligible, LogicalVector IsMarried, NumericVector SpouseIncome);
RcppExport SEXP _grattan_do_sapto_rcpp2(SEXP RebateIncomeSEXP, SEXP maxOffsetSingleSEXP, SEXP maxOffsetMarriedSEXP, SEXP lowerThresholdSingleSEXP, SEXP lowerThresholdMarriedSEXP, SEXP taperRateSingleSEXP, SEXP taperRateMarriedSEXP, SEXP SaptoEligibleSEXP, SEXP IsMarriedSEXP, SEXP SpouseIncomeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type RebateIncome(RebateIncomeSEXP);
    Rcpp::traits::input_parameter< double >::type maxOffsetSingle(maxOffsetSingleSEXP);
    Rcpp::traits::input_parameter< double >::type maxOffsetMarried(maxOffsetMarriedSEXP);
    Rcpp::traits::input_parameter< double >::type lowerThresholdSingle(lowerThresholdSingleSEXP);
    Rcpp::traits::input_parameter< double >::type lowerThresholdMarried(lowerThresholdMarriedSEXP);
    Rcpp::traits::input_parameter< double >::type taperRateSingle(taperRateSingleSEXP);
    Rcpp::traits::input_parameter< double >::type taperRateMarried(taperRateMarriedSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type SaptoEligible(SaptoEligibleSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type IsMarried(IsMarriedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type SpouseIncome(SpouseIncomeSEXP);
    rcpp_result_gen = Rcpp::wrap(do_sapto_rcpp2(RebateIncome, maxOffsetSingle, maxOffsetMarried, lowerThresholdSingle, lowerThresholdMarried, taperRateSingle, taperRateMarried, SaptoEligible, IsMarried, SpouseIncome));
    return rcpp_result_gen;
END_RCPP
}
// sapto_rcpp_singleton
double sapto_rcpp_singleton(double rebate_income, double max_offset, double lower_threshold, double taper_rate, bool sapto_eligible, double Spouse_income, bool is_married);
RcppExport SEXP _grattan_sapto_rcpp_singleton(SEXP rebate_incomeSEXP, SEXP max_offsetSEXP, SEXP lower_thresholdSEXP, SEXP taper_rateSEXP, SEXP sapto_eligibleSEXP, SEXP Spouse_incomeSEXP, SEXP is_marriedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type rebate_income(rebate_incomeSEXP);
    Rcpp::traits::input_parameter< double >::type max_offset(max_offsetSEXP);
    Rcpp::traits::input_parameter< double >::type lower_threshold(lower_thresholdSEXP);
    Rcpp::traits::input_parameter< double >::type taper_rate(taper_rateSEXP);
    Rcpp::traits::input_parameter< bool >::type sapto_eligible(sapto_eligibleSEXP);
    Rcpp::traits::input_parameter< double >::type Spouse_income(Spouse_incomeSEXP);
    Rcpp::traits::input_parameter< bool >::type is_married(is_marriedSEXP);
    rcpp_result_gen = Rcpp::wrap(sapto_rcpp_singleton(rebate_income, max_offset, lower_threshold, taper_rate, sapto_eligible, Spouse_income, is_married));
    return rcpp_result_gen;
END_RCPP
}
// sapto_rcpp_yr_singleton
double sapto_rcpp_yr_singleton(double rebateIncome, bool isMarried, double spouseIncome, int yr);
RcppExport SEXP _grattan_sapto_rcpp_yr_singleton(SEXP rebateIncomeSEXP, SEXP isMarriedSEXP, SEXP spouseIncomeSEXP, SEXP yrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type rebateIncome(rebateIncomeSEXP);
    Rcpp::traits::input_parameter< bool >::type isMarried(isMarriedSEXP);
    Rcpp::traits::input_parameter< double >::type spouseIncome(spouseIncomeSEXP);
    Rcpp::traits::input_parameter< int >::type yr(yrSEXP);
    rcpp_result_gen = Rcpp::wrap(sapto_rcpp_yr_singleton(rebateIncome, isMarried, spouseIncome, yr));
    return rcpp_result_gen;
END_RCPP
}
// sapto_rcpp_yr
NumericVector sapto_rcpp_yr(NumericVector RebateIncome, NumericVector SpouseIncome, LogicalVector IsMarried, int yr);
RcppExport SEXP _grattan_sapto_rcpp_yr(SEXP RebateIncomeSEXP, SEXP SpouseIncomeSEXP, SEXP IsMarriedSEXP, SEXP yrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type RebateIncome(RebateIncomeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type SpouseIncome(SpouseIncomeSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type IsMarried(IsMarriedSEXP);
    Rcpp::traits::input_parameter< int >::type yr(yrSEXP);
    rcpp_result_gen = Rcpp::wrap(sapto_rcpp_yr(RebateIncome, SpouseIncome, IsMarried, yr));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_grattan_AnyWhich", (DL_FUNC) &_grattan_AnyWhich, 5},
    {"_grattan_IncomeTax", (DL_FUNC) &_grattan_IncomeTax, 3},
    {"_grattan_MedicareLevySingle", (DL_FUNC) &_grattan_MedicareLevySingle, 11},
    {"_grattan_MedicareLevySaptoYear", (DL_FUNC) &_grattan_MedicareLevySaptoYear, 5},
    {"_grattan_MedicareLevy", (DL_FUNC) &_grattan_MedicareLevy, 11},
    {"_grattan_Offset", (DL_FUNC) &_grattan_Offset, 4},
    {"_grattan_anyOutside", (DL_FUNC) &_grattan_anyOutside, 3},
    {"_grattan_pmax3", (DL_FUNC) &_grattan_pmax3, 3},
    {"_grattan_pmaxC", (DL_FUNC) &_grattan_pmaxC, 2},
    {"_grattan_pmaxCint", (DL_FUNC) &_grattan_pmaxCint, 2},
    {"_grattan_pmax0", (DL_FUNC) &_grattan_pmax0, 1},
    {"_grattan_pmaxIPnum0", (DL_FUNC) &_grattan_pmaxIPnum0, 1},
    {"_grattan_pmaxIPint0", (DL_FUNC) &_grattan_pmaxIPint0, 1},
    {"_grattan_pmaxV", (DL_FUNC) &_grattan_pmaxV, 2},
    {"_grattan_pminC", (DL_FUNC) &_grattan_pminC, 2},
    {"_grattan_pmin0", (DL_FUNC) &_grattan_pmin0, 1},
    {"_grattan_pminV", (DL_FUNC) &_grattan_pminV, 2},
    {"_grattan_do_sapto_rcpp", (DL_FUNC) &_grattan_do_sapto_rcpp, 7},
    {"_grattan_do_sapto_rcpp2", (DL_FUNC) &_grattan_do_sapto_rcpp2, 10},
    {"_grattan_sapto_rcpp_singleton", (DL_FUNC) &_grattan_sapto_rcpp_singleton, 7},
    {"_grattan_sapto_rcpp_yr_singleton", (DL_FUNC) &_grattan_sapto_rcpp_yr_singleton, 4},
    {"_grattan_sapto_rcpp_yr", (DL_FUNC) &_grattan_sapto_rcpp_yr, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_grattan(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}