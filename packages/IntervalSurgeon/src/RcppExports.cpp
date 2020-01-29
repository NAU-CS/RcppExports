// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_hyper_cubes
IntegerMatrix rcpp_hyper_cubes(IntegerVector vals, IntegerVector starts, IntegerMatrix lengths);
RcppExport SEXP _IntervalSurgeon_rcpp_hyper_cubes(SEXP valsSEXP, SEXP startsSEXP, SEXP lengthsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type vals(valsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type starts(startsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type lengths(lengthsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_hyper_cubes(vals, starts, lengths));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_depth
IntegerVector rcpp_depth(IntegerVector sorted_starts, IntegerVector sorted_ends, IntegerVector pts);
RcppExport SEXP _IntervalSurgeon_rcpp_depth(SEXP sorted_startsSEXP, SEXP sorted_endsSEXP, SEXP ptsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type sorted_starts(sorted_startsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type sorted_ends(sorted_endsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type pts(ptsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_depth(sorted_starts, sorted_ends, pts));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_pile
IntegerVector rcpp_pile(IntegerVector starts, IntegerVector ends, IntegerVector pts, int total_members);
RcppExport SEXP _IntervalSurgeon_rcpp_pile(SEXP startsSEXP, SEXP endsSEXP, SEXP ptsSEXP, SEXP total_membersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type starts(startsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ends(endsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type pts(ptsSEXP);
    Rcpp::traits::input_parameter< int >::type total_members(total_membersSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_pile(starts, ends, pts, total_members));
    return rcpp_result_gen;
END_RCPP
}
// dash_set_overlaps
LogicalVector dash_set_overlaps(IntegerVector starts1, IntegerVector ends1, IntegerVector starts2, IntegerVector ends2, bool state1, bool state2, bool op_is_and, IntegerVector pts);
RcppExport SEXP _IntervalSurgeon_dash_set_overlaps(SEXP starts1SEXP, SEXP ends1SEXP, SEXP starts2SEXP, SEXP ends2SEXP, SEXP state1SEXP, SEXP state2SEXP, SEXP op_is_andSEXP, SEXP ptsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type starts1(starts1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ends1(ends1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type starts2(starts2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ends2(ends2SEXP);
    Rcpp::traits::input_parameter< bool >::type state1(state1SEXP);
    Rcpp::traits::input_parameter< bool >::type state2(state2SEXP);
    Rcpp::traits::input_parameter< bool >::type op_is_and(op_is_andSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type pts(ptsSEXP);
    rcpp_result_gen = Rcpp::wrap(dash_set_overlaps(starts1, ends1, starts2, ends2, state1, state2, op_is_and, pts));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_IntervalSurgeon_rcpp_hyper_cubes", (DL_FUNC) &_IntervalSurgeon_rcpp_hyper_cubes, 3},
    {"_IntervalSurgeon_rcpp_depth", (DL_FUNC) &_IntervalSurgeon_rcpp_depth, 3},
    {"_IntervalSurgeon_rcpp_pile", (DL_FUNC) &_IntervalSurgeon_rcpp_pile, 4},
    {"_IntervalSurgeon_dash_set_overlaps", (DL_FUNC) &_IntervalSurgeon_dash_set_overlaps, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_IntervalSurgeon(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
