// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// C_concat
std::string C_concat(const CharacterVector x, const std::string sep);
RcppExport SEXP _cna_C_concat(SEXP xSEXP, SEXP sepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string >::type sep(sepSEXP);
    rcpp_result_gen = Rcpp::wrap(C_concat(x, sep));
    return rcpp_result_gen;
END_RCPP
}
// C_mconcat
CharacterVector C_mconcat(const charList x, const std::string sep, const bool sorted);
RcppExport SEXP _cna_C_mconcat(SEXP xSEXP, SEXP sepSEXP, SEXP sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const charList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< const bool >::type sorted(sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(C_mconcat(x, sep, sorted));
    return rcpp_result_gen;
END_RCPP
}
// C_charList2string
std::string C_charList2string(const charList x, const std::string disj, const std::string conj, const bool sorted);
RcppExport SEXP _cna_C_charList2string(SEXP xSEXP, SEXP disjSEXP, SEXP conjSEXP, SEXP sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const charList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string >::type disj(disjSEXP);
    Rcpp::traits::input_parameter< const std::string >::type conj(conjSEXP);
    Rcpp::traits::input_parameter< const bool >::type sorted(sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(C_charList2string(x, disj, conj, sorted));
    return rcpp_result_gen;
END_RCPP
}
// C_recCharList2char
CharacterVector C_recCharList2char(const recCharList x, const std::string disj, const std::string conj, const bool sorted);
RcppExport SEXP _cna_C_recCharList2char(SEXP xSEXP, SEXP disjSEXP, SEXP conjSEXP, SEXP sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const recCharList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string >::type disj(disjSEXP);
    Rcpp::traits::input_parameter< const std::string >::type conj(conjSEXP);
    Rcpp::traits::input_parameter< const bool >::type sorted(sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(C_recCharList2char(x, disj, conj, sorted));
    return rcpp_result_gen;
END_RCPP
}
// C_find_first_false
int C_find_first_false(const LogicalVector x);
RcppExport SEXP _cna_C_find_first_false(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_find_first_false(x));
    return rcpp_result_gen;
END_RCPP
}
// C_redund
LogicalVector C_redund(const LogicalMatrix x);
RcppExport SEXP _cna_C_redund(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_redund(x));
    return rcpp_result_gen;
END_RCPP
}
// C_mredund
List C_mredund(const LogicalMatrix x, const IntegerVector l);
RcppExport SEXP _cna_C_mredund(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(C_mredund(x, l));
    return rcpp_result_gen;
END_RCPP
}
// C_relist_Int
List C_relist_Int(IntegerVector x, const IntegerVector l);
RcppExport SEXP _cna_C_relist_Int(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(C_relist_Int(x, l));
    return rcpp_result_gen;
END_RCPP
}
// C_relist_Num
List C_relist_Num(const NumericVector x, const IntegerVector l);
RcppExport SEXP _cna_C_relist_Num(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(C_relist_Num(x, l));
    return rcpp_result_gen;
END_RCPP
}
// C_relist_Log
List C_relist_Log(const LogicalVector x, const IntegerVector l);
RcppExport SEXP _cna_C_relist_Log(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(C_relist_Log(x, l));
    return rcpp_result_gen;
END_RCPP
}
// C_relist_Char
List C_relist_Char(const CharacterVector x, const IntegerVector l);
RcppExport SEXP _cna_C_relist_Char(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(C_relist_Char(x, l));
    return rcpp_result_gen;
END_RCPP
}
// C_relist_List
List C_relist_List(const List x, const IntegerVector l);
RcppExport SEXP _cna_C_relist_List(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(C_relist_List(x, l));
    return rcpp_result_gen;
END_RCPP
}
// C_conCov
NumericVector C_conCov(const NumericVector x, const NumericVector y, const IntegerVector f);
RcppExport SEXP _cna_C_conCov(SEXP xSEXP, SEXP ySEXP, SEXP fSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type f(fSEXP);
    rcpp_result_gen = Rcpp::wrap(C_conCov(x, y, f));
    return rcpp_result_gen;
END_RCPP
}
// C_subsetMin
double C_subsetMin(const NumericVector x, const IntegerVector sub);
RcppExport SEXP _cna_C_subsetMin(SEXP xSEXP, SEXP subSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type sub(subSEXP);
    rcpp_result_gen = Rcpp::wrap(C_subsetMin(x, sub));
    return rcpp_result_gen;
END_RCPP
}
// C_conjScore
NumericMatrix C_conjScore(const NumericMatrix x, const IntegerMatrix m);
RcppExport SEXP _cna_C_conjScore(SEXP xSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerMatrix >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(C_conjScore(x, m));
    return rcpp_result_gen;
END_RCPP
}
// C_init_ii
IntegerVector C_init_ii(const IntegerVector nn, const LogicalVector st);
RcppExport SEXP _cna_C_init_ii(SEXP nnSEXP, SEXP stSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type nn(nnSEXP);
    Rcpp::traits::input_parameter< const LogicalVector >::type st(stSEXP);
    rcpp_result_gen = Rcpp::wrap(C_init_ii(nn, st));
    return rcpp_result_gen;
END_RCPP
}
// C_set_lim
IntegerVector C_set_lim(const IntegerVector nn, const LogicalVector st);
RcppExport SEXP _cna_C_set_lim(SEXP nnSEXP, SEXP stSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type nn(nnSEXP);
    Rcpp::traits::input_parameter< const LogicalVector >::type st(stSEXP);
    rcpp_result_gen = Rcpp::wrap(C_set_lim(nn, st));
    return rcpp_result_gen;
END_RCPP
}
// C_max_which
int C_max_which(const LogicalVector x);
RcppExport SEXP _cna_C_max_which(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_max_which(x));
    return rcpp_result_gen;
END_RCPP
}
// C_increment
IntegerVector C_increment(IntegerVector ii, const IntegerVector nn, const LogicalVector st, const IntegerVector lim);
RcppExport SEXP _cna_C_increment(SEXP iiSEXP, SEXP nnSEXP, SEXP stSEXP, SEXP limSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type ii(iiSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type nn(nnSEXP);
    Rcpp::traits::input_parameter< const LogicalVector >::type st(stSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type lim(limSEXP);
    rcpp_result_gen = Rcpp::wrap(C_increment(ii, nn, st, lim));
    return rcpp_result_gen;
END_RCPP
}
// C_find_asf
IntegerMatrix C_find_asf(const IntegerVector conjlen, const numMatList x, const NumericVector y, const IntegerVector f, const double con, const double cov, const int maxSol);
RcppExport SEXP _cna_C_find_asf(SEXP conjlenSEXP, SEXP xSEXP, SEXP ySEXP, SEXP fSEXP, SEXP conSEXP, SEXP covSEXP, SEXP maxSolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type conjlen(conjlenSEXP);
    Rcpp::traits::input_parameter< const numMatList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type f(fSEXP);
    Rcpp::traits::input_parameter< const double >::type con(conSEXP);
    Rcpp::traits::input_parameter< const double >::type cov(covSEXP);
    Rcpp::traits::input_parameter< const int >::type maxSol(maxSolSEXP);
    rcpp_result_gen = Rcpp::wrap(C_find_asf(conjlen, x, y, f, con, cov, maxSol));
    return rcpp_result_gen;
END_RCPP
}
// C_conj_conCov
NumericVector C_conj_conCov(const IntegerVector cols, const NumericMatrix x, const NumericVector y, const IntegerVector f);
RcppExport SEXP _cna_C_conj_conCov(SEXP colsSEXP, SEXP xSEXP, SEXP ySEXP, SEXP fSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type f(fSEXP);
    rcpp_result_gen = Rcpp::wrap(C_conj_conCov(cols, x, y, f));
    return rcpp_result_gen;
END_RCPP
}
// C_disj_conCov
NumericVector C_disj_conCov(const IntegerVector cols, const NumericMatrix x, const NumericVector y, const IntegerVector f);
RcppExport SEXP _cna_C_disj_conCov(SEXP colsSEXP, SEXP xSEXP, SEXP ySEXP, SEXP fSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type f(fSEXP);
    rcpp_result_gen = Rcpp::wrap(C_disj_conCov(cols, x, y, f));
    return rcpp_result_gen;
END_RCPP
}
// C_disj_contained
LogicalMatrix C_disj_contained(const intList x, const intList y, const bool shortcut);
RcppExport SEXP _cna_C_disj_contained(SEXP xSEXP, SEXP ySEXP, SEXP shortcutSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const intList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const intList >::type y(ySEXP);
    Rcpp::traits::input_parameter< const bool >::type shortcut(shortcutSEXP);
    rcpp_result_gen = Rcpp::wrap(C_disj_contained(x, y, shortcut));
    return rcpp_result_gen;
END_RCPP
}
// C_is_submodel
LogicalVector C_is_submodel(const recIntList x, const intList ref, const bool strict);
RcppExport SEXP _cna_C_is_submodel(SEXP xSEXP, SEXP refSEXP, SEXP strictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const recIntList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const intList >::type ref(refSEXP);
    Rcpp::traits::input_parameter< const bool >::type strict(strictSEXP);
    rcpp_result_gen = Rcpp::wrap(C_is_submodel(x, ref, strict));
    return rcpp_result_gen;
END_RCPP
}
// C_minimal
LogicalVector C_minimal(const recIntList x, const recIntList ref, bool strict);
RcppExport SEXP _cna_C_minimal(SEXP xSEXP, SEXP refSEXP, SEXP strictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const recIntList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const recIntList >::type ref(refSEXP);
    Rcpp::traits::input_parameter< bool >::type strict(strictSEXP);
    rcpp_result_gen = Rcpp::wrap(C_minimal(x, ref, strict));
    return rcpp_result_gen;
END_RCPP
}
// C_rowSubsetColAnys
LogicalVector C_rowSubsetColAnys(const LogicalMatrix x, const IntegerVector rows);
RcppExport SEXP _cna_C_rowSubsetColAnys(SEXP xSEXP, SEXP rowsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type rows(rowsSEXP);
    rcpp_result_gen = Rcpp::wrap(C_rowSubsetColAnys(x, rows));
    return rcpp_result_gen;
END_RCPP
}
// C_checkHall_k
bool C_checkHall_k(const LogicalMatrix x, const int k);
RcppExport SEXP _cna_C_checkHall_k(SEXP xSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(C_checkHall_k(x, k));
    return rcpp_result_gen;
END_RCPP
}
// C_checkHallsCondition
bool C_checkHallsCondition(const LogicalMatrix x);
RcppExport SEXP _cna_C_checkHallsCondition(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const LogicalMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_checkHallsCondition(x));
    return rcpp_result_gen;
END_RCPP
}
// C_intList_minimal_old
bool C_intList_minimal_old(const intList x, const recIntList ref, bool ignore_equals);
RcppExport SEXP _cna_C_intList_minimal_old(SEXP xSEXP, SEXP refSEXP, SEXP ignore_equalsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const intList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const recIntList >::type ref(refSEXP);
    Rcpp::traits::input_parameter< bool >::type ignore_equals(ignore_equalsSEXP);
    rcpp_result_gen = Rcpp::wrap(C_intList_minimal_old(x, ref, ignore_equals));
    return rcpp_result_gen;
END_RCPP
}
// C_minimal_old
LogicalVector C_minimal_old(const recIntList x, const recIntList ref, bool ignore_equals);
RcppExport SEXP _cna_C_minimal_old(SEXP xSEXP, SEXP refSEXP, SEXP ignore_equalsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const recIntList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const recIntList >::type ref(refSEXP);
    Rcpp::traits::input_parameter< bool >::type ignore_equals(ignore_equalsSEXP);
    rcpp_result_gen = Rcpp::wrap(C_minimal_old(x, ref, ignore_equals));
    return rcpp_result_gen;
END_RCPP
}
// C_countUniques
IntegerVector C_countUniques(const IntegerMatrix x);
RcppExport SEXP _cna_C_countUniques(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_countUniques(x));
    return rcpp_result_gen;
END_RCPP
}
// C_duplicatedMat
LogicalVector C_duplicatedMat(const IntegerMatrix x);
RcppExport SEXP _cna_C_duplicatedMat(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_duplicatedMat(x));
    return rcpp_result_gen;
END_RCPP
}
// C_uniqueMat
IntegerMatrix C_uniqueMat(const IntegerMatrix x);
RcppExport SEXP _cna_C_uniqueMat(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_uniqueMat(x));
    return rcpp_result_gen;
END_RCPP
}
// C_selectCols
IntegerMatrix C_selectCols(const IntegerMatrix x, const IntegerVector idx);
RcppExport SEXP _cna_C_selectCols(SEXP xSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(C_selectCols(x, idx));
    return rcpp_result_gen;
END_RCPP
}
// C_uniqueCombs
IntegerMatrix C_uniqueCombs(const IntegerMatrix x, const IntegerVector idx);
RcppExport SEXP _cna_C_uniqueCombs(SEXP xSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(C_uniqueCombs(x, idx));
    return rcpp_result_gen;
END_RCPP
}
// C_isSubsetOf
bool C_isSubsetOf(const IntegerVector x, const IntegerVector y);
RcppExport SEXP _cna_C_isSubsetOf(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(C_isSubsetOf(x, y));
    return rcpp_result_gen;
END_RCPP
}
// intList_equal
bool intList_equal(const intList x, const intList y);
RcppExport SEXP _cna_intList_equal(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const intList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const intList >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(intList_equal(x, y));
    return rcpp_result_gen;
END_RCPP
}
// C_hasSupersetIn
LogicalVector C_hasSupersetIn(const intList x, const intList y, const bool ignore_equals);
RcppExport SEXP _cna_C_hasSupersetIn(SEXP xSEXP, SEXP ySEXP, SEXP ignore_equalsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const intList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const intList >::type y(ySEXP);
    Rcpp::traits::input_parameter< const bool >::type ignore_equals(ignore_equalsSEXP);
    rcpp_result_gen = Rcpp::wrap(C_hasSupersetIn(x, y, ignore_equals));
    return rcpp_result_gen;
END_RCPP
}
// C_hasSubsetInM
LogicalVector C_hasSubsetInM(const IntegerMatrix y, const IntegerMatrix x);
RcppExport SEXP _cna_C_hasSubsetInM(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< const IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_hasSubsetInM(y, x));
    return rcpp_result_gen;
END_RCPP
}
// C_append_intList
intList C_append_intList(const intList x, const intList y);
RcppExport SEXP _cna_C_append_intList(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const intList >::type x(xSEXP);
    Rcpp::traits::input_parameter< const intList >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(C_append_intList(x, y));
    return rcpp_result_gen;
END_RCPP
}