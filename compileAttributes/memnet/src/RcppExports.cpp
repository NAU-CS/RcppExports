// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// set
std::vector<std::string> set(std::vector<std::string> v);
RcppExport SEXP _memnet_set(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(set(v));
    return rcpp_result_gen;
END_RCPP
}
// mset
std::vector<std::string> mset(GenericVector dat);
RcppExport SEXP _memnet_mset(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(mset(dat));
    return rcpp_result_gen;
END_RCPP
}
// indx
int indx(std::string s, std::vector<std::string> set);
RcppExport SEXP _memnet_indx(SEXP sSEXP, SEXP setSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type set(setSEXP);
    rcpp_result_gen = Rcpp::wrap(indx(s, set));
    return rcpp_result_gen;
END_RCPP
}
// lags
GenericVector lags(GenericVector dat, int l, bool na_rm);
RcppExport SEXP _memnet_lags(SEXP datSEXP, SEXP lSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(lags(dat, l, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// strsplit
std::vector<std::string> strsplit(const std::string& s, const std::string& delim);
RcppExport SEXP _memnet_strsplit(SEXP sSEXP, SEXP delimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type s(sSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type delim(delimSEXP);
    rcpp_result_gen = Rcpp::wrap(strsplit(s, delim));
    return rcpp_result_gen;
END_RCPP
}
// getinds
NumericMatrix getinds(std::vector<std::string> pairs, std::vector<std::string> unis);
RcppExport SEXP _memnet_getinds(SEXP pairsSEXP, SEXP unisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type pairs(pairsSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type unis(unisSEXP);
    rcpp_result_gen = Rcpp::wrap(getinds(pairs, unis));
    return rcpp_result_gen;
END_RCPP
}
// getpairs
CharacterMatrix getpairs(std::vector<std::string> spairs, std::string del);
RcppExport SEXP _memnet_getpairs(SEXP spairsSEXP, SEXP delSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type spairs(spairsSEXP);
    Rcpp::traits::input_parameter< std::string >::type del(delSEXP);
    rcpp_result_gen = Rcpp::wrap(getpairs(spairs, del));
    return rcpp_result_gen;
END_RCPP
}
// count
std::vector<int> count(std::vector<std::string> v);
RcppExport SEXP _memnet_count(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(count(v));
    return rcpp_result_gen;
END_RCPP
}
// range
std::vector<int> range(int n);
RcppExport SEXP _memnet_range(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(range(n));
    return rcpp_result_gen;
END_RCPP
}
// get_indices
std::vector<int> get_indices(int n, int use);
RcppExport SEXP _memnet_get_indices(SEXP nSEXP, SEXP useSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type use(useSEXP);
    rcpp_result_gen = Rcpp::wrap(get_indices(n, use));
    return rcpp_result_gen;
END_RCPP
}
// cut_stringvec
std::vector<std::string> cut_stringvec(std::vector<std::string> items, std::vector<int> indices);
RcppExport SEXP _memnet_cut_stringvec(SEXP itemsSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type items(itemsSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(cut_stringvec(items, indices));
    return rcpp_result_gen;
END_RCPP
}
// cut_dat
GenericVector cut_dat(GenericVector dat, GenericVector inds, std::vector<int> indices);
RcppExport SEXP _memnet_cut_dat(SEXP datSEXP, SEXP indsSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< GenericVector >::type inds(indsSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(cut_dat(dat, inds, indices));
    return rcpp_result_gen;
END_RCPP
}
// mcount
std::vector<int> mcount(GenericVector dat);
RcppExport SEXP _memnet_mcount(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(mcount(dat));
    return rcpp_result_gen;
END_RCPP
}
// getprob
NumericVector getprob(std::vector<int> counts, double N);
RcppExport SEXP _memnet_getprob(SEXP countsSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type counts(countsSEXP);
    Rcpp::traits::input_parameter< double >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(getprob(counts, N));
    return rcpp_result_gen;
END_RCPP
}
// pinwin
double pinwin(double n, double l);
RcppExport SEXP _memnet_pinwin(SEXP nSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(pinwin(n, l));
    return rcpp_result_gen;
END_RCPP
}
// mpinwin
double mpinwin(NumericVector ns, double l);
RcppExport SEXP _memnet_mpinwin(SEXP nsSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type ns(nsSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(mpinwin(ns, l));
    return rcpp_result_gen;
END_RCPP
}
// lens
NumericVector lens(GenericVector dat);
RcppExport SEXP _memnet_lens(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(lens(dat));
    return rcpp_result_gen;
END_RCPP
}
// mlength
double mlength(GenericVector dat);
RcppExport SEXP _memnet_mlength(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(mlength(dat));
    return rcpp_result_gen;
END_RCPP
}
// getplink
NumericVector getplink(NumericMatrix inds, NumericVector probs, double pinwin);
RcppExport SEXP _memnet_getplink(SEXP indsSEXP, SEXP probsSEXP, SEXP pinwinSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type inds(indsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< double >::type pinwin(pinwinSEXP);
    rcpp_result_gen = Rcpp::wrap(getplink(inds, probs, pinwin));
    return rcpp_result_gen;
END_RCPP
}
// dbinom
double dbinom(int k, int n, double p);
RcppExport SEXP _memnet_dbinom(SEXP kSEXP, SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(dbinom(k, n, p));
    return rcpp_result_gen;
END_RCPP
}
// pbinom
double pbinom(int k, int n, double p);
RcppExport SEXP _memnet_pbinom(SEXP kSEXP, SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(pbinom(k, n, p));
    return rcpp_result_gen;
END_RCPP
}
// community_graph
CharacterMatrix community_graph(GenericVector dat, int l, int min_cooc, double crit);
RcppExport SEXP _memnet_community_graph(SEXP datSEXP, SEXP lSEXP, SEXP min_coocSEXP, SEXP critSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    Rcpp::traits::input_parameter< int >::type min_cooc(min_coocSEXP);
    Rcpp::traits::input_parameter< double >::type crit(critSEXP);
    rcpp_result_gen = Rcpp::wrap(community_graph(dat, l, min_cooc, crit));
    return rcpp_result_gen;
END_RCPP
}
// rw_graph
CharacterMatrix rw_graph(GenericVector dat);
RcppExport SEXP _memnet_rw_graph(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(rw_graph(dat));
    return rcpp_result_gen;
END_RCPP
}
// threshold_graph
CharacterMatrix threshold_graph(GenericVector dat, int min_cooc);
RcppExport SEXP _memnet_threshold_graph(SEXP datSEXP, SEXP min_coocSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< int >::type min_cooc(min_coocSEXP);
    rcpp_result_gen = Rcpp::wrap(threshold_graph(dat, min_cooc));
    return rcpp_result_gen;
END_RCPP
}
// shuffle
std::vector<int> shuffle(std::vector<int>& v);
RcppExport SEXP _memnet_shuffle(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int>& >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(shuffle(v));
    return rcpp_result_gen;
END_RCPP
}
// seed
NumericMatrix seed(int n, int m);
RcppExport SEXP _memnet_seed(SEXP nSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(seed(n, m));
    return rcpp_result_gen;
END_RCPP
}
// sm
int sm(NumericVector x);
RcppExport SEXP _memnet_sm(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sm(x));
    return rcpp_result_gen;
END_RCPP
}
// getdegrees
std::vector<int> getdegrees(NumericMatrix adj, int pos);
RcppExport SEXP _memnet_getdegrees(SEXP adjSEXP, SEXP posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type pos(posSEXP);
    rcpp_result_gen = Rcpp::wrap(getdegrees(adj, pos));
    return rcpp_result_gen;
END_RCPP
}
// getnonneighbors
std::vector<int> getnonneighbors(NumericMatrix adj, int node);
RcppExport SEXP _memnet_getnonneighbors(SEXP adjSEXP, SEXP nodeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type node(nodeSEXP);
    rcpp_result_gen = Rcpp::wrap(getnonneighbors(adj, node));
    return rcpp_result_gen;
END_RCPP
}
// randint
int randint(int n);
RcppExport SEXP _memnet_randint(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(randint(n));
    return rcpp_result_gen;
END_RCPP
}
// selectnode
int selectnode(std::vector<int> ps);
RcppExport SEXP _memnet_selectnode(SEXP psSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type ps(psSEXP);
    rcpp_result_gen = Rcpp::wrap(selectnode(ps));
    return rcpp_result_gen;
END_RCPP
}
// selectnode_power
int selectnode_power(std::vector<int> ps, double power);
RcppExport SEXP _memnet_selectnode_power(SEXP psSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type ps(psSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(selectnode_power(ps, power));
    return rcpp_result_gen;
END_RCPP
}
// grow_st
NumericMatrix grow_st(int n, int m);
RcppExport SEXP _memnet_grow_st(SEXP nSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(grow_st(n, m));
    return rcpp_result_gen;
END_RCPP
}
// emptyseed
NumericMatrix emptyseed(int n);
RcppExport SEXP _memnet_emptyseed(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(emptyseed(n));
    return rcpp_result_gen;
END_RCPP
}
// puni
double puni();
RcppExport SEXP _memnet_puni() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(puni());
    return rcpp_result_gen;
END_RCPP
}
// unconnectedneighbor
int unconnectedneighbor(NumericMatrix adj, int from, int to);
RcppExport SEXP _memnet_unconnectedneighbor(SEXP adjSEXP, SEXP fromSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type from(fromSEXP);
    Rcpp::traits::input_parameter< int >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(unconnectedneighbor(adj, from, to));
    return rcpp_result_gen;
END_RCPP
}
// grow_hk
NumericMatrix grow_hk(int n, int m, double p);
RcppExport SEXP _memnet_grow_hk(SEXP nSEXP, SEXP mSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(grow_hk(n, m, p));
    return rcpp_result_gen;
END_RCPP
}
// grow_ba
NumericMatrix grow_ba(int n, int m, double power);
RcppExport SEXP _memnet_grow_ba(SEXP nSEXP, SEXP mSEXP, SEXP powerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type power(powerSEXP);
    rcpp_result_gen = Rcpp::wrap(grow_ba(n, m, power));
    return rcpp_result_gen;
END_RCPP
}
// grow_ws
IntegerMatrix grow_ws(int n, int k, double p);
RcppExport SEXP _memnet_grow_ws(SEXP nSEXP, SEXP kSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(grow_ws(n, k, p));
    return rcpp_result_gen;
END_RCPP
}
// grow_lattice
IntegerMatrix grow_lattice(int n, int k);
RcppExport SEXP _memnet_grow_lattice(SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(grow_lattice(n, k));
    return rcpp_result_gen;
END_RCPP
}
// unique_int
std::vector<int> unique_int(std::vector<int> v);
RcppExport SEXP _memnet_unique_int(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(unique_int(v));
    return rcpp_result_gen;
END_RCPP
}
// runi
double runi();
RcppExport SEXP _memnet_runi() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(runi());
    return rcpp_result_gen;
END_RCPP
}
// rint
int rint(int n);
RcppExport SEXP _memnet_rint(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rint(n));
    return rcpp_result_gen;
END_RCPP
}
// get_adjlist
GenericVector get_adjlist(NumericMatrix adj);
RcppExport SEXP _memnet_get_adjlist(SEXP adjSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    rcpp_result_gen = Rcpp::wrap(get_adjlist(adj));
    return rcpp_result_gen;
END_RCPP
}
// get_neighborhood
NumericMatrix get_neighborhood(NumericMatrix adj, int start, int k);
RcppExport SEXP _memnet_get_neighborhood(SEXP adjSEXP, SEXP startSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(get_neighborhood(adj, start, k));
    return rcpp_result_gen;
END_RCPP
}
// get_kneighbors
std::vector<int> get_kneighbors(NumericMatrix adj, int start, int k);
RcppExport SEXP _memnet_get_kneighbors(SEXP adjSEXP, SEXP startSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(get_kneighbors(adj, start, k));
    return rcpp_result_gen;
END_RCPP
}
// prbs
double prbs(std::vector<double> x, double p);
RcppExport SEXP _memnet_prbs(SEXP xSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(prbs(x, p));
    return rcpp_result_gen;
END_RCPP
}
// trm
double trm(std::vector<double> x, std::vector<double> y);
RcppExport SEXP _memnet_trm(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(trm(x, y));
    return rcpp_result_gen;
END_RCPP
}
// get_names_c
std::vector<std::string> get_names_c(CharacterMatrix& edg);
RcppExport SEXP _memnet_get_names_c(SEXP edgSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterMatrix& >::type edg(edgSEXP);
    rcpp_result_gen = Rcpp::wrap(get_names_c(edg));
    return rcpp_result_gen;
END_RCPP
}
// get_names_i
std::vector<int> get_names_i(IntegerMatrix& edg);
RcppExport SEXP _memnet_get_names_i(SEXP edgSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix& >::type edg(edgSEXP);
    rcpp_result_gen = Rcpp::wrap(get_names_i(edg));
    return rcpp_result_gen;
END_RCPP
}
// noverk
double noverk(int n, int k);
RcppExport SEXP _memnet_noverk(SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(noverk(n, k));
    return rcpp_result_gen;
END_RCPP
}
// smpl
int smpl(std::vector<double> ps);
RcppExport SEXP _memnet_smpl(SEXP psSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type ps(psSEXP);
    rcpp_result_gen = Rcpp::wrap(smpl(ps));
    return rcpp_result_gen;
END_RCPP
}
// my_to_string
std::string my_to_string(int value);
RcppExport SEXP _memnet_my_to_string(SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(my_to_string(value));
    return rcpp_result_gen;
END_RCPP
}
// to_str
Rcpp::CharacterVector to_str(std::vector<int> items);
RcppExport SEXP _memnet_to_str(SEXP itemsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type items(itemsSEXP);
    rcpp_result_gen = Rcpp::wrap(to_str(items));
    return rcpp_result_gen;
END_RCPP
}
// getneighbors
std::vector<int> getneighbors(GenericVector adjlist, int pos);
RcppExport SEXP _memnet_getneighbors(SEXP adjlistSEXP, SEXP posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adjlist(adjlistSEXP);
    Rcpp::traits::input_parameter< int >::type pos(posSEXP);
    rcpp_result_gen = Rcpp::wrap(getneighbors(adjlist, pos));
    return rcpp_result_gen;
END_RCPP
}
// getnext
int getnext(std::vector<int> neighbors);
RcppExport SEXP _memnet_getnext(SEXP neighborsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type neighbors(neighborsSEXP);
    rcpp_result_gen = Rcpp::wrap(getnext(neighbors));
    return rcpp_result_gen;
END_RCPP
}
// unicut
std::vector<int> unicut(std::vector<int> vs, int n);
RcppExport SEXP _memnet_unicut(SEXP vsSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type vs(vsSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(unicut(vs, n));
    return rcpp_result_gen;
END_RCPP
}
// adjlist_minus1
GenericVector adjlist_minus1(GenericVector& adjlist);
RcppExport SEXP _memnet_adjlist_minus1(SEXP adjlistSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector& >::type adjlist(adjlistSEXP);
    rcpp_result_gen = Rcpp::wrap(adjlist_minus1(adjlist));
    return rcpp_result_gen;
END_RCPP
}
// add_1
void add_1(std::vector<int>& items);
RcppExport SEXP _memnet_add_1(SEXP itemsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int>& >::type items(itemsSEXP);
    add_1(items);
    return R_NilValue;
END_RCPP
}
// one_fluency
std::vector<int> one_fluency(GenericVector adj_list, int n, double pjump, int type);
RcppExport SEXP _memnet_one_fluency(SEXP adj_listSEXP, SEXP nSEXP, SEXP pjumpSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adj_list(adj_listSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(one_fluency(adj_list, n, pjump, type));
    return rcpp_result_gen;
END_RCPP
}
// fluency
GenericVector fluency(GenericVector adjlist, NumericVector n, double pjump, int type, bool string);
RcppExport SEXP _memnet_fluency(SEXP adjlistSEXP, SEXP nSEXP, SEXP pjumpSEXP, SEXP typeSEXP, SEXP stringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adjlist(adjlistSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    Rcpp::traits::input_parameter< bool >::type string(stringSEXP);
    rcpp_result_gen = Rcpp::wrap(fluency(adjlist, n, pjump, type, string));
    return rcpp_result_gen;
END_RCPP
}
// one_ffluency
std::vector<int> one_ffluency(GenericVector adj_list, int n, double pjump, int type);
RcppExport SEXP _memnet_one_ffluency(SEXP adj_listSEXP, SEXP nSEXP, SEXP pjumpSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adj_list(adj_listSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(one_ffluency(adj_list, n, pjump, type));
    return rcpp_result_gen;
END_RCPP
}
// ffluency
GenericVector ffluency(GenericVector adjlist, NumericVector n, double pjump, int type, bool string);
RcppExport SEXP _memnet_ffluency(SEXP adjlistSEXP, SEXP nSEXP, SEXP pjumpSEXP, SEXP typeSEXP, SEXP stringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adjlist(adjlistSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    Rcpp::traits::input_parameter< bool >::type string(stringSEXP);
    rcpp_result_gen = Rcpp::wrap(ffluency(adjlist, n, pjump, type, string));
    return rcpp_result_gen;
END_RCPP
}
// one_fluency_steps
int one_fluency_steps(GenericVector adj_list, int n, double pjump, int type);
RcppExport SEXP _memnet_one_fluency_steps(SEXP adj_listSEXP, SEXP nSEXP, SEXP pjumpSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adj_list(adj_listSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(one_fluency_steps(adj_list, n, pjump, type));
    return rcpp_result_gen;
END_RCPP
}
// fluency_steps
std::vector<int> fluency_steps(GenericVector adjlist, NumericVector n, double pjump, int type);
RcppExport SEXP _memnet_fluency_steps(SEXP adjlistSEXP, SEXP nSEXP, SEXP pjumpSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adjlist(adjlistSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(fluency_steps(adjlist, n, pjump, type));
    return rcpp_result_gen;
END_RCPP
}
// one_search
NumericMatrix one_search(GenericVector adj_list, int start, std::vector<int> observe, int nmax, double pjump, int type);
RcppExport SEXP _memnet_one_search(SEXP adj_listSEXP, SEXP startSEXP, SEXP observeSEXP, SEXP nmaxSEXP, SEXP pjumpSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adj_list(adj_listSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type observe(observeSEXP);
    Rcpp::traits::input_parameter< int >::type nmax(nmaxSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(one_search(adj_list, start, observe, nmax, pjump, type));
    return rcpp_result_gen;
END_RCPP
}
// search_rw
NumericMatrix search_rw(GenericVector adjlist, std::vector<int> start, std::vector<int> observe, int nmax, double pjump, int type);
RcppExport SEXP _memnet_search_rw(SEXP adjlistSEXP, SEXP startSEXP, SEXP observeSEXP, SEXP nmaxSEXP, SEXP pjumpSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adjlist(adjlistSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type start(startSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type observe(observeSEXP);
    Rcpp::traits::input_parameter< int >::type nmax(nmaxSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(search_rw(adjlist, start, observe, nmax, pjump, type));
    return rcpp_result_gen;
END_RCPP
}
// search_rw_mean
NumericMatrix search_rw_mean(GenericVector adjlist, std::vector<int> start, std::vector<int> observe, int nmax, double pjump, int type, int nrep);
RcppExport SEXP _memnet_search_rw_mean(SEXP adjlistSEXP, SEXP startSEXP, SEXP observeSEXP, SEXP nmaxSEXP, SEXP pjumpSEXP, SEXP typeSEXP, SEXP nrepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< GenericVector >::type adjlist(adjlistSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type start(startSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type observe(observeSEXP);
    Rcpp::traits::input_parameter< int >::type nmax(nmaxSEXP);
    Rcpp::traits::input_parameter< double >::type pjump(pjumpSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    Rcpp::traits::input_parameter< int >::type nrep(nrepSEXP);
    rcpp_result_gen = Rcpp::wrap(search_rw_mean(adjlist, start, observe, nmax, pjump, type, nrep));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_memnet_set", (DL_FUNC) &_memnet_set, 1},
    {"_memnet_mset", (DL_FUNC) &_memnet_mset, 1},
    {"_memnet_indx", (DL_FUNC) &_memnet_indx, 2},
    {"_memnet_lags", (DL_FUNC) &_memnet_lags, 3},
    {"_memnet_strsplit", (DL_FUNC) &_memnet_strsplit, 2},
    {"_memnet_getinds", (DL_FUNC) &_memnet_getinds, 2},
    {"_memnet_getpairs", (DL_FUNC) &_memnet_getpairs, 2},
    {"_memnet_count", (DL_FUNC) &_memnet_count, 1},
    {"_memnet_range", (DL_FUNC) &_memnet_range, 1},
    {"_memnet_get_indices", (DL_FUNC) &_memnet_get_indices, 2},
    {"_memnet_cut_stringvec", (DL_FUNC) &_memnet_cut_stringvec, 2},
    {"_memnet_cut_dat", (DL_FUNC) &_memnet_cut_dat, 3},
    {"_memnet_mcount", (DL_FUNC) &_memnet_mcount, 1},
    {"_memnet_getprob", (DL_FUNC) &_memnet_getprob, 2},
    {"_memnet_pinwin", (DL_FUNC) &_memnet_pinwin, 2},
    {"_memnet_mpinwin", (DL_FUNC) &_memnet_mpinwin, 2},
    {"_memnet_lens", (DL_FUNC) &_memnet_lens, 1},
    {"_memnet_mlength", (DL_FUNC) &_memnet_mlength, 1},
    {"_memnet_getplink", (DL_FUNC) &_memnet_getplink, 3},
    {"_memnet_dbinom", (DL_FUNC) &_memnet_dbinom, 3},
    {"_memnet_pbinom", (DL_FUNC) &_memnet_pbinom, 3},
    {"_memnet_community_graph", (DL_FUNC) &_memnet_community_graph, 4},
    {"_memnet_rw_graph", (DL_FUNC) &_memnet_rw_graph, 1},
    {"_memnet_threshold_graph", (DL_FUNC) &_memnet_threshold_graph, 2},
    {"_memnet_shuffle", (DL_FUNC) &_memnet_shuffle, 1},
    {"_memnet_seed", (DL_FUNC) &_memnet_seed, 2},
    {"_memnet_sm", (DL_FUNC) &_memnet_sm, 1},
    {"_memnet_getdegrees", (DL_FUNC) &_memnet_getdegrees, 2},
    {"_memnet_getnonneighbors", (DL_FUNC) &_memnet_getnonneighbors, 2},
    {"_memnet_randint", (DL_FUNC) &_memnet_randint, 1},
    {"_memnet_selectnode", (DL_FUNC) &_memnet_selectnode, 1},
    {"_memnet_selectnode_power", (DL_FUNC) &_memnet_selectnode_power, 2},
    {"_memnet_grow_st", (DL_FUNC) &_memnet_grow_st, 2},
    {"_memnet_emptyseed", (DL_FUNC) &_memnet_emptyseed, 1},
    {"_memnet_puni", (DL_FUNC) &_memnet_puni, 0},
    {"_memnet_unconnectedneighbor", (DL_FUNC) &_memnet_unconnectedneighbor, 3},
    {"_memnet_grow_hk", (DL_FUNC) &_memnet_grow_hk, 3},
    {"_memnet_grow_ba", (DL_FUNC) &_memnet_grow_ba, 3},
    {"_memnet_grow_ws", (DL_FUNC) &_memnet_grow_ws, 3},
    {"_memnet_grow_lattice", (DL_FUNC) &_memnet_grow_lattice, 2},
    {"_memnet_unique_int", (DL_FUNC) &_memnet_unique_int, 1},
    {"_memnet_runi", (DL_FUNC) &_memnet_runi, 0},
    {"_memnet_rint", (DL_FUNC) &_memnet_rint, 1},
    {"_memnet_get_adjlist", (DL_FUNC) &_memnet_get_adjlist, 1},
    {"_memnet_get_neighborhood", (DL_FUNC) &_memnet_get_neighborhood, 3},
    {"_memnet_get_kneighbors", (DL_FUNC) &_memnet_get_kneighbors, 3},
    {"_memnet_prbs", (DL_FUNC) &_memnet_prbs, 2},
    {"_memnet_trm", (DL_FUNC) &_memnet_trm, 2},
    {"_memnet_get_names_c", (DL_FUNC) &_memnet_get_names_c, 1},
    {"_memnet_get_names_i", (DL_FUNC) &_memnet_get_names_i, 1},
    {"_memnet_noverk", (DL_FUNC) &_memnet_noverk, 2},
    {"_memnet_smpl", (DL_FUNC) &_memnet_smpl, 1},
    {"_memnet_my_to_string", (DL_FUNC) &_memnet_my_to_string, 1},
    {"_memnet_to_str", (DL_FUNC) &_memnet_to_str, 1},
    {"_memnet_getneighbors", (DL_FUNC) &_memnet_getneighbors, 2},
    {"_memnet_getnext", (DL_FUNC) &_memnet_getnext, 1},
    {"_memnet_unicut", (DL_FUNC) &_memnet_unicut, 2},
    {"_memnet_adjlist_minus1", (DL_FUNC) &_memnet_adjlist_minus1, 1},
    {"_memnet_add_1", (DL_FUNC) &_memnet_add_1, 1},
    {"_memnet_one_fluency", (DL_FUNC) &_memnet_one_fluency, 4},
    {"_memnet_fluency", (DL_FUNC) &_memnet_fluency, 5},
    {"_memnet_one_ffluency", (DL_FUNC) &_memnet_one_ffluency, 4},
    {"_memnet_ffluency", (DL_FUNC) &_memnet_ffluency, 5},
    {"_memnet_one_fluency_steps", (DL_FUNC) &_memnet_one_fluency_steps, 4},
    {"_memnet_fluency_steps", (DL_FUNC) &_memnet_fluency_steps, 4},
    {"_memnet_one_search", (DL_FUNC) &_memnet_one_search, 6},
    {"_memnet_search_rw", (DL_FUNC) &_memnet_search_rw, 6},
    {"_memnet_search_rw_mean", (DL_FUNC) &_memnet_search_rw_mean, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_memnet(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
