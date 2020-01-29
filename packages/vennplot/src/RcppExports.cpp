// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// loop_R
List loop_R(NumericMatrix xy, double lambda, NumericVector radius, NumericMatrix ED, bool ThreeD, double ToleranceofLoss, int maximumStep, double ToleranceofStepsize, bool proportional, double ALPHA, bool Bool);
RcppExport SEXP vennplot_loop_R(SEXP xySEXP, SEXP lambdaSEXP, SEXP radiusSEXP, SEXP EDSEXP, SEXP ThreeDSEXP, SEXP ToleranceofLossSEXP, SEXP maximumStepSEXP, SEXP ToleranceofStepsizeSEXP, SEXP proportionalSEXP, SEXP ALPHASEXP, SEXP BoolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type xy(xySEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ED(EDSEXP);
    Rcpp::traits::input_parameter< bool >::type ThreeD(ThreeDSEXP);
    Rcpp::traits::input_parameter< double >::type ToleranceofLoss(ToleranceofLossSEXP);
    Rcpp::traits::input_parameter< int >::type maximumStep(maximumStepSEXP);
    Rcpp::traits::input_parameter< double >::type ToleranceofStepsize(ToleranceofStepsizeSEXP);
    Rcpp::traits::input_parameter< bool >::type proportional(proportionalSEXP);
    Rcpp::traits::input_parameter< double >::type ALPHA(ALPHASEXP);
    Rcpp::traits::input_parameter< bool >::type Bool(BoolSEXP);
    rcpp_result_gen = Rcpp::wrap(loop_R(xy, lambda, radius, ED, ThreeD, ToleranceofLoss, maximumStep, ToleranceofStepsize, proportional, ALPHA, Bool));
    return rcpp_result_gen;
END_RCPP
}
// trans_R
NumericVector trans_R(NumericMatrix xy, NumericVector radius, double radiusvec, NumericVector radiusall);
RcppExport SEXP vennplot_trans_R(SEXP xySEXP, SEXP radiusSEXP, SEXP radiusvecSEXP, SEXP radiusallSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type xy(xySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< double >::type radiusvec(radiusvecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radiusall(radiusallSEXP);
    rcpp_result_gen = Rcpp::wrap(trans_R(xy, radius, radiusvec, radiusall));
    return rcpp_result_gen;
END_RCPP
}
// alldis_R
int alldis_R(NumericMatrix xy1, NumericMatrix xy2, NumericVector radius1, NumericVector radius2, double delta);
RcppExport SEXP vennplot_alldis_R(SEXP xy1SEXP, SEXP xy2SEXP, SEXP radius1SEXP, SEXP radius2SEXP, SEXP deltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type xy1(xy1SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type xy2(xy2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius1(radius1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius2(radius2SEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    rcpp_result_gen = Rcpp::wrap(alldis_R(xy1, xy2, radius1, radius2, delta));
    return rcpp_result_gen;
END_RCPP
}
// close_R
List close_R(NumericMatrix xy1, NumericMatrix xy2, NumericVector radius1, NumericVector radius2, double delta, NumericVector direc);
RcppExport SEXP vennplot_close_R(SEXP xy1SEXP, SEXP xy2SEXP, SEXP radius1SEXP, SEXP radius2SEXP, SEXP deltaSEXP, SEXP direcSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type xy1(xy1SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type xy2(xy2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius1(radius1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius2(radius2SEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type direc(direcSEXP);
    rcpp_result_gen = Rcpp::wrap(close_R(xy1, xy2, radius1, radius2, delta, direc));
    return rcpp_result_gen;
END_RCPP
}
// binaryIndexCpp
NumericMatrix binaryIndexCpp(NumericMatrix M, NumericMatrix xy, NumericVector radius, int k, double yuan, double xuan, int num);
RcppExport SEXP vennplot_binaryIndexCpp(SEXP MSEXP, SEXP xySEXP, SEXP radiusSEXP, SEXP kSEXP, SEXP yuanSEXP, SEXP xuanSEXP, SEXP numSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type xy(xySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type yuan(yuanSEXP);
    Rcpp::traits::input_parameter< double >::type xuan(xuanSEXP);
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    rcpp_result_gen = Rcpp::wrap(binaryIndexCpp(M, xy, radius, k, yuan, xuan, num));
    return rcpp_result_gen;
END_RCPP
}
// goThroughPixelCpp
NumericMatrix goThroughPixelCpp(Rcpp::List myList, int m, int num);
RcppExport SEXP vennplot_goThroughPixelCpp(SEXP myListSEXP, SEXP mSEXP, SEXP numSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type myList(myListSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    rcpp_result_gen = Rcpp::wrap(goThroughPixelCpp(myList, m, num));
    return rcpp_result_gen;
END_RCPP
}
// countCpp
NumericVector countCpp(NumericMatrix M, NumericMatrix Me);
RcppExport SEXP vennplot_countCpp(SEXP MSEXP, SEXP MeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Me(MeSEXP);
    rcpp_result_gen = Rcpp::wrap(countCpp(M, Me));
    return rcpp_result_gen;
END_RCPP
}
// getRidofZeroCpp
NumericVector getRidofZeroCpp(NumericMatrix M);
RcppExport SEXP vennplot_getRidofZeroCpp(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(getRidofZeroCpp(M));
    return rcpp_result_gen;
END_RCPP
}
// binaryIndexThreeDCpp
List binaryIndexThreeDCpp(Rcpp::List myList, NumericMatrix xy, NumericVector radius, int k, double yuan, double xuan, double zuan, int num);
RcppExport SEXP vennplot_binaryIndexThreeDCpp(SEXP myListSEXP, SEXP xySEXP, SEXP radiusSEXP, SEXP kSEXP, SEXP yuanSEXP, SEXP xuanSEXP, SEXP zuanSEXP, SEXP numSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type myList(myListSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type xy(xySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type yuan(yuanSEXP);
    Rcpp::traits::input_parameter< double >::type xuan(xuanSEXP);
    Rcpp::traits::input_parameter< double >::type zuan(zuanSEXP);
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    rcpp_result_gen = Rcpp::wrap(binaryIndexThreeDCpp(myList, xy, radius, k, yuan, xuan, zuan, num));
    return rcpp_result_gen;
END_RCPP
}
// goThroughPixelThreeDCpp
NumericMatrix goThroughPixelThreeDCpp(Rcpp::List list, int m, int num);
RcppExport SEXP vennplot_goThroughPixelThreeDCpp(SEXP listSEXP, SEXP mSEXP, SEXP numSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type list(listSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    rcpp_result_gen = Rcpp::wrap(goThroughPixelThreeDCpp(list, m, num));
    return rcpp_result_gen;
END_RCPP
}
// allConnectedCpp
bool allConnectedCpp(NumericMatrix xy, NumericVector radius, bool ThreeD);
RcppExport SEXP vennplot_allConnectedCpp(SEXP xySEXP, SEXP radiusSEXP, SEXP ThreeDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type xy(xySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< bool >::type ThreeD(ThreeDSEXP);
    rcpp_result_gen = Rcpp::wrap(allConnectedCpp(xy, radius, ThreeD));
    return rcpp_result_gen;
END_RCPP
}
// distanceCpp
NumericMatrix distanceCpp(double r1, double r2, NumericVector theta1, NumericVector theta2, double S, bool ThreeD);
RcppExport SEXP vennplot_distanceCpp(SEXP r1SEXP, SEXP r2SEXP, SEXP theta1SEXP, SEXP theta2SEXP, SEXP SSEXP, SEXP ThreeDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< double >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta1(theta1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta2(theta2SEXP);
    Rcpp::traits::input_parameter< double >::type S(SSEXP);
    Rcpp::traits::input_parameter< bool >::type ThreeD(ThreeDSEXP);
    rcpp_result_gen = Rcpp::wrap(distanceCpp(r1, r2, theta1, theta2, S, ThreeD));
    return rcpp_result_gen;
END_RCPP
}
// BoolScaleNMCpp
bool BoolScaleNMCpp(bool proportional, double value, NumericVector LAMBDA, NumericVector STRESS);
RcppExport SEXP vennplot_BoolScaleNMCpp(SEXP proportionalSEXP, SEXP valueSEXP, SEXP LAMBDASEXP, SEXP STRESSSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type proportional(proportionalSEXP);
    Rcpp::traits::input_parameter< double >::type value(valueSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type LAMBDA(LAMBDASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type STRESS(STRESSSEXP);
    rcpp_result_gen = Rcpp::wrap(BoolScaleNMCpp(proportional, value, LAMBDA, STRESS));
    return rcpp_result_gen;
END_RCPP
}
// BoolScaleLCpp
bool BoolScaleLCpp(bool proportional, double value, double stress_n, double stress);
RcppExport SEXP vennplot_BoolScaleLCpp(SEXP proportionalSEXP, SEXP valueSEXP, SEXP stress_nSEXP, SEXP stressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type proportional(proportionalSEXP);
    Rcpp::traits::input_parameter< double >::type value(valueSEXP);
    Rcpp::traits::input_parameter< double >::type stress_n(stress_nSEXP);
    Rcpp::traits::input_parameter< double >::type stress(stressSEXP);
    rcpp_result_gen = Rcpp::wrap(BoolScaleLCpp(proportional, value, stress_n, stress));
    return rcpp_result_gen;
END_RCPP
}
// BoolDistanceCpp
bool BoolDistanceCpp(bool proportional, double value, double f1, double f2, NumericMatrix thetanew, NumericMatrix theta);
RcppExport SEXP vennplot_BoolDistanceCpp(SEXP proportionalSEXP, SEXP valueSEXP, SEXP f1SEXP, SEXP f2SEXP, SEXP thetanewSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type proportional(proportionalSEXP);
    Rcpp::traits::input_parameter< double >::type value(valueSEXP);
    Rcpp::traits::input_parameter< double >::type f1(f1SEXP);
    Rcpp::traits::input_parameter< double >::type f2(f2SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type thetanew(thetanewSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(BoolDistanceCpp(proportional, value, f1, f2, thetanew, theta));
    return rcpp_result_gen;
END_RCPP
}