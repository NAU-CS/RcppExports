// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// constituerGrappes
arma::Mat<int> constituerGrappes(const unsigned int iNbObsMin, const arma::Mat<int>& mEffectifs);
RcppExport SEXP btb_constituerGrappes(SEXP iNbObsMinSEXP, SEXP mEffectifsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const unsigned int >::type iNbObsMin(iNbObsMinSEXP);
    Rcpp::traits::input_parameter< const arma::Mat<int>& >::type mEffectifs(mEffectifsSEXP);
    rcpp_result_gen = Rcpp::wrap(constituerGrappes(iNbObsMin, mEffectifs));
    return rcpp_result_gen;
END_RCPP
}
// coordonneesGrappe
std::vector<int> coordonneesGrappe(int iNiveauMax, int iNoGrappe);
RcppExport SEXP btb_coordonneesGrappe(SEXP iNiveauMaxSEXP, SEXP iNoGrappeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type iNiveauMax(iNiveauMaxSEXP);
    Rcpp::traits::input_parameter< int >::type iNoGrappe(iNoGrappeSEXP);
    rcpp_result_gen = Rcpp::wrap(coordonneesGrappe(iNiveauMax, iNoGrappe));
    return rcpp_result_gen;
END_RCPP
}
// constituerMatriceEffectifs
arma::Mat<int> constituerMatriceEffectifs(IntegerVector vLigneObservation, IntegerVector vColonneObservation);
RcppExport SEXP btb_constituerMatriceEffectifs(SEXP vLigneObservationSEXP, SEXP vColonneObservationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type vLigneObservation(vLigneObservationSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vColonneObservation(vColonneObservationSEXP);
    rcpp_result_gen = Rcpp::wrap(constituerMatriceEffectifs(vLigneObservation, vColonneObservation));
    return rcpp_result_gen;
END_RCPP
}
// calculeQuantiles
std::vector<double> calculeQuantiles(std::vector<double>& vModalites, std::vector<double>& vPonderation, const std::vector<double> vQuantiles);
RcppExport SEXP btb_calculeQuantiles(SEXP vModalitesSEXP, SEXP vPonderationSEXP, SEXP vQuantilesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double>& >::type vModalites(vModalitesSEXP);
    Rcpp::traits::input_parameter< std::vector<double>& >::type vPonderation(vPonderationSEXP);
    Rcpp::traits::input_parameter< const std::vector<double> >::type vQuantiles(vQuantilesSEXP);
    rcpp_result_gen = Rcpp::wrap(calculeQuantiles(vModalites, vPonderation, vQuantiles));
    return rcpp_result_gen;
END_RCPP
}
// rcppLissageMedian
arma::mat rcppLissageMedian(std::vector<int> vXobservations, std::vector<int> vYobservations, int iRayon, arma::mat& mVar, std::vector<int> vXCentroides, std::vector<int> vYCentroides, std::vector<double> vQuantiles);
RcppExport SEXP btb_rcppLissageMedian(SEXP vXobservationsSEXP, SEXP vYobservationsSEXP, SEXP iRayonSEXP, SEXP mVarSEXP, SEXP vXCentroidesSEXP, SEXP vYCentroidesSEXP, SEXP vQuantilesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type vXobservations(vXobservationsSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type vYobservations(vYobservationsSEXP);
    Rcpp::traits::input_parameter< int >::type iRayon(iRayonSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type mVar(mVarSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type vXCentroides(vXCentroidesSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type vYCentroides(vYCentroidesSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type vQuantiles(vQuantilesSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppLissageMedian(vXobservations, vYobservations, iRayon, mVar, vXCentroides, vYCentroides, vQuantiles));
    return rcpp_result_gen;
END_RCPP
}
// rcppLissage
NumericMatrix rcppLissage(IntegerVector vXObservation, IntegerVector vYObservation, IntegerVector vLigneObservation, IntegerVector vColonneObservation, int iPas, int iRayon, int iNeighbor, NumericMatrix mVariables, int iNumberCols, int iNumberRows, int iMinXCentroide, int iMinYCentroide, IntegerMatrix mIcentroide, int iNbCentroides, Nullable <Function> updateProgress);
RcppExport SEXP btb_rcppLissage(SEXP vXObservationSEXP, SEXP vYObservationSEXP, SEXP vLigneObservationSEXP, SEXP vColonneObservationSEXP, SEXP iPasSEXP, SEXP iRayonSEXP, SEXP iNeighborSEXP, SEXP mVariablesSEXP, SEXP iNumberColsSEXP, SEXP iNumberRowsSEXP, SEXP iMinXCentroideSEXP, SEXP iMinYCentroideSEXP, SEXP mIcentroideSEXP, SEXP iNbCentroidesSEXP, SEXP updateProgressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type vXObservation(vXObservationSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vYObservation(vYObservationSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vLigneObservation(vLigneObservationSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vColonneObservation(vColonneObservationSEXP);
    Rcpp::traits::input_parameter< int >::type iPas(iPasSEXP);
    Rcpp::traits::input_parameter< int >::type iRayon(iRayonSEXP);
    Rcpp::traits::input_parameter< int >::type iNeighbor(iNeighborSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mVariables(mVariablesSEXP);
    Rcpp::traits::input_parameter< int >::type iNumberCols(iNumberColsSEXP);
    Rcpp::traits::input_parameter< int >::type iNumberRows(iNumberRowsSEXP);
    Rcpp::traits::input_parameter< int >::type iMinXCentroide(iMinXCentroideSEXP);
    Rcpp::traits::input_parameter< int >::type iMinYCentroide(iMinYCentroideSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type mIcentroide(mIcentroideSEXP);
    Rcpp::traits::input_parameter< int >::type iNbCentroides(iNbCentroidesSEXP);
    Rcpp::traits::input_parameter< Nullable <Function> >::type updateProgress(updateProgressSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppLissage(vXObservation, vYObservation, vLigneObservation, vColonneObservation, iPas, iRayon, iNeighbor, mVariables, iNumberCols, iNumberRows, iMinXCentroide, iMinYCentroide, mIcentroide, iNbCentroides, updateProgress));
    return rcpp_result_gen;
END_RCPP
}
// rcppLissageMedianGrappe
NumericMatrix rcppLissageMedianGrappe(int iMinObsGrappe, IntegerVector vXObservation, IntegerVector vYObservation, IntegerVector vLigneObservation, IntegerVector vColonneObservation, int iPas, int iRayon, NumericMatrix mVariables, IntegerVector vXCentroide, IntegerVector vYCentroide, IntegerVector vLigneCentroide, IntegerVector vColonneCentroide, NumericVector vQuantile);
RcppExport SEXP btb_rcppLissageMedianGrappe(SEXP iMinObsGrappeSEXP, SEXP vXObservationSEXP, SEXP vYObservationSEXP, SEXP vLigneObservationSEXP, SEXP vColonneObservationSEXP, SEXP iPasSEXP, SEXP iRayonSEXP, SEXP mVariablesSEXP, SEXP vXCentroideSEXP, SEXP vYCentroideSEXP, SEXP vLigneCentroideSEXP, SEXP vColonneCentroideSEXP, SEXP vQuantileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type iMinObsGrappe(iMinObsGrappeSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vXObservation(vXObservationSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vYObservation(vYObservationSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vLigneObservation(vLigneObservationSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vColonneObservation(vColonneObservationSEXP);
    Rcpp::traits::input_parameter< int >::type iPas(iPasSEXP);
    Rcpp::traits::input_parameter< int >::type iRayon(iRayonSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mVariables(mVariablesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vXCentroide(vXCentroideSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vYCentroide(vYCentroideSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vLigneCentroide(vLigneCentroideSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vColonneCentroide(vColonneCentroideSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vQuantile(vQuantileSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppLissageMedianGrappe(iMinObsGrappe, vXObservation, vYObservation, vLigneObservation, vColonneObservation, iPas, iRayon, mVariables, vXCentroide, vYCentroide, vLigneCentroide, vColonneCentroide, vQuantile));
    return rcpp_result_gen;
END_RCPP
}