// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// getNberCriteria_Rcpp
int getNberCriteria_Rcpp();
RcppExport SEXP _ClustMMDD_getNberCriteria_Rcpp() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getNberCriteria_Rcpp());
    return rcpp_result_gen;
END_RCPP
}
// getCriteriaNames_Rcpp
Rcpp::CharacterVector getCriteriaNames_Rcpp();
RcppExport SEXP _ClustMMDD_getCriteriaNames_Rcpp() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getCriteriaNames_Rcpp());
    return rcpp_result_gen;
END_RCPP
}
// getNberOccurrencesMax
int getNberOccurrencesMax();
RcppExport SEXP _ClustMMDD_getNberOccurrencesMax() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getNberOccurrencesMax());
    return rcpp_result_gen;
END_RCPP
}
// simulProb
Rcpp::DoubleVector simulProb(int n);
RcppExport SEXP _ClustMMDD_simulProb(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(simulProb(n));
    return rcpp_result_gen;
END_RCPP
}
// testFactorial
void testFactorial();
RcppExport SEXP _ClustMMDD_testFactorial() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    testFactorial();
    return R_NilValue;
END_RCPP
}
// cutInN
Rcpp::CharacterMatrix cutInN(Rcpp::CharacterVector x, int N);
RcppExport SEXP _ClustMMDD_cutInN(SEXP xSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(cutInN(x, N));
    return rcpp_result_gen;
END_RCPP
}
// cutEachColInN
Rcpp::CharacterMatrix cutEachColInN(Rcpp::CharacterMatrix tab, int N);
RcppExport SEXP _ClustMMDD_cutEachColInN(SEXP tabSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterMatrix >::type tab(tabSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(cutEachColInN(tab, N));
    return rcpp_result_gen;
END_RCPP
}
// howmanyWords
int howmanyWords(std::string line);
RcppExport SEXP _ClustMMDD_howmanyWords(SEXP lineSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type line(lineSEXP);
    rcpp_result_gen = Rcpp::wrap(howmanyWords(line));
    return rcpp_result_gen;
END_RCPP
}
// isComment
bool isComment(std::string ligne);
RcppExport SEXP _ClustMMDD_isComment(SEXP ligneSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type ligne(ligneSEXP);
    rcpp_result_gen = Rcpp::wrap(isComment(ligne));
    return rcpp_result_gen;
END_RCPP
}
// nberOfLines
int nberOfLines(std::string fichier);
RcppExport SEXP _ClustMMDD_nberOfLines(SEXP fichierSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type fichier(fichierSEXP);
    rcpp_result_gen = Rcpp::wrap(nberOfLines(fichier));
    return rcpp_result_gen;
END_RCPP
}
// nberOfColumns
int nberOfColumns(std::string fichier);
RcppExport SEXP _ClustMMDD_nberOfColumns(SEXP fichierSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type fichier(fichierSEXP);
    rcpp_result_gen = Rcpp::wrap(nberOfColumns(fichier));
    return rcpp_result_gen;
END_RCPP
}
// readLineN_R
std::string readLineN_R(std::string fichier, int n);
RcppExport SEXP _ClustMMDD_readLineN_R(SEXP fichierSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type fichier(fichierSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(readLineN_R(fichier, n));
    return rcpp_result_gen;
END_RCPP
}
// initialiseEmSettings
void initialiseEmSettings();
RcppExport SEXP _ClustMMDD_initialiseEmSettings() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    initialiseEmSettings();
    return R_NilValue;
END_RCPP
}
// EmOptionsDefault
void EmOptionsDefault();
RcppExport SEXP _ClustMMDD_EmOptionsDefault() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    EmOptionsDefault();
    return R_NilValue;
END_RCPP
}
// EmSettings
void EmSettings(double xepsi, int xnberSmallEM, int xnberIterations, int xtypeEM, int xtypeSmallEM, int xnberIterLongEM, bool xputThreshold);
RcppExport SEXP _ClustMMDD_EmSettings(SEXP xepsiSEXP, SEXP xnberSmallEMSEXP, SEXP xnberIterationsSEXP, SEXP xtypeEMSEXP, SEXP xtypeSmallEMSEXP, SEXP xnberIterLongEMSEXP, SEXP xputThresholdSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type xepsi(xepsiSEXP);
    Rcpp::traits::input_parameter< int >::type xnberSmallEM(xnberSmallEMSEXP);
    Rcpp::traits::input_parameter< int >::type xnberIterations(xnberIterationsSEXP);
    Rcpp::traits::input_parameter< int >::type xtypeEM(xtypeEMSEXP);
    Rcpp::traits::input_parameter< int >::type xtypeSmallEM(xtypeSmallEMSEXP);
    Rcpp::traits::input_parameter< int >::type xnberIterLongEM(xnberIterLongEMSEXP);
    Rcpp::traits::input_parameter< bool >::type xputThreshold(xputThresholdSEXP);
    EmSettings(xepsi, xnberSmallEM, xnberIterations, xtypeEM, xtypeSmallEM, xnberIterLongEM, xputThreshold);
    return R_NilValue;
END_RCPP
}
// EmOptionsDisplay
void EmOptionsDisplay();
RcppExport SEXP _ClustMMDD_EmOptionsDisplay() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    EmOptionsDisplay();
    return R_NilValue;
END_RCPP
}
// getEmOptions_Rcpp
Rcpp::List getEmOptions_Rcpp();
RcppExport SEXP _ClustMMDD_getEmOptions_Rcpp() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getEmOptions_Rcpp());
    return rcpp_result_gen;
END_RCPP
}
// obsFreq
Rcpp::List obsFreq(Rcpp::IntegerMatrix data, int ploidy, Rcpp::CharacterVector levels, Rcpp::IntegerVector n_levels, Rcpp::DoubleVector levels_freq, Rcpp::IntegerVector classif, Rcpp::LogicalVector S);
RcppExport SEXP _ClustMMDD_obsFreq(SEXP dataSEXP, SEXP ploidySEXP, SEXP levelsSEXP, SEXP n_levelsSEXP, SEXP levels_freqSEXP, SEXP classifSEXP, SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type ploidy(ploidySEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type n_levels(n_levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type levels_freq(levels_freqSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type classif(classifSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type S(SSEXP);
    rcpp_result_gen = Rcpp::wrap(obsFreq(data, ploidy, levels, n_levels, levels_freq, classif, S));
    return rcpp_result_gen;
END_RCPP
}
// EM1_Rcpp
Rcpp::List EM1_Rcpp(Rcpp::IntegerMatrix tab, int ploidy, Rcpp::CharacterVector levels, Rcpp::IntegerVector n_levels, Rcpp::IntegerVector levels_count, Rcpp::DoubleVector levels_freq, double Cte);
RcppExport SEXP _ClustMMDD_EM1_Rcpp(SEXP tabSEXP, SEXP ploidySEXP, SEXP levelsSEXP, SEXP n_levelsSEXP, SEXP levels_countSEXP, SEXP levels_freqSEXP, SEXP CteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type tab(tabSEXP);
    Rcpp::traits::input_parameter< int >::type ploidy(ploidySEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type n_levels(n_levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type levels_count(levels_countSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type levels_freq(levels_freqSEXP);
    Rcpp::traits::input_parameter< double >::type Cte(CteSEXP);
    rcpp_result_gen = Rcpp::wrap(EM1_Rcpp(tab, ploidy, levels, n_levels, levels_count, levels_freq, Cte));
    return rcpp_result_gen;
END_RCPP
}
// mapClassification_Rcpp
Rcpp::IntegerVector mapClassification_Rcpp(Rcpp::NumericMatrix Tik);
RcppExport SEXP _ClustMMDD_mapClassification_Rcpp(SEXP TikSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Tik(TikSEXP);
    rcpp_result_gen = Rcpp::wrap(mapClassification_Rcpp(Tik));
    return rcpp_result_gen;
END_RCPP
}
// smallEM_Rcpp
Rcpp::List smallEM_Rcpp(Rcpp::IntegerMatrix tab, int ploidy, Rcpp::CharacterVector levels, Rcpp::IntegerVector n_levels, Rcpp::IntegerVector levels_count, Rcpp::DoubleVector levels_freq, int K, Rcpp::LogicalVector S);
RcppExport SEXP _ClustMMDD_smallEM_Rcpp(SEXP tabSEXP, SEXP ploidySEXP, SEXP levelsSEXP, SEXP n_levelsSEXP, SEXP levels_countSEXP, SEXP levels_freqSEXP, SEXP KSEXP, SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type tab(tabSEXP);
    Rcpp::traits::input_parameter< int >::type ploidy(ploidySEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type n_levels(n_levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type levels_count(levels_countSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type levels_freq(levels_freqSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type S(SSEXP);
    rcpp_result_gen = Rcpp::wrap(smallEM_Rcpp(tab, ploidy, levels, n_levels, levels_count, levels_freq, K, S));
    return rcpp_result_gen;
END_RCPP
}
// EM_Rcpp
Rcpp::List EM_Rcpp(Rcpp::IntegerMatrix tab, int ploidy, Rcpp::CharacterVector levels, Rcpp::IntegerVector n_levels, Rcpp::IntegerVector levels_count, Rcpp::DoubleVector levels_freq, int K, Rcpp::LogicalVector S, double Cte);
RcppExport SEXP _ClustMMDD_EM_Rcpp(SEXP tabSEXP, SEXP ploidySEXP, SEXP levelsSEXP, SEXP n_levelsSEXP, SEXP levels_countSEXP, SEXP levels_freqSEXP, SEXP KSEXP, SEXP SSEXP, SEXP CteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type tab(tabSEXP);
    Rcpp::traits::input_parameter< int >::type ploidy(ploidySEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type n_levels(n_levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type levels_count(levels_countSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type levels_freq(levels_freqSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type S(SSEXP);
    Rcpp::traits::input_parameter< double >::type Cte(CteSEXP);
    rcpp_result_gen = Rcpp::wrap(EM_Rcpp(tab, ploidy, levels, n_levels, levels_count, levels_freq, K, S, Cte));
    return rcpp_result_gen;
END_RCPP
}
// readModelFromString_Rcpp
Rcpp::List readModelFromString_Rcpp(std::string mod);
RcppExport SEXP _ClustMMDD_readModelFromString_Rcpp(SEXP modSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type mod(modSEXP);
    rcpp_result_gen = Rcpp::wrap(readModelFromString_Rcpp(mod));
    return rcpp_result_gen;
END_RCPP
}
// isInFile_Rcpp
Rcpp::List isInFile_Rcpp(int K, Rcpp::LogicalVector S, std::string fichier, bool header);
RcppExport SEXP _ClustMMDD_isInFile_Rcpp(SEXP KSEXP, SEXP SSEXP, SEXP fichierSEXP, SEXP headerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type S(SSEXP);
    Rcpp::traits::input_parameter< std::string >::type fichier(fichierSEXP);
    Rcpp::traits::input_parameter< bool >::type header(headerSEXP);
    rcpp_result_gen = Rcpp::wrap(isInFile_Rcpp(K, S, fichier, header));
    return rcpp_result_gen;
END_RCPP
}
// computeCriteria_Rcpp
Rcpp::DoubleVector computeCriteria_Rcpp(double lv, int dim, int N, double entropy, double Cte);
RcppExport SEXP _ClustMMDD_computeCriteria_Rcpp(SEXP lvSEXP, SEXP dimSEXP, SEXP NSEXP, SEXP entropySEXP, SEXP CteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lv(lvSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type entropy(entropySEXP);
    Rcpp::traits::input_parameter< double >::type Cte(CteSEXP);
    rcpp_result_gen = Rcpp::wrap(computeCriteria_Rcpp(lv, dim, N, entropy, Cte));
    return rcpp_result_gen;
END_RCPP
}
// computeCriteriaFromFile_Rcpp
Rcpp::List computeCriteriaFromFile_Rcpp(std::string xfile, double Cte, bool header, Rcpp::IntegerVector indexes);
RcppExport SEXP _ClustMMDD_computeCriteriaFromFile_Rcpp(SEXP xfileSEXP, SEXP CteSEXP, SEXP headerSEXP, SEXP indexesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type xfile(xfileSEXP);
    Rcpp::traits::input_parameter< double >::type Cte(CteSEXP);
    Rcpp::traits::input_parameter< bool >::type header(headerSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type indexes(indexesSEXP);
    rcpp_result_gen = Rcpp::wrap(computeCriteriaFromFile_Rcpp(xfile, Cte, header, indexes));
    return rcpp_result_gen;
END_RCPP
}
// writeParInFile_Rcpp
void writeParInFile_Rcpp(Rcpp::List x, std::string xfile);
RcppExport SEXP _ClustMMDD_writeParInFile_Rcpp(SEXP xSEXP, SEXP xfileSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type xfile(xfileSEXP);
    writeParInFile_Rcpp(x, xfile);
    return R_NilValue;
END_RCPP
}
// writeModelInFile_Rcpp
void writeModelInFile_Rcpp(Rcpp::List x, std::string xfile);
RcppExport SEXP _ClustMMDD_writeModelInFile_Rcpp(SEXP xSEXP, SEXP xfileSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type xfile(xfileSEXP);
    writeModelInFile_Rcpp(x, xfile);
    return R_NilValue;
END_RCPP
}
// writeCriteriaInFile_Rcpp
void writeCriteriaInFile_Rcpp(Rcpp::DoubleVector criteria, std::string xfile);
RcppExport SEXP _ClustMMDD_writeCriteriaInFile_Rcpp(SEXP criteriaSEXP, SEXP xfileSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type criteria(criteriaSEXP);
    Rcpp::traits::input_parameter< std::string >::type xfile(xfileSEXP);
    writeCriteriaInFile_Rcpp(criteria, xfile);
    return R_NilValue;
END_RCPP
}
// readModelAt_Rcpp
Rcpp::List readModelAt_Rcpp(std::string xfile, int n, bool header);
RcppExport SEXP _ClustMMDD_readModelAt_Rcpp(SEXP xfileSEXP, SEXP nSEXP, SEXP headerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type xfile(xfileSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< bool >::type header(headerSEXP);
    rcpp_result_gen = Rcpp::wrap(readModelAt_Rcpp(xfile, n, header));
    return rcpp_result_gen;
END_RCPP
}
// selectDimFromFile_Rcpp
bool selectDimFromFile_Rcpp(std::string xfileExploredModels, Rcpp::DoubleVector constantGrid, Rcpp::DoubleVector vectLogLik, Rcpp::IntegerVector vectDim, bool header);
RcppExport SEXP _ClustMMDD_selectDimFromFile_Rcpp(SEXP xfileExploredModelsSEXP, SEXP constantGridSEXP, SEXP vectLogLikSEXP, SEXP vectDimSEXP, SEXP headerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type xfileExploredModels(xfileExploredModelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type constantGrid(constantGridSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type vectLogLik(vectLogLikSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vectDim(vectDimSEXP);
    Rcpp::traits::input_parameter< bool >::type header(headerSEXP);
    rcpp_result_gen = Rcpp::wrap(selectDimFromFile_Rcpp(xfileExploredModels, constantGrid, vectLogLik, vectDim, header));
    return rcpp_result_gen;
END_RCPP
}
// selectDimFromData_Rcpp
bool selectDimFromData_Rcpp(Rcpp::DoubleVector xlogLik, Rcpp::IntegerVector xdim, Rcpp::DoubleVector xconstantGrid, Rcpp::DoubleVector outLogLik, Rcpp::IntegerVector outDim);
RcppExport SEXP _ClustMMDD_selectDimFromData_Rcpp(SEXP xlogLikSEXP, SEXP xdimSEXP, SEXP xconstantGridSEXP, SEXP outLogLikSEXP, SEXP outDimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type xlogLik(xlogLikSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type xdim(xdimSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type xconstantGrid(xconstantGridSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type outLogLik(outLogLikSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type outDim(outDimSEXP);
    rcpp_result_gen = Rcpp::wrap(selectDimFromData_Rcpp(xlogLik, xdim, xconstantGrid, outLogLik, outDim));
    return rcpp_result_gen;
END_RCPP
}
// dimJump_Rcpp
bool dimJump_Rcpp(Rcpp::IntegerVector vectDim, int pas, Rcpp::IntegerVector BeginEnd1, Rcpp::IntegerVector BeginEnd2);
RcppExport SEXP _ClustMMDD_dimJump_Rcpp(SEXP vectDimSEXP, SEXP pasSEXP, SEXP BeginEnd1SEXP, SEXP BeginEnd2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vectDim(vectDimSEXP);
    Rcpp::traits::input_parameter< int >::type pas(pasSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type BeginEnd1(BeginEnd1SEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type BeginEnd2(BeginEnd2SEXP);
    rcpp_result_gen = Rcpp::wrap(dimJump_Rcpp(vectDim, pas, BeginEnd1, BeginEnd2));
    return rcpp_result_gen;
END_RCPP
}
// selectModelFromFile_Rcpp
void selectModelFromFile_Rcpp(std::string xfileExploredModels, Rcpp::IntegerVector vectN, Rcpp::IntegerVector vectK, Rcpp::IntegerMatrix matS, Rcpp::DoubleVector vectLogLik, Rcpp::IntegerVector vectDim, Rcpp::DoubleVector vectEntropy, Rcpp::DoubleVector vectCriteria, double cte, bool header, Rcpp::IntegerVector lines);
RcppExport SEXP _ClustMMDD_selectModelFromFile_Rcpp(SEXP xfileExploredModelsSEXP, SEXP vectNSEXP, SEXP vectKSEXP, SEXP matSSEXP, SEXP vectLogLikSEXP, SEXP vectDimSEXP, SEXP vectEntropySEXP, SEXP vectCriteriaSEXP, SEXP cteSEXP, SEXP headerSEXP, SEXP linesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type xfileExploredModels(xfileExploredModelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vectN(vectNSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vectK(vectKSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type matS(matSSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type vectLogLik(vectLogLikSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vectDim(vectDimSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type vectEntropy(vectEntropySEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type vectCriteria(vectCriteriaSEXP);
    Rcpp::traits::input_parameter< double >::type cte(cteSEXP);
    Rcpp::traits::input_parameter< bool >::type header(headerSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type lines(linesSEXP);
    selectModelFromFile_Rcpp(xfileExploredModels, vectN, vectK, matS, vectLogLik, vectDim, vectEntropy, vectCriteria, cte, header, lines);
    return R_NilValue;
END_RCPP
}
// selectModelFromData_Rcpp
void selectModelFromData_Rcpp(Rcpp::DoubleVector vectLogLik, Rcpp::IntegerVector vectDim, Rcpp::DoubleVector vectEntropy, int N, double cte, Rcpp::IntegerVector vectIndexes, Rcpp::DoubleVector vectCriteria);
RcppExport SEXP _ClustMMDD_selectModelFromData_Rcpp(SEXP vectLogLikSEXP, SEXP vectDimSEXP, SEXP vectEntropySEXP, SEXP NSEXP, SEXP cteSEXP, SEXP vectIndexesSEXP, SEXP vectCriteriaSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type vectLogLik(vectLogLikSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vectDim(vectDimSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type vectEntropy(vectEntropySEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type cte(cteSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type vectIndexes(vectIndexesSEXP);
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type vectCriteria(vectCriteriaSEXP);
    selectModelFromData_Rcpp(vectLogLik, vectDim, vectEntropy, N, cte, vectIndexes, vectCriteria);
    return R_NilValue;
END_RCPP
}
// readParKS_Rcpp
Rcpp::List readParKS_Rcpp(std::string xfile);
RcppExport SEXP _ClustMMDD_readParKS_Rcpp(SEXP xfileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type xfile(xfileSEXP);
    rcpp_result_gen = Rcpp::wrap(readParKS_Rcpp(xfile));
    return rcpp_result_gen;
END_RCPP
}
// writeParKS_InFile_Rcpp
void writeParKS_InFile_Rcpp(Rcpp::List modelList, std::string file);
RcppExport SEXP _ClustMMDD_writeParKS_InFile_Rcpp(SEXP modelListSEXP, SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type modelList(modelListSEXP);
    Rcpp::traits::input_parameter< std::string >::type file(fileSEXP);
    writeParKS_InFile_Rcpp(modelList, file);
    return R_NilValue;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_MODULE_PAR_KS();
RcppExport SEXP _rcpp_module_boot_MODULE_DATA();

static const R_CallMethodDef CallEntries[] = {
    {"_ClustMMDD_getNberCriteria_Rcpp", (DL_FUNC) &_ClustMMDD_getNberCriteria_Rcpp, 0},
    {"_ClustMMDD_getCriteriaNames_Rcpp", (DL_FUNC) &_ClustMMDD_getCriteriaNames_Rcpp, 0},
    {"_ClustMMDD_getNberOccurrencesMax", (DL_FUNC) &_ClustMMDD_getNberOccurrencesMax, 0},
    {"_ClustMMDD_simulProb", (DL_FUNC) &_ClustMMDD_simulProb, 1},
    {"_ClustMMDD_testFactorial", (DL_FUNC) &_ClustMMDD_testFactorial, 0},
    {"_ClustMMDD_cutInN", (DL_FUNC) &_ClustMMDD_cutInN, 2},
    {"_ClustMMDD_cutEachColInN", (DL_FUNC) &_ClustMMDD_cutEachColInN, 2},
    {"_ClustMMDD_howmanyWords", (DL_FUNC) &_ClustMMDD_howmanyWords, 1},
    {"_ClustMMDD_isComment", (DL_FUNC) &_ClustMMDD_isComment, 1},
    {"_ClustMMDD_nberOfLines", (DL_FUNC) &_ClustMMDD_nberOfLines, 1},
    {"_ClustMMDD_nberOfColumns", (DL_FUNC) &_ClustMMDD_nberOfColumns, 1},
    {"_ClustMMDD_readLineN_R", (DL_FUNC) &_ClustMMDD_readLineN_R, 2},
    {"_ClustMMDD_initialiseEmSettings", (DL_FUNC) &_ClustMMDD_initialiseEmSettings, 0},
    {"_ClustMMDD_EmOptionsDefault", (DL_FUNC) &_ClustMMDD_EmOptionsDefault, 0},
    {"_ClustMMDD_EmSettings", (DL_FUNC) &_ClustMMDD_EmSettings, 7},
    {"_ClustMMDD_EmOptionsDisplay", (DL_FUNC) &_ClustMMDD_EmOptionsDisplay, 0},
    {"_ClustMMDD_getEmOptions_Rcpp", (DL_FUNC) &_ClustMMDD_getEmOptions_Rcpp, 0},
    {"_ClustMMDD_obsFreq", (DL_FUNC) &_ClustMMDD_obsFreq, 7},
    {"_ClustMMDD_EM1_Rcpp", (DL_FUNC) &_ClustMMDD_EM1_Rcpp, 7},
    {"_ClustMMDD_mapClassification_Rcpp", (DL_FUNC) &_ClustMMDD_mapClassification_Rcpp, 1},
    {"_ClustMMDD_smallEM_Rcpp", (DL_FUNC) &_ClustMMDD_smallEM_Rcpp, 8},
    {"_ClustMMDD_EM_Rcpp", (DL_FUNC) &_ClustMMDD_EM_Rcpp, 9},
    {"_ClustMMDD_readModelFromString_Rcpp", (DL_FUNC) &_ClustMMDD_readModelFromString_Rcpp, 1},
    {"_ClustMMDD_isInFile_Rcpp", (DL_FUNC) &_ClustMMDD_isInFile_Rcpp, 4},
    {"_ClustMMDD_computeCriteria_Rcpp", (DL_FUNC) &_ClustMMDD_computeCriteria_Rcpp, 5},
    {"_ClustMMDD_computeCriteriaFromFile_Rcpp", (DL_FUNC) &_ClustMMDD_computeCriteriaFromFile_Rcpp, 4},
    {"_ClustMMDD_writeParInFile_Rcpp", (DL_FUNC) &_ClustMMDD_writeParInFile_Rcpp, 2},
    {"_ClustMMDD_writeModelInFile_Rcpp", (DL_FUNC) &_ClustMMDD_writeModelInFile_Rcpp, 2},
    {"_ClustMMDD_writeCriteriaInFile_Rcpp", (DL_FUNC) &_ClustMMDD_writeCriteriaInFile_Rcpp, 2},
    {"_ClustMMDD_readModelAt_Rcpp", (DL_FUNC) &_ClustMMDD_readModelAt_Rcpp, 3},
    {"_ClustMMDD_selectDimFromFile_Rcpp", (DL_FUNC) &_ClustMMDD_selectDimFromFile_Rcpp, 5},
    {"_ClustMMDD_selectDimFromData_Rcpp", (DL_FUNC) &_ClustMMDD_selectDimFromData_Rcpp, 5},
    {"_ClustMMDD_dimJump_Rcpp", (DL_FUNC) &_ClustMMDD_dimJump_Rcpp, 4},
    {"_ClustMMDD_selectModelFromFile_Rcpp", (DL_FUNC) &_ClustMMDD_selectModelFromFile_Rcpp, 11},
    {"_ClustMMDD_selectModelFromData_Rcpp", (DL_FUNC) &_ClustMMDD_selectModelFromData_Rcpp, 7},
    {"_ClustMMDD_readParKS_Rcpp", (DL_FUNC) &_ClustMMDD_readParKS_Rcpp, 1},
    {"_ClustMMDD_writeParKS_InFile_Rcpp", (DL_FUNC) &_ClustMMDD_writeParKS_InFile_Rcpp, 2},
    {"_rcpp_module_boot_MODULE_PAR_KS", (DL_FUNC) &_rcpp_module_boot_MODULE_PAR_KS, 0},
    {"_rcpp_module_boot_MODULE_DATA", (DL_FUNC) &_rcpp_module_boot_MODULE_DATA, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_ClustMMDD(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
