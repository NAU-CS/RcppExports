// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/gdtools.h"
#include "../inst/include/gdtools_types.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// str_extents_
NumericMatrix str_extents_(CharacterVector x, std::string fontname, double fontsize, int bold, int italic, std::string fontfile);
RcppExport SEXP _gdtools_str_extents_(SEXP xSEXP, SEXP fontnameSEXP, SEXP fontsizeSEXP, SEXP boldSEXP, SEXP italicSEXP, SEXP fontfileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontname(fontnameSEXP);
    Rcpp::traits::input_parameter< double >::type fontsize(fontsizeSEXP);
    Rcpp::traits::input_parameter< int >::type bold(boldSEXP);
    Rcpp::traits::input_parameter< int >::type italic(italicSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontfile(fontfileSEXP);
    rcpp_result_gen = Rcpp::wrap(str_extents_(x, fontname, fontsize, bold, italic, fontfile));
    return rcpp_result_gen;
END_RCPP
}
// str_metrics_
NumericVector str_metrics_(CharacterVector x, std::string fontname, double fontsize, int bold, int italic, std::string fontfile);
RcppExport SEXP _gdtools_str_metrics_(SEXP xSEXP, SEXP fontnameSEXP, SEXP fontsizeSEXP, SEXP boldSEXP, SEXP italicSEXP, SEXP fontfileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontname(fontnameSEXP);
    Rcpp::traits::input_parameter< double >::type fontsize(fontsizeSEXP);
    Rcpp::traits::input_parameter< int >::type bold(boldSEXP);
    Rcpp::traits::input_parameter< int >::type italic(italicSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontfile(fontfileSEXP);
    rcpp_result_gen = Rcpp::wrap(str_metrics_(x, fontname, fontsize, bold, italic, fontfile));
    return rcpp_result_gen;
END_RCPP
}
// m_str_extents_
NumericMatrix m_str_extents_(CharacterVector x, std::vector<std::string> fontname, std::vector<double> fontsize, std::vector<int> bold, std::vector<int> italic, std::vector<std::string> fontfile);
RcppExport SEXP _gdtools_m_str_extents_(SEXP xSEXP, SEXP fontnameSEXP, SEXP fontsizeSEXP, SEXP boldSEXP, SEXP italicSEXP, SEXP fontfileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type fontname(fontnameSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type fontsize(fontsizeSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type bold(boldSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type italic(italicSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type fontfile(fontfileSEXP);
    rcpp_result_gen = Rcpp::wrap(m_str_extents_(x, fontname, fontsize, bold, italic, fontfile));
    return rcpp_result_gen;
END_RCPP
}
// glyphs_match_
LogicalVector glyphs_match_(CharacterVector x, std::string fontname, int bold, int italic, std::string fontfile);
RcppExport SEXP _gdtools_glyphs_match_(SEXP xSEXP, SEXP fontnameSEXP, SEXP boldSEXP, SEXP italicSEXP, SEXP fontfileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontname(fontnameSEXP);
    Rcpp::traits::input_parameter< int >::type bold(boldSEXP);
    Rcpp::traits::input_parameter< int >::type italic(italicSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontfile(fontfileSEXP);
    rcpp_result_gen = Rcpp::wrap(glyphs_match_(x, fontname, bold, italic, fontfile));
    return rcpp_result_gen;
END_RCPP
}
// context_create
XPtrCairoContext context_create();
static SEXP _gdtools_context_create_try() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(context_create());
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_context_create() {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_context_create_try());
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
// context_set_font
bool context_set_font(XPtrCairoContext cc, std::string fontname, double fontsize, bool bold, bool italic, std::string fontfile);
static SEXP _gdtools_context_set_font_try(SEXP ccSEXP, SEXP fontnameSEXP, SEXP fontsizeSEXP, SEXP boldSEXP, SEXP italicSEXP, SEXP fontfileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< XPtrCairoContext >::type cc(ccSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontname(fontnameSEXP);
    Rcpp::traits::input_parameter< double >::type fontsize(fontsizeSEXP);
    Rcpp::traits::input_parameter< bool >::type bold(boldSEXP);
    Rcpp::traits::input_parameter< bool >::type italic(italicSEXP);
    Rcpp::traits::input_parameter< std::string >::type fontfile(fontfileSEXP);
    rcpp_result_gen = Rcpp::wrap(context_set_font(cc, fontname, fontsize, bold, italic, fontfile));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_context_set_font(SEXP ccSEXP, SEXP fontnameSEXP, SEXP fontsizeSEXP, SEXP boldSEXP, SEXP italicSEXP, SEXP fontfileSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_context_set_font_try(ccSEXP, fontnameSEXP, fontsizeSEXP, boldSEXP, italicSEXP, fontfileSEXP));
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
// context_extents
FontMetric context_extents(XPtrCairoContext cc, std::string x);
static SEXP _gdtools_context_extents_try(SEXP ccSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< XPtrCairoContext >::type cc(ccSEXP);
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(context_extents(cc, x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_context_extents(SEXP ccSEXP, SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_context_extents_try(ccSEXP, xSEXP));
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
// raster_to_str
std::string raster_to_str(std::vector<unsigned int> raster, int w, int h, double width, double height, int interpolate);
static SEXP _gdtools_raster_to_str_try(SEXP rasterSEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<unsigned int> >::type raster(rasterSEXP);
    Rcpp::traits::input_parameter< int >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type height(heightSEXP);
    Rcpp::traits::input_parameter< int >::type interpolate(interpolateSEXP);
    rcpp_result_gen = Rcpp::wrap(raster_to_str(raster, w, h, width, height, interpolate));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_raster_to_str(SEXP rasterSEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_raster_to_str_try(rasterSEXP, wSEXP, hSEXP, widthSEXP, heightSEXP, interpolateSEXP));
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
// raster_to_file
int raster_to_file(std::vector<unsigned int> raster, int w, int h, double width, double height, int interpolate, std::string filename);
static SEXP _gdtools_raster_to_file_try(SEXP rasterSEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP, SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<unsigned int> >::type raster(rasterSEXP);
    Rcpp::traits::input_parameter< int >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type height(heightSEXP);
    Rcpp::traits::input_parameter< int >::type interpolate(interpolateSEXP);
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    rcpp_result_gen = Rcpp::wrap(raster_to_file(raster, w, h, width, height, interpolate, filename));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_raster_to_file(SEXP rasterSEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP, SEXP filenameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_raster_to_file_try(rasterSEXP, wSEXP, hSEXP, widthSEXP, heightSEXP, interpolateSEXP, filenameSEXP));
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
// raster_png_
bool raster_png_(CharacterVector raster_, int w, int h, double width, double height, int interpolate, std::string filename);
static SEXP _gdtools_raster_png__try(SEXP raster_SEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP, SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type raster_(raster_SEXP);
    Rcpp::traits::input_parameter< int >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type height(heightSEXP);
    Rcpp::traits::input_parameter< int >::type interpolate(interpolateSEXP);
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    rcpp_result_gen = Rcpp::wrap(raster_png_(raster_, w, h, width, height, interpolate, filename));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_raster_png_(SEXP raster_SEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP, SEXP filenameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_raster_png__try(raster_SEXP, wSEXP, hSEXP, widthSEXP, heightSEXP, interpolateSEXP, filenameSEXP));
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
// base64_raster_encode
std::string base64_raster_encode(CharacterVector raster_, int w, int h, double width, double height, int interpolate);
static SEXP _gdtools_base64_raster_encode_try(SEXP raster_SEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type raster_(raster_SEXP);
    Rcpp::traits::input_parameter< int >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type height(heightSEXP);
    Rcpp::traits::input_parameter< int >::type interpolate(interpolateSEXP);
    rcpp_result_gen = Rcpp::wrap(base64_raster_encode(raster_, w, h, width, height, interpolate));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_base64_raster_encode(SEXP raster_SEXP, SEXP wSEXP, SEXP hSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP interpolateSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_base64_raster_encode_try(raster_SEXP, wSEXP, hSEXP, widthSEXP, heightSEXP, interpolateSEXP));
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
// base64_file_encode
std::string base64_file_encode(std::string filename);
static SEXP _gdtools_base64_file_encode_try(SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    rcpp_result_gen = Rcpp::wrap(base64_file_encode(filename));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_base64_file_encode(SEXP filenameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_base64_file_encode_try(filenameSEXP));
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
// base64_string_encode
std::string base64_string_encode(std::string string);
static SEXP _gdtools_base64_string_encode_try(SEXP stringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type string(stringSEXP);
    rcpp_result_gen = Rcpp::wrap(base64_string_encode(string));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _gdtools_base64_string_encode(SEXP stringSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_gdtools_base64_string_encode_try(stringSEXP));
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
// version_freetype
Rcpp::List version_freetype();
RcppExport SEXP _gdtools_version_freetype() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(version_freetype());
    return rcpp_result_gen;
END_RCPP
}
// version_cairo_
Rcpp::CharacterVector version_cairo_();
RcppExport SEXP _gdtools_version_cairo_() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(version_cairo_());
    return rcpp_result_gen;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _gdtools_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("XPtrCairoContext(*context_create)()");
        signatures.insert("bool(*context_set_font)(XPtrCairoContext,std::string,double,bool,bool,std::string)");
        signatures.insert("FontMetric(*context_extents)(XPtrCairoContext,std::string)");
        signatures.insert("std::string(*raster_to_str)(std::vector<unsigned int>,int,int,double,double,int)");
        signatures.insert("int(*raster_to_file)(std::vector<unsigned int>,int,int,double,double,int,std::string)");
        signatures.insert("bool(*raster_png_)(CharacterVector,int,int,double,double,int,std::string)");
        signatures.insert("std::string(*base64_raster_encode)(CharacterVector,int,int,double,double,int)");
        signatures.insert("std::string(*base64_file_encode)(std::string)");
        signatures.insert("std::string(*base64_string_encode)(std::string)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _gdtools_RcppExport_registerCCallable() { 
    R_RegisterCCallable("gdtools", "_gdtools_context_create", (DL_FUNC)_gdtools_context_create_try);
    R_RegisterCCallable("gdtools", "_gdtools_context_set_font", (DL_FUNC)_gdtools_context_set_font_try);
    R_RegisterCCallable("gdtools", "_gdtools_context_extents", (DL_FUNC)_gdtools_context_extents_try);
    R_RegisterCCallable("gdtools", "_gdtools_raster_to_str", (DL_FUNC)_gdtools_raster_to_str_try);
    R_RegisterCCallable("gdtools", "_gdtools_raster_to_file", (DL_FUNC)_gdtools_raster_to_file_try);
    R_RegisterCCallable("gdtools", "_gdtools_raster_png_", (DL_FUNC)_gdtools_raster_png__try);
    R_RegisterCCallable("gdtools", "_gdtools_base64_raster_encode", (DL_FUNC)_gdtools_base64_raster_encode_try);
    R_RegisterCCallable("gdtools", "_gdtools_base64_file_encode", (DL_FUNC)_gdtools_base64_file_encode_try);
    R_RegisterCCallable("gdtools", "_gdtools_base64_string_encode", (DL_FUNC)_gdtools_base64_string_encode_try);
    R_RegisterCCallable("gdtools", "_gdtools_RcppExport_validate", (DL_FUNC)_gdtools_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_gdtools_str_extents_", (DL_FUNC) &_gdtools_str_extents_, 6},
    {"_gdtools_str_metrics_", (DL_FUNC) &_gdtools_str_metrics_, 6},
    {"_gdtools_m_str_extents_", (DL_FUNC) &_gdtools_m_str_extents_, 6},
    {"_gdtools_glyphs_match_", (DL_FUNC) &_gdtools_glyphs_match_, 5},
    {"_gdtools_context_create", (DL_FUNC) &_gdtools_context_create, 0},
    {"_gdtools_context_set_font", (DL_FUNC) &_gdtools_context_set_font, 6},
    {"_gdtools_context_extents", (DL_FUNC) &_gdtools_context_extents, 2},
    {"_gdtools_raster_to_str", (DL_FUNC) &_gdtools_raster_to_str, 6},
    {"_gdtools_raster_to_file", (DL_FUNC) &_gdtools_raster_to_file, 7},
    {"_gdtools_raster_png_", (DL_FUNC) &_gdtools_raster_png_, 7},
    {"_gdtools_base64_raster_encode", (DL_FUNC) &_gdtools_base64_raster_encode, 6},
    {"_gdtools_base64_file_encode", (DL_FUNC) &_gdtools_base64_file_encode, 1},
    {"_gdtools_base64_string_encode", (DL_FUNC) &_gdtools_base64_string_encode, 1},
    {"_gdtools_version_freetype", (DL_FUNC) &_gdtools_version_freetype, 0},
    {"_gdtools_version_cairo_", (DL_FUNC) &_gdtools_version_cairo_, 0},
    {"_gdtools_RcppExport_registerCCallable", (DL_FUNC) &_gdtools_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_gdtools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}