// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cpp_serialize_geobuf
RawVector cpp_serialize_geobuf(List x, int decimals);
RcppExport SEXP _protolite_cpp_serialize_geobuf(SEXP xSEXP, SEXP decimalsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type decimals(decimalsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_serialize_geobuf(x, decimals));
    return rcpp_result_gen;
END_RCPP
}
// R_start_protobuf
void R_start_protobuf();
RcppExport SEXP _protolite_R_start_protobuf() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    R_start_protobuf();
    return R_NilValue;
END_RCPP
}
// cpp_serialize_pb
Rcpp::RawVector cpp_serialize_pb(Rcpp::RObject x, bool skip_native);
RcppExport SEXP _protolite_cpp_serialize_pb(SEXP xSEXP, SEXP skip_nativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type skip_native(skip_nativeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_serialize_pb(x, skip_native));
    return rcpp_result_gen;
END_RCPP
}
// cpp_unserialize_geobuf
List cpp_unserialize_geobuf(Rcpp::RawVector x);
RcppExport SEXP _protolite_cpp_unserialize_geobuf(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RawVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_unserialize_geobuf(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_unserialize_mvt
Rcpp::List cpp_unserialize_mvt(Rcpp::RawVector x);
RcppExport SEXP _protolite_cpp_unserialize_mvt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RawVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_unserialize_mvt(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_unserialize_pb
Rcpp::RObject cpp_unserialize_pb(Rcpp::RawVector x);
RcppExport SEXP _protolite_cpp_unserialize_pb(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RawVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_unserialize_pb(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_protolite_cpp_serialize_geobuf", (DL_FUNC) &_protolite_cpp_serialize_geobuf, 2},
    {"_protolite_R_start_protobuf", (DL_FUNC) &_protolite_R_start_protobuf, 0},
    {"_protolite_cpp_serialize_pb", (DL_FUNC) &_protolite_cpp_serialize_pb, 2},
    {"_protolite_cpp_unserialize_geobuf", (DL_FUNC) &_protolite_cpp_unserialize_geobuf, 1},
    {"_protolite_cpp_unserialize_mvt", (DL_FUNC) &_protolite_cpp_unserialize_mvt, 1},
    {"_protolite_cpp_unserialize_pb", (DL_FUNC) &_protolite_cpp_unserialize_pb, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_protolite(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}