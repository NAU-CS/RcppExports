// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_calculate_bbox
SEXP rcpp_calculate_bbox(SEXP x, SEXP geometry_cols);
RcppExport SEXP _sfheaders_rcpp_calculate_bbox(SEXP xSEXP, SEXP geometry_colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type geometry_cols(geometry_colsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calculate_bbox(x, geometry_cols));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calculate_z_range
SEXP rcpp_calculate_z_range(SEXP x);
RcppExport SEXP _sfheaders_rcpp_calculate_z_range(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calculate_z_range(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calculate_m_range
SEXP rcpp_calculate_m_range(SEXP x);
RcppExport SEXP _sfheaders_rcpp_calculate_m_range(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calculate_m_range(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_dimension
std::string rcpp_sfg_dimension(SEXP x);
RcppExport SEXP _sfheaders_rcpp_sfg_dimension(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_dimension(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_vec
SEXP rcpp_get_vec(SEXP x, SEXP cols);
RcppExport SEXP _sfheaders_rcpp_get_vec(SEXP xSEXP, SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_vec(x, cols));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_mat
SEXP rcpp_get_mat(SEXP x, SEXP cols);
RcppExport SEXP _sfheaders_rcpp_get_mat(SEXP xSEXP, SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_mat(x, cols));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_list_mat
SEXP rcpp_get_list_mat(SEXP x, SEXP cols, SEXP id);
RcppExport SEXP _sfheaders_rcpp_get_list_mat(SEXP xSEXP, SEXP colsSEXP, SEXP idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id(idSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_list_mat(x, cols, id));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_to_df
SEXP rcpp_sfg_to_df(SEXP sfg);
RcppExport SEXP _sfheaders_rcpp_sfg_to_df(SEXP sfgSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sfg(sfgSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_to_df(sfg));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_to_df
SEXP rcpp_sfc_to_df(Rcpp::List sfc);
RcppExport SEXP _sfheaders_rcpp_sfc_to_df(SEXP sfcSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type sfc(sfcSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_to_df(sfc));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sf_to_df
SEXP rcpp_sf_to_df(Rcpp::DataFrame sf, bool fill);
RcppExport SEXP _sfheaders_rcpp_sf_to_df(SEXP sfSEXP, SEXP fillSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type sf(sfSEXP);
    Rcpp::traits::input_parameter< bool >::type fill(fillSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sf_to_df(sf, fill));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sf_point
SEXP rcpp_sf_point(SEXP x, SEXP cols, bool keep);
RcppExport SEXP _sfheaders_rcpp_sf_point(SEXP xSEXP, SEXP colsSEXP, SEXP keepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< bool >::type keep(keepSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sf_point(x, cols, keep));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sf_multipoint
SEXP rcpp_sf_multipoint(SEXP x, SEXP cols, SEXP multipoint_id, bool keep);
RcppExport SEXP _sfheaders_rcpp_sf_multipoint(SEXP xSEXP, SEXP colsSEXP, SEXP multipoint_idSEXP, SEXP keepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type multipoint_id(multipoint_idSEXP);
    Rcpp::traits::input_parameter< bool >::type keep(keepSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sf_multipoint(x, cols, multipoint_id, keep));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sf_linestring
SEXP rcpp_sf_linestring(SEXP x, SEXP cols, SEXP linestring_id, bool keep);
RcppExport SEXP _sfheaders_rcpp_sf_linestring(SEXP xSEXP, SEXP colsSEXP, SEXP linestring_idSEXP, SEXP keepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type linestring_id(linestring_idSEXP);
    Rcpp::traits::input_parameter< bool >::type keep(keepSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sf_linestring(x, cols, linestring_id, keep));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sf_multilinestring
SEXP rcpp_sf_multilinestring(SEXP x, SEXP cols, SEXP multilinestring_id, SEXP linestring_id, bool keep);
RcppExport SEXP _sfheaders_rcpp_sf_multilinestring(SEXP xSEXP, SEXP colsSEXP, SEXP multilinestring_idSEXP, SEXP linestring_idSEXP, SEXP keepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type multilinestring_id(multilinestring_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type linestring_id(linestring_idSEXP);
    Rcpp::traits::input_parameter< bool >::type keep(keepSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sf_multilinestring(x, cols, multilinestring_id, linestring_id, keep));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sf_polygon
SEXP rcpp_sf_polygon(SEXP x, SEXP cols, SEXP polygon_id, SEXP linestring_id, bool close, bool keep);
RcppExport SEXP _sfheaders_rcpp_sf_polygon(SEXP xSEXP, SEXP colsSEXP, SEXP polygon_idSEXP, SEXP linestring_idSEXP, SEXP closeSEXP, SEXP keepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type polygon_id(polygon_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type linestring_id(linestring_idSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    Rcpp::traits::input_parameter< bool >::type keep(keepSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sf_polygon(x, cols, polygon_id, linestring_id, close, keep));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sf_multipolygon
SEXP rcpp_sf_multipolygon(SEXP x, SEXP cols, SEXP multipolygon_id, SEXP polygon_id, SEXP linestring_id, bool close, bool keep);
RcppExport SEXP _sfheaders_rcpp_sf_multipolygon(SEXP xSEXP, SEXP colsSEXP, SEXP multipolygon_idSEXP, SEXP polygon_idSEXP, SEXP linestring_idSEXP, SEXP closeSEXP, SEXP keepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type multipolygon_id(multipolygon_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type polygon_id(polygon_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type linestring_id(linestring_idSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    Rcpp::traits::input_parameter< bool >::type keep(keepSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sf_multipolygon(x, cols, multipolygon_id, polygon_id, linestring_id, close, keep));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_point
SEXP rcpp_sfc_point(SEXP x, SEXP cols);
RcppExport SEXP _sfheaders_rcpp_sfc_point(SEXP xSEXP, SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_point(x, cols));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_points
SEXP rcpp_sfc_points(Rcpp::List lst);
RcppExport SEXP _sfheaders_rcpp_sfc_points(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_points(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_multipoint
SEXP rcpp_sfc_multipoint(SEXP x, SEXP cols, SEXP id_col);
RcppExport SEXP _sfheaders_rcpp_sfc_multipoint(SEXP xSEXP, SEXP colsSEXP, SEXP id_colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id_col(id_colSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_multipoint(x, cols, id_col));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_multipoints
SEXP rcpp_sfc_multipoints(Rcpp::List lst);
RcppExport SEXP _sfheaders_rcpp_sfc_multipoints(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_multipoints(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_linestring
SEXP rcpp_sfc_linestring(SEXP x, SEXP cols, SEXP id_col);
RcppExport SEXP _sfheaders_rcpp_sfc_linestring(SEXP xSEXP, SEXP colsSEXP, SEXP id_colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id_col(id_colSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_linestring(x, cols, id_col));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_linestrings
SEXP rcpp_sfc_linestrings(Rcpp::List lst);
RcppExport SEXP _sfheaders_rcpp_sfc_linestrings(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_linestrings(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_multilinestring
SEXP rcpp_sfc_multilinestring(SEXP x, SEXP cols, SEXP multiline_id, SEXP line_id);
RcppExport SEXP _sfheaders_rcpp_sfc_multilinestring(SEXP xSEXP, SEXP colsSEXP, SEXP multiline_idSEXP, SEXP line_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type multiline_id(multiline_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type line_id(line_idSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_multilinestring(x, cols, multiline_id, line_id));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_multilinestrings
SEXP rcpp_sfc_multilinestrings(Rcpp::List lst);
RcppExport SEXP _sfheaders_rcpp_sfc_multilinestrings(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_multilinestrings(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_polygon
SEXP rcpp_sfc_polygon(SEXP x, SEXP cols, SEXP polygon_id, SEXP line_id, bool close);
RcppExport SEXP _sfheaders_rcpp_sfc_polygon(SEXP xSEXP, SEXP colsSEXP, SEXP polygon_idSEXP, SEXP line_idSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type polygon_id(polygon_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type line_id(line_idSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_polygon(x, cols, polygon_id, line_id, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_polygons
SEXP rcpp_sfc_polygons(Rcpp::List lst, bool close);
RcppExport SEXP _sfheaders_rcpp_sfc_polygons(SEXP lstSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_polygons(lst, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_multipolygon
SEXP rcpp_sfc_multipolygon(SEXP x, SEXP cols, SEXP multipolygon_id, SEXP polygon_id, SEXP linestring_id, bool close);
RcppExport SEXP _sfheaders_rcpp_sfc_multipolygon(SEXP xSEXP, SEXP colsSEXP, SEXP multipolygon_idSEXP, SEXP polygon_idSEXP, SEXP linestring_idSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type multipolygon_id(multipolygon_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type polygon_id(polygon_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type linestring_id(linestring_idSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_multipolygon(x, cols, multipolygon_id, polygon_id, linestring_id, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfc_multipolygons
SEXP rcpp_sfc_multipolygons(Rcpp::List lst, bool close);
RcppExport SEXP _sfheaders_rcpp_sfc_multipolygons(SEXP lstSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfc_multipolygons(lst, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_sfg_type
std::string rcpp_get_sfg_type(int sfg_type);
RcppExport SEXP _sfheaders_rcpp_get_sfg_type(SEXP sfg_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type sfg_type(sfg_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_sfg_type(sfg_type));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_point
SEXP rcpp_sfg_point(SEXP x, SEXP geometry_columns);
RcppExport SEXP _sfheaders_rcpp_sfg_point(SEXP xSEXP, SEXP geometry_columnsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type geometry_columns(geometry_columnsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_point(x, geometry_columns));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_points
Rcpp::List rcpp_sfg_points(Rcpp::List& lst);
RcppExport SEXP _sfheaders_rcpp_sfg_points(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_points(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_multipoint
SEXP rcpp_sfg_multipoint(SEXP x, SEXP geometry_columns);
RcppExport SEXP _sfheaders_rcpp_sfg_multipoint(SEXP xSEXP, SEXP geometry_columnsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type geometry_columns(geometry_columnsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_multipoint(x, geometry_columns));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_multipoints
Rcpp::List rcpp_sfg_multipoints(Rcpp::List& lst);
RcppExport SEXP _sfheaders_rcpp_sfg_multipoints(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_multipoints(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_linestring
SEXP rcpp_sfg_linestring(SEXP x, SEXP geometry_columns);
RcppExport SEXP _sfheaders_rcpp_sfg_linestring(SEXP xSEXP, SEXP geometry_columnsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type geometry_columns(geometry_columnsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_linestring(x, geometry_columns));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_linestrings
Rcpp::List rcpp_sfg_linestrings(Rcpp::List& lst);
RcppExport SEXP _sfheaders_rcpp_sfg_linestrings(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_linestrings(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_multilinestring
SEXP rcpp_sfg_multilinestring(SEXP x, SEXP geometry_columns, SEXP line_id);
RcppExport SEXP _sfheaders_rcpp_sfg_multilinestring(SEXP xSEXP, SEXP geometry_columnsSEXP, SEXP line_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type geometry_columns(geometry_columnsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type line_id(line_idSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_multilinestring(x, geometry_columns, line_id));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_multilinestrings
Rcpp::List rcpp_sfg_multilinestrings(Rcpp::List& lst);
RcppExport SEXP _sfheaders_rcpp_sfg_multilinestrings(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_multilinestrings(lst));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_polygon
SEXP rcpp_sfg_polygon(SEXP x, SEXP geometry_columns, SEXP line_id, bool close);
RcppExport SEXP _sfheaders_rcpp_sfg_polygon(SEXP xSEXP, SEXP geometry_columnsSEXP, SEXP line_idSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type geometry_columns(geometry_columnsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type line_id(line_idSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_polygon(x, geometry_columns, line_id, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_polygons
Rcpp::List rcpp_sfg_polygons(Rcpp::List& lst, bool close);
RcppExport SEXP _sfheaders_rcpp_sfg_polygons(SEXP lstSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type lst(lstSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_polygons(lst, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_multipolygon
SEXP rcpp_sfg_multipolygon(SEXP x, SEXP geometry_columns, SEXP polygon_id, SEXP line_id, bool close);
RcppExport SEXP _sfheaders_rcpp_sfg_multipolygon(SEXP xSEXP, SEXP geometry_columnsSEXP, SEXP polygon_idSEXP, SEXP line_idSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type geometry_columns(geometry_columnsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type polygon_id(polygon_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type line_id(line_idSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_multipolygon(x, geometry_columns, polygon_id, line_id, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sfg_multipolygons
Rcpp::List rcpp_sfg_multipolygons(Rcpp::List& lst, bool close);
RcppExport SEXP _sfheaders_rcpp_sfg_multipolygons(SEXP lstSEXP, SEXP closeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type lst(lstSEXP);
    Rcpp::traits::input_parameter< bool >::type close(closeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sfg_multipolygons(lst, close));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_other_columns
SEXP rcpp_other_columns(SEXP x, SEXP id_cols, SEXP id_col2, SEXP id_col3);
RcppExport SEXP _sfheaders_rcpp_other_columns(SEXP xSEXP, SEXP id_colsSEXP, SEXP id_col2SEXP, SEXP id_col3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id_cols(id_colsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id_col2(id_col2SEXP);
    Rcpp::traits::input_parameter< SEXP >::type id_col3(id_col3SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_other_columns(x, id_cols, id_col2, id_col3));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_id_positions
Rcpp::IntegerMatrix rcpp_id_positions(SEXP line_ids, SEXP unique_ids);
RcppExport SEXP _sfheaders_rcpp_id_positions(SEXP line_idsSEXP, SEXP unique_idsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type line_ids(line_idsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type unique_ids(unique_idsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_id_positions(line_ids, unique_ids));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_subset_dataframe
Rcpp::DataFrame rcpp_subset_dataframe(Rcpp::DataFrame df, Rcpp::StringVector cols, int start, int end);
RcppExport SEXP _sfheaders_rcpp_subset_dataframe(SEXP dfSEXP, SEXP colsSEXP, SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_subset_dataframe(df, cols, start, end));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_concatenate_vectors
SEXP rcpp_concatenate_vectors(SEXP vec_1, SEXP vec_2);
RcppExport SEXP _sfheaders_rcpp_concatenate_vectors(SEXP vec_1SEXP, SEXP vec_2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type vec_1(vec_1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type vec_2(vec_2SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_concatenate_vectors(vec_1, vec_2));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_column_positions
Rcpp::IntegerVector rcpp_column_positions(SEXP m, Rcpp::StringVector cols);
RcppExport SEXP _sfheaders_rcpp_column_positions(SEXP mSEXP, SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_column_positions(m, cols));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_where_is
int rcpp_where_is(Rcpp::String to_find, Rcpp::StringVector sv);
RcppExport SEXP _sfheaders_rcpp_where_is(SEXP to_findSEXP, SEXP svSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type to_find(to_findSEXP);
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type sv(svSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_where_is(to_find, sv));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_ids
SEXP rcpp_get_ids(SEXP x, SEXP id_col);
RcppExport SEXP _sfheaders_rcpp_get_ids(SEXP xSEXP, SEXP id_colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type id_col(id_colSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_ids(x, id_col));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sfheaders_rcpp_calculate_bbox", (DL_FUNC) &_sfheaders_rcpp_calculate_bbox, 2},
    {"_sfheaders_rcpp_calculate_z_range", (DL_FUNC) &_sfheaders_rcpp_calculate_z_range, 1},
    {"_sfheaders_rcpp_calculate_m_range", (DL_FUNC) &_sfheaders_rcpp_calculate_m_range, 1},
    {"_sfheaders_rcpp_sfg_dimension", (DL_FUNC) &_sfheaders_rcpp_sfg_dimension, 1},
    {"_sfheaders_rcpp_get_vec", (DL_FUNC) &_sfheaders_rcpp_get_vec, 2},
    {"_sfheaders_rcpp_get_mat", (DL_FUNC) &_sfheaders_rcpp_get_mat, 2},
    {"_sfheaders_rcpp_get_list_mat", (DL_FUNC) &_sfheaders_rcpp_get_list_mat, 3},
    {"_sfheaders_rcpp_sfg_to_df", (DL_FUNC) &_sfheaders_rcpp_sfg_to_df, 1},
    {"_sfheaders_rcpp_sfc_to_df", (DL_FUNC) &_sfheaders_rcpp_sfc_to_df, 1},
    {"_sfheaders_rcpp_sf_to_df", (DL_FUNC) &_sfheaders_rcpp_sf_to_df, 2},
    {"_sfheaders_rcpp_sf_point", (DL_FUNC) &_sfheaders_rcpp_sf_point, 3},
    {"_sfheaders_rcpp_sf_multipoint", (DL_FUNC) &_sfheaders_rcpp_sf_multipoint, 4},
    {"_sfheaders_rcpp_sf_linestring", (DL_FUNC) &_sfheaders_rcpp_sf_linestring, 4},
    {"_sfheaders_rcpp_sf_multilinestring", (DL_FUNC) &_sfheaders_rcpp_sf_multilinestring, 5},
    {"_sfheaders_rcpp_sf_polygon", (DL_FUNC) &_sfheaders_rcpp_sf_polygon, 6},
    {"_sfheaders_rcpp_sf_multipolygon", (DL_FUNC) &_sfheaders_rcpp_sf_multipolygon, 7},
    {"_sfheaders_rcpp_sfc_point", (DL_FUNC) &_sfheaders_rcpp_sfc_point, 2},
    {"_sfheaders_rcpp_sfc_points", (DL_FUNC) &_sfheaders_rcpp_sfc_points, 1},
    {"_sfheaders_rcpp_sfc_multipoint", (DL_FUNC) &_sfheaders_rcpp_sfc_multipoint, 3},
    {"_sfheaders_rcpp_sfc_multipoints", (DL_FUNC) &_sfheaders_rcpp_sfc_multipoints, 1},
    {"_sfheaders_rcpp_sfc_linestring", (DL_FUNC) &_sfheaders_rcpp_sfc_linestring, 3},
    {"_sfheaders_rcpp_sfc_linestrings", (DL_FUNC) &_sfheaders_rcpp_sfc_linestrings, 1},
    {"_sfheaders_rcpp_sfc_multilinestring", (DL_FUNC) &_sfheaders_rcpp_sfc_multilinestring, 4},
    {"_sfheaders_rcpp_sfc_multilinestrings", (DL_FUNC) &_sfheaders_rcpp_sfc_multilinestrings, 1},
    {"_sfheaders_rcpp_sfc_polygon", (DL_FUNC) &_sfheaders_rcpp_sfc_polygon, 5},
    {"_sfheaders_rcpp_sfc_polygons", (DL_FUNC) &_sfheaders_rcpp_sfc_polygons, 2},
    {"_sfheaders_rcpp_sfc_multipolygon", (DL_FUNC) &_sfheaders_rcpp_sfc_multipolygon, 6},
    {"_sfheaders_rcpp_sfc_multipolygons", (DL_FUNC) &_sfheaders_rcpp_sfc_multipolygons, 2},
    {"_sfheaders_rcpp_get_sfg_type", (DL_FUNC) &_sfheaders_rcpp_get_sfg_type, 1},
    {"_sfheaders_rcpp_sfg_point", (DL_FUNC) &_sfheaders_rcpp_sfg_point, 2},
    {"_sfheaders_rcpp_sfg_points", (DL_FUNC) &_sfheaders_rcpp_sfg_points, 1},
    {"_sfheaders_rcpp_sfg_multipoint", (DL_FUNC) &_sfheaders_rcpp_sfg_multipoint, 2},
    {"_sfheaders_rcpp_sfg_multipoints", (DL_FUNC) &_sfheaders_rcpp_sfg_multipoints, 1},
    {"_sfheaders_rcpp_sfg_linestring", (DL_FUNC) &_sfheaders_rcpp_sfg_linestring, 2},
    {"_sfheaders_rcpp_sfg_linestrings", (DL_FUNC) &_sfheaders_rcpp_sfg_linestrings, 1},
    {"_sfheaders_rcpp_sfg_multilinestring", (DL_FUNC) &_sfheaders_rcpp_sfg_multilinestring, 3},
    {"_sfheaders_rcpp_sfg_multilinestrings", (DL_FUNC) &_sfheaders_rcpp_sfg_multilinestrings, 1},
    {"_sfheaders_rcpp_sfg_polygon", (DL_FUNC) &_sfheaders_rcpp_sfg_polygon, 4},
    {"_sfheaders_rcpp_sfg_polygons", (DL_FUNC) &_sfheaders_rcpp_sfg_polygons, 2},
    {"_sfheaders_rcpp_sfg_multipolygon", (DL_FUNC) &_sfheaders_rcpp_sfg_multipolygon, 5},
    {"_sfheaders_rcpp_sfg_multipolygons", (DL_FUNC) &_sfheaders_rcpp_sfg_multipolygons, 2},
    {"_sfheaders_rcpp_other_columns", (DL_FUNC) &_sfheaders_rcpp_other_columns, 4},
    {"_sfheaders_rcpp_id_positions", (DL_FUNC) &_sfheaders_rcpp_id_positions, 2},
    {"_sfheaders_rcpp_subset_dataframe", (DL_FUNC) &_sfheaders_rcpp_subset_dataframe, 4},
    {"_sfheaders_rcpp_concatenate_vectors", (DL_FUNC) &_sfheaders_rcpp_concatenate_vectors, 2},
    {"_sfheaders_rcpp_column_positions", (DL_FUNC) &_sfheaders_rcpp_column_positions, 2},
    {"_sfheaders_rcpp_where_is", (DL_FUNC) &_sfheaders_rcpp_where_is, 2},
    {"_sfheaders_rcpp_get_ids", (DL_FUNC) &_sfheaders_rcpp_get_ids, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_sfheaders(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
