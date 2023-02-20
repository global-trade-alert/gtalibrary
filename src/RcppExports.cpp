// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// datefunction
List datefunction(const DateVector& start, const DateVector& end, const Date& current_date, const bool current_year_todate);
RcppExport SEXP _tet_datefunction(SEXP startSEXP, SEXP endSEXP, SEXP current_dateSEXP, SEXP current_year_todateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DateVector& >::type start(startSEXP);
    Rcpp::traits::input_parameter< const DateVector& >::type end(endSEXP);
    Rcpp::traits::input_parameter< const Date& >::type current_date(current_dateSEXP);
    Rcpp::traits::input_parameter< const bool >::type current_year_todate(current_year_todateSEXP);
    rcpp_result_gen = Rcpp::wrap(datefunction(start, end, current_date, current_year_todate));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _tet_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tet_datefunction", (DL_FUNC) &_tet_datefunction, 4},
    {"_tet_rcpp_hello_world", (DL_FUNC) &_tet_rcpp_hello_world, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_tet(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
