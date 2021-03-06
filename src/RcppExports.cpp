// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// stata
List stata(const char * filePath, const bool missing);
RcppExport SEXP readstata13_stata(SEXP filePathSEXP, SEXP missingSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const char * >::type filePath(filePathSEXP);
    Rcpp::traits::input_parameter< const bool >::type missing(missingSEXP);
    __result = Rcpp::wrap(stata(filePath, missing));
    return __result;
END_RCPP
}
// stataWrite
int stataWrite(const char * filePath, Rcpp::DataFrame dat);
RcppExport SEXP readstata13_stataWrite(SEXP filePathSEXP, SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const char * >::type filePath(filePathSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type dat(datSEXP);
    __result = Rcpp::wrap(stataWrite(filePath, dat));
    return __result;
END_RCPP
}
