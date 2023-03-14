# Roxygen documentation

#' Convert CPC 2.1 codes into HS 2012 (6-digit level).
#'
#' This function converts vectors of CPC 2.1 codes (any granularity) into a vector of 6-digit HS 2012 codes.
#'
#' @param codes Supply the CPC codes you want to convert. Values with 2 or more digits are allowed.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_cpc_to_hs <- function(codes) {
  code.correspondence <- gtalibrary::cpc.to.hs

  codes <- gta_cpc_code_check(codes)
  codes <- code.correspondence$hs[code.correspondence$cpc %in% codes]
  return(codes)
  rm(codes, code.correspondence)
}
