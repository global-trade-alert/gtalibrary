# Roxygen documentation

#' Convert CPC 2.1 codes into HS 2012 (6-digit level).
#'
#' This function converts vectors of CPC 2.1 codes (any granularity) into a vector of 6-digit HS 2012 codes.
#'
#' @param codes Supply the CPC codes you want to convert. Values with 2 or more digits are allowed.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_cpc_to_hs=function(codes){
  code.correspondence=read.csv("R help files/cpc 2.1 to HS 2012.csv", sep=";")
  names(code.correspondence)=c("cpc.3digit", "hs.6digit") ## JF has some importing issue, thus the renaming here.

  codes=gta_cpc_code_check(codes)
  codes=code.correspondence$hs.6digit[code.correspondence$cpc.3digit %in% codes]
  return(codes)
  rm(codes, code.correspondence)
}
