# Roxygen documentation

#' Convert HS 2012 codes into CPC 2.1 codes (3-digit level).
#'
#' This function converts vectors of HS 2012 codes (any granularity) into a vector of 3-digit CPC 2.1 codes.
#'
#' @param codes Supply the HS codes you want to convert. Values with 2, 4, 6 or 8 digits are allowed.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_hs_to_cpc=function(codes){
  code.correspondence=read.csv("R help files/cpc 2.1 to HS 2012.csv", sep=";")
  names(code.correspondence)=c("cpc.3digit", "hs.6digit") ## JF has some importing issue, thus the renaming here.

  codes=gta_hs_code_check(codes)
  codes=code.correspondence$cpc.3digit[code.correspondence$hs.6digit %in% codes]
  return(codes)
  rm(codes, code.correspondence)

}
