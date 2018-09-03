# Roxygen documentation

#' Check whether your CPC codes are consistent with CPC 2.1.
#'
#' This function checks whether a vector of CPC codes is consistent with CPC 2.1 and returns it as a vector of 3-digit level codes.
#'
#' @param codes Supply the CPC codes you want to check. Values with 2 or more digits are allowed.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_cpc_code_check=function(codes){
  code.correspondence=read.csv("R help files/cpc 2.1 to HS 2012.csv", sep=";")
  names(code.correspondence)=c("cpc.3digit", "hs.6digit") ## JF has some importing issue, thus the renaming here.

  ## NOTES
  # see gta_hs_code_check notes
  rm(codes)
}
