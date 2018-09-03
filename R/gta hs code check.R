# Roxygen documentation

#' Check whether your HS codes are consistent with HS 2012.
#'
#' This function checks whether a vector of HS codes is consistent with HS 2012 and returns it as a vector of 6-digit level HS codes.
#'
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_hs_code_check=function(codes){
  code.correspondence=read.csv("R help files/cpc 2.1 to HS 2012.csv", sep=";")
  names(code.correspondence)=c("cpc.3digit", "hs.6digit") ## JF has some importing issue, thus the renaming here.

  ## NOTES:
  # check whether there are zeros at the front
  # check whether you can find all codes in the first place; provide error message for those you cannot find.
  # cut 6+ digits
  rm(codes)
}
