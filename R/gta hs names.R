# Roxygen documentation

#' Create a dataframe with HS names out of a list of HS codes.
#'
#' This function returns the names of HS codes on a 6 digit level.
#'
#' @param codes Supply the HS codes of which you want to get the name.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_hs_names = function(codes){

  # Load cpc names
  hs.names <- gtalibrary::hs.names
  codes <- gta_hs_code_check(codes)

  hs.code.names <<- data.frame(hs = codes, names = hs.names$hs.name[hs.names$HS12code %in% codes])

  rm(codes)

}
