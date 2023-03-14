# Roxygen documentation

#' Create a dataframe with CPC names out of a list of CPC codes.
#'
#' This function returns the names of CPC codes.
#'
#' @param codes Supply the CPC codes of which you want to get the name.
#' @param level Specify the level of the codes provided (2 or 3).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_cpc_names = function(codes, level=NULL){

  # Load cpc names
  cpc.names <- gtalibrary::cpc.names

  if(level == 2) {
    check <- gta_parameter_check(codes, cpc.names$cpc[cpc.names$cpc.digit.level==2])

    if(check!="OK"){
      print(paste("Unkown CPC code(s): ", check, ".", sep=""))

    } else {
        cpc.code.names <<- data.frame(cpc = codes, names = cpc.names$cpc.name[cpc.names$cpc.digit.level==2 & cpc.names$cpc == codes])
      }
  } else if (level == 3) {
    check <- gta_parameter_check(codes, cpc.names$cpc[cpc.names$cpc.digit.level==3])

    if(check!="OK"){
      print(paste("Unkown CPC code(s): ", check, ".", sep=""))

    } else {
      cpc.code.names <<- data.frame(cpc = codes, names = cpc.names$cpc.name[cpc.names$cpc.digit.level==3 & cpc.names$cpc == codes])
    }
  } else {
    stop("Please specify the CPC level correctly.")
  }

  rm(codes)

}
