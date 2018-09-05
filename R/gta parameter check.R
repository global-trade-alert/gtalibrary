# Roxygen documentation

#' This is a helper function that evaluates whether all user input is understood. Returns 'OK' if this is so and the unconforming values otherwise.
#'
#' @param check.vector Specify the vector you want to check.
#' @param permissible.values Specify the permissible values.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_parameter_check=function(check.vector = NULL, permissible.values=NULL){

  if(sum(as.numeric((check.vector %in% permissible.values)==F))>0){
    error.source=paste("'",paste(check.vector[(check.vector %in% permissible.values)==F], collapse="; "),"'", sep="")
    return(error.source)
  } else {return("OK")}
}
