# Roxygen documentation

#' Sets your working directory to that used by the GTA data team.
#'
#' @return A common working directory.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_setwd = function(x){

  split.path=unlist(strsplit(getwd(),"/"))
  cld.position=which(split.path=="GTA cloud")

  if(cld.position<length(split.path)){
    setwd(paste(rep("..", length(split.path)-cld.position), collapse="/"))
  }

}
