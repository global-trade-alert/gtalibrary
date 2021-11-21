# Roxygen documentation

#' Sets your working directory to that used by the GTA data team.
#'
#' @return A common working directory.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_setwd = function(gdrive=NA){

  if(is.na(gdrive)){

    split.path=unlist(strsplit(getwd(),"/"))
    cld.position=which(split.path=="GTA root")
    if(length(cld.position)==0) cld.position=which(split.path=="GTA cloud")
    if(cld.position<length(split.path)){
      setwd(paste(rep("..", length(split.path)-cld.position), collapse="/"))
    }

  } else {

    gdrive.gta_cloud <- "/.shortcut-targets-by-id/17FRvsAcpS6vSAA-JfUUBOS0ixF9FSuio/GTA cloud"

    if(!is.null(Sys.info())) {
      if(Sys.info()['sysname'] == "Darwin") {  # set wd for mac osx
        setwd(paste0(gdrive, gdrive.gta_cloud))
      } else {  # set wd for windows
        setwd(paste0(gdrive, ":", gdrive.gta_cloud))
      }
    } else {  # fallback to windows
      setwd(paste0(gdrive, ":", gdrive.gta_cloud))
    }

  }

}

