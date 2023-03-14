# Roxygen documentation

#' GTA pwd function
#'
#' Returns a list of GTA credentials.
#'
#' @param type If parameter type is specified, a single list will be returned. Otherwise all credentials will be returned. Default: All.

#' @return Outputs either a list of credentials for a specified app or all credentials available for GTA.
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_pwd <- function(type=NULL) {

  # Load stored credentials
  source("setup/pwd.R")

  if(is.null(type)) {
    eval(parse(text=paste0("creds <- list(",paste0(credentials," = ",credentials, collapse=", "),")")))
    return(creds)
  } else {
    if (! tolower(type) %in% tolower(credentials)) {
      stop("Credentials type not found")
    } else {
      eval(parse(text=paste0("creds <- ",credentials[tolower(credentials) == tolower(type)])))
      # eval(parse(text=paste0("rm(credentials,",paste(credentials, collapse=","),")")))
      return(creds)
    }
  }

  }
