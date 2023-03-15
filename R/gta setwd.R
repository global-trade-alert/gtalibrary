# Roxygen documentation

#' Sets your working directory to that used by the GTA data team.
#' @return A common working directory
#' @export
gta_setwd <- function(gdrive = NA) {
    if (is.na(gdrive)) {
        split_path <- unlist(strsplit(getwd(), "/"))
        cld_position <- which(split_path == "GTA root")
        if (length(cld_position) == 0) cld_position <- which(split_path == "GTA cloud")
        if (cld_position < length(split_path)) {
            setwd(paste(rep("..", length(split_path) - cld_position), collapse = "/"))
        }
    } else {
        gdrive_gta_cloud <- "/.shortcut-targets-by-id/17FRvsAcpS6vSAA-JfUUBOS0ixF9FSuio/GTA cloud"
        if (!is.null(Sys.info())) {
            if (Sys.info()["sysname"] == "Darwin") { # set wd for mac osx
                setwd(paste0(gdrive, gdrive_gta_cloud))
            } else if (Sys.info()["sysname"] == "Linux") {
                setwd(gdrive)
            } else { # set wd for windows
                setwd(paste0(gdrive, ":", gdrive_gta_cloud))
            }
        } else { # fallback to windows
            setwd(paste0(gdrive, ":", gdrive_gta_cloud))
        }
    }
}
