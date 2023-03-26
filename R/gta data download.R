# Roxygen documentation

#' Download the latest copy of the GTA database.
#'
#' This function downloads the latest copy of the full GTA database export and stores it locally.
#'
#' @param download.folder Set the download folder. Default is current working directory.
#' @param load.data Loads the downloaded data into the current environment. Default is FALSE.
#' @export
gta_data_download <- function(download.folder = NULL,
                              load.data = FALSE) {
    if (is.null(download.folder)) {
        download.path <- "GTA data.Rdata"
    } else {
        download.path <- gsub("/{2,}GTA data", "/GTA data", paste(download.folder, "GTA data.Rdata", sep = "/"))
    }

    download.file("https://www.dropbox.com/s/78kpe232p2b36ze/GTA%20full%20data%20export.Rdata?dl=1", download.path)
    if (load.data) {
        load("GTA data.Rdata")
    }
}
