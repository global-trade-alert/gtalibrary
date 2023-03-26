# Roxygen documentation

#' Update your local copy of our data set.
#'
#' @return Be up to date with our latest data.
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_cloud_update <- function() {
    library(httr)
    zip.file.name <- "cloud update.zip"
    print("Be sure to set your working directory to your local GTA folder.")

    user.pwd <- .rs.askForPassword("Please enter your password to start the download.")

    source("setup/gta_cloud_download.R")
    if (user.pwd == download.passphrase) {
        GET("https://www.dropbox.com/s/znk3kcmohbp36s5/GTA%20cloud.zip?dl=1", write_disk(zip.file.name, overwrite = TRUE))
        unzip(zip.file.name)
        unlink(zip.file.name)
    } else {
        print("Wrong password.")
    }
}
