# Roxygen documentation

#' Get the latest version from GitHub.
#'
#' Syncs your local library with our latest GTA GitHub release.
#' @import devtools
#' @usage gta_update_library(NULL)
#' @return Be up to date with our latest functions.
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_update_library <- function(x) {
  devtools::install_github("global-trade-alert/gtalibrary", force = TRUE)
  print("You are up to date.")
}


