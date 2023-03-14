# Roxygen documentation

#' Get the latest gtalibrary version from GitHub.
#'
#' Syncs your local library with our latest GTA GitHub release.
#'
#' @usage
#' gta_update_library(branch = master)
#' @export
gta_update_library <- function(branch = "master") {
  repo_location <- "global-trade-alert/gtalibrary"

  # check if branch deviates from master
  if (branch != "master") {
    repo_location <- paste("global-trade-alert/gtalibrary", branch, sep = "@")
  }

  devtools::install_github(repo_location, force = TRUE)
  cli::cli_alert_success("You are up to date")
}
