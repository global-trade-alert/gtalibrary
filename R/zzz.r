.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        cli::cli_bullets(c("v" = "gtalibrary successfully loaded - have fun"))
    )
}
