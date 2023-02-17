# Roxygen documentation

#' This is a helper function that evaluates whether all user input is understood. Returns 'OK' if this is so and the unconforming values otherwise.
#'
#' @param check.vector Specify the vector you want to check.
#' @param permissible.values Specify the permissible values.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_parameter_check <- function(check.vector = NULL, permissible.values = NULL, check.name = NULL) {
    if (!all(check.vector %in% permissible.values)) {
        error_source <- paste(check.vector[!check.vector %in% permissible.values], collapse = ", ")
        error_msg <- glue::glue("Unknown values in {check.name}: {paste(error_source, collapse = ', ')}")
        cli::cli_abort(error_msg)
    }
}

#' @export
#' checks if a parameter of a function is of a desired value. if no, show error message
gta_type_check <- function(check.argument = NULL, check.function = NULL) {
    check <- tryCatch(
        expr = check.function(check.argument),
        error = function(e) cli::cli_abort("Please pass a valid check.function", call = NULL)
    )
    if (!is.logical(check)) {
        cli::cli_abort("Make sure that the check.function returns a boolean")
    }

    if (!check) {
        error_msg <- glue::glue("{1}")
        fun_name <- deparse(substitute(check.function))
        cli::cli_abort("{deparse(substitute(check.argument))} must fulfill: {fun_name}")
    }
}
