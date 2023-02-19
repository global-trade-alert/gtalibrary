# Roxygen documentation

#' This is a helper function that evaluates whether all user input is understood. Returns 'OK' if this is so and the unconforming values otherwise.
#'
#' @param check.vector Specify the vector you want to check.
#' @param permissible.values Specify the permissible values.
#' @param warning specifies whether the ocurrence of a value in parameter which is not contained in permissible_values should give a warning or an error
#' gives an error by default (ie. Warning = FALSE)
#' @import glue
#' @import cli
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_parameter_check <- function(parameter = NULL, permissible_values = NULL, arg_name = NULL, warning = FALSE) {
    if (!all(parameter %in% permissible_values)) {
        if (is.null(arg_name)) {
            arg_name <- deparse(substitute(parameter))
        }

        error_source <- paste(parameter[!parameter %in% permissible_values], collapse = ", ")
        error_msg <- glue::glue("Unknown values in {arg_name}: {paste(error_source, collapse = ', ')}")

        if (warning) {
            cli::cli_alert_warning(error_msg)
        } else {
            cli::cli_abort(error_msg)
        }
    }
}

#' @export
gta_logical_check <- function(check_arg = NULL, check_function = NULL, error_msg = NULL) {
    check <- tryCatch(
        expr = check_function(check_arg),
        error = function(e) cli::cli_abort("Please pass a valid check_function", call = NULL)
    )

    if (!is.logical(check)) cli::cli_abort("Make sure that the check_function returns a boolean")

    if (!check) {
        if (is.null(error_msg)) {
            check_function_name <- deparse(substitute(check_function))
            error_msg <- "{deparse(substitute(check_arg))} must fulfill: {check_function_name}"
        }
        cli::cli_abort(error_msg)
    }
}
