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
#'
#' @export
gta_parameter_check <- function(parameter = NULL, permissible_values = NULL, arg_name = NULL, warning = FALSE) {
    if (!all(parameter %in% permissible_values)) {
        if (is.null(arg_name)) {
            arg_name <- deparse(substitute(parameter))
        }

        error_source <- paste(parameter[!parameter %in% permissible_values], collapse = ", ")
        error_msg <- glue::glue("Unknown values in {.var arg_name}: {paste(error_source, collapse = ', ')}")

        if (warning) {
            cli::cli_alert_warning(error_msg)
        } else {
            cli::cli_abort(error_msg)
        }
    }
}
