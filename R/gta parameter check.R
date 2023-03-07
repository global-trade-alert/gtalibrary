# Roxygen documentation

#' This is a helper function that evaluates whether all user input is understood. Returns 'OK' if this is so and the unconforming values otherwise.
#'
#' @usage gta_parameter_check(
#'      parameter,
#'      permissible_values,
#'      arg_name = NULL,
#'      warning = FALSE
#' )
#' @param parameter Specify the vector you want to check.
#' @param permissible_values Specify the permissible values.
#' @param warning specifies whether the ocurrence of a value in parameter which is not contained in permissible_values should give a warning or an error
#' gives an error by default (ie. Warning = FALSE)
#' @param arg_name Optional specification of the argument's name that should be printed in the error message.
#' @example
#' parameter  <- c("one", "TWO", "tHrEE")
#' permissible_values  <- c("one", "two")
#' # if you do not specify the arg_name, tolower(parameter) will be printed as the arg name.
#' ```
#' gta_parameter_check(tolower(parameter), permissible_values)
#' ```
#' To print the passed, unmodified name of the argument, specify: arg_name = parameter
#' @import glue
#' @import cli
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#'
#' @export
gta_parameter_check <- function(parameter, permissible_values, arg_name = NULL, warning = FALSE) {
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
