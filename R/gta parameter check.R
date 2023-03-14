# Roxygen documentation

#' Helper function that evaluates whether user input matches permissible values
#'
#' @usage gta_parameter_check(
#'      parameter,
#'      permissible_values,
#'      arg_name = NULL,
#'      warning = FALSE
#' )
#' @param parameter Specify the argument you want to check.
#' @param permissible_values Specify the permissible values.
#' @param warning specifies whether the ocurrence of a value in parameter which is not contained in permissible_values should give a warning or an error
#' gives an error by default (ie. Warning = FALSE)
#' @param arg_name Optional specification of the argument's name that should be printed in the error message.
#' @examples
#' parameter <- c("one", "TWO", "tHrEE")
#' permissible_values <- c("one", "two")
#' # if you do not specify the arg_name, tolower(parameter) will be printed as the arg name.
#' # To print the passed, unmodified name of the argument, specify: arg_name = parameter
#' gta_parameter_check(tolower(parameter), permissible_values)
#' @export
gta_parameter_check <- function(parameter, permissible_values, arg_name = NULL, warning = FALSE) {
    # check if all parameters are in permissible values
    if (!all(parameter %in% permissible_values)) {
        # if arg_name is not specified, take name of value passed to parameter
        if (is.null(arg_name)) {
            arg_name <- deparse(substitute(parameter))
        }
        # generate error message
        error_source <- paste(parameter[!parameter %in% permissible_values], collapse = ", ")
        error_msg <- glue::glue("Unknown values in {arg_name}: {paste(error_source, collapse = ', ')}")
        # warn user or abort with error message
        if (warning) {
            cli::cli_alert_warning(error_msg)
        } else {
            cli::cli_abort(error_msg)
        }
    }
}
