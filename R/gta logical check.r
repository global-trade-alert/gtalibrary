
#' Check whether function arguments fulfill a logical conditoion
#' @usage
#' gta_logical_check(
#'      check_arg,
#'      check_function,
#'      error_msg = NULL
#' )
#' @param check_arg The argument / variable that you want to check
#' @param check_function A function that takes a scalar `check_arg` as an input and returns a boolean
#' If you pass internal functions (eg. is.numeric) etc., pass them without brackets.
#' You can also pass anonymous functions (eg. \(x) (is.numeric(x) & length(x) >= 10))
#' @param error_msg Message that is printed if check_function returns FALSE. Glue Syntax (ie. passing of an expression
#' which should be evaluated with {} is supported)
#' @example
#' # 1.) you want to make sure that a is numeric, and b is numeric and larger than 0
#' example_function(a, b){
#' gta_is_logical(a, is.numeric, error_msg = "{.var a} must be logical")
#' gta_is_logical(b, \(x) is.numeric(x) & x > 0, error_msg = "{.var b} must be larger than 10")
#' return(log(a) + b)
#' }
#' @import cli
#' @import glue
#' @author Global Trade Alert
#' @export
gta_logical_check <- function(check_arg, check_function, error_msg = NULL) {
    check <- tryCatch(
        expr = check_function(check_arg),
        error = function(e) cli::cli_abort("Please pass a valid check_function", call = NULL)
    )

    if (!is.logical(check)) cli::cli_abort("Make sure that the check_function returns a boolean")

    if (!all(check)) {
        if (is.null(error_msg)) {
            check_function_name <- deparse(substitute(check_function))
            error_msg <- "{deparse(substitute(check_arg))} must fulfill: {check_function_name}"
        }
        cli::cli_abort(error_msg)
    }
}
