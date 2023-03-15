#' Check whether function arguments fulfill a logical conditoion
#' 
#' @usage
#' gta_logical_check(
#'      check_arg,
#'      check_function,
#'      error_msg = NULL
#' )
#' @param check_arg The argument / variable that you want to check
#' @param check_function A function that takes a scalar `check_arg` as an input and returns a boolean
#' If you pass internal functions (eg. \code{is.numeric}), pass them without brackets.
#' You can also pass anonymous functions (eg. \code{\(x)}  \code{is.numeric(x) & length(x) >= 10)})
#' @param error_msg Message that is printed if check_function returns FALSE. Glue Syntax (ie. passing of an expression
#' which should be evaluated with {} is supported)
#' @examples
#' # 1.) you want to make sure that a is numeric, and b is numeric and larger than 0
#' example_function(a, b){
#' gta_is_logical(a, is.numeric, error_msg = "{.var a} must be logical")
#' gta_is_logical(b, \(x) is.numeric(x) & x > 0, error_msg = "{.var b} must be larger than 10")
#' return(log(a) + b)
#' }
gta_logical_check <- function(check_arg, check_function, error_msg = NULL) {
    # check if check_function can be evaluated
    check <- tryCatch(
        expr = check_function(check_arg),
        error = function(e) cli::cli_abort("Please pass a valid check_function", call = NULL)
    )

    # check if check_function returns a boolean (or a vector of booleans)
    if (!is.logical(check)) cli::cli_abort("Make sure that the check_function returns a boolean")

    # error message if not all booleans are true
    if (!all(check)) {
        # return base message if no specific error message is defined
        if (is.null(error_msg)) {
            check_function_name <- deparse(substitute(check_function))
            error_msg <- "{deparse(substitute(check_arg))} must fulfill: {check_function_name}"
        }
        # return user specific error message defined in error_msg
        cli::cli_abort(error_msg)
    }
}
