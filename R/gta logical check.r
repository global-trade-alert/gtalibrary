#' @export
gta_logical_check <- function(check_arg = NULL, check_function = NULL, error_msg = NULL) {
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
