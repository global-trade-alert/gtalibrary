# Roxygen documentation

#' Check whether your HS codes are consistent with HS 2012.
#'
#' This function checks whether a vector of HS codes is consistent with HS 2012 and returns it as a vector of 6-digit level HS codes.
#'
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed. Values with more than 6 digits will be limited to 6. Please input the codes as integer.
#' @param message prints conversion results if TRUE
#' @import cli
#' @param as_list Returns a list with the same length as the codes vector with each list entry containing the converted codes for one supplied code
#' @examples
#' converting a vector of HS codes:
#' codes = c(1, 2, 3)
#' gta_hs:code_check(codes)
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_hs_code_check <- function(codes, as_list = FALSE, message = TRUE) {
    # Load HS names
    hs.names <- gtalibrary::hs.names

    # transform all hs codes to 6 digit strings
    hs.codes <- unique(hs.names$HS12code)
    hs.codes[nchar(hs.codes) %% 2 != 0] <- paste0("0", hs.codes[nchar(hs.codes) %% 2 != 0])

    # return error if codes cannot be converted to numeric without NA coercion (NAs are allowed)
    tryCatch(
        codes <- as.character(as.numeric(codes)),
        warning = \(w) {
            cli::cli_abort("{.var codes} must be convertible to a numeric vector", call = NULL)
        },
        error = \(e){
            cli::cli_abort("{.var codes} must be convertible to a numeric vector", call = NULL)
        }
    )

    # transform input codes with uneven number of charaters to even by adding leading 0
    codes[!is.na(codes) & nchar(codes) %% 2 != 0] <- paste0("0", codes[!is.na(codes) & nchar(codes) %% 2 != 0])

    # shorten codes which were longer than 6 characters & store truncated codes for warning output
    truncated.codes <- codes[nchar(codes) > 6 & !is.na(codes)]
    codes[nchar(codes) > 6 & !is.na(codes)] <- substr(codes[nchar(codes) > 6 & !is.na(codes)], 1, 6)

    # convert codes to 6 digit hs codes
    converted.codes <- lapply(
        cli::cli_progress_along(codes),
        \(x) {
            if (is.na(x)) {
                integer(0)
            } else {
                hs.codes[(x == substr(hs.codes, 1, 2) | x == substr(hs.codes, 1, 4) | x == hs.codes)]
            }
        }
    )

    # print message if message = TRUE
    if (message) {
        # retreive unconverted codes as those with empty list entries
        unconverted.codes <- sapply(
            converted.codes,
            \(x) all(x == 0)
        )

        if (length(truncated.codes > 0)) {
            cli::cli_alert_info(paste0(
                cli::style_bold("Codes with more than 6 figures provided. These will be reduced: "),
                paste0(as.numeric(truncated.codes), collapse = ", ")
            ))
        }
        if (!any(unconverted.codes) && all(!is.na(unconverted.codes))) {
            cli::cli_alert_success(cli::style_bold("Conversion successful. Returning vector of 6-digit HS codes."))
        } else {
            cli::cli_alert_warning(paste0(
                cli::style_bold("Non existing values provided: "),
                paste0(unique(as.numeric(codes[unconverted.codes])), collapse = ", ")
            ))
        }
    }

    # return all values as numeric
    if (as_list) {
        return(lapply(converted.codes, as.numeric))
    } else {
        # return only unique values if not within list
        return(unique(as.numeric(unlist(converted.codes))))
    }
}
