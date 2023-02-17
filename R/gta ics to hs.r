# Roxygen documentation

#' Convert ICS codes to 4 digit HS codes
#'
#' @param codes Supply the ICS codes you want to check. Only codes in 5 digit format are converted (eg. 67120 / 67.120). Shorter codes are ignored, longer codes are truncated to 5 digits
#' @param as_list returns the results as a list with the same length as the codes input vector, FALSE by default
#' @param message Prints conversion results if TRUE
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @import cli
#' @export
gta_ics_to_hs <- function(codes, as_list = FALSE, message = TRUE) {
    # Load conversion table
    ics.to.hs <- gtalibrary::ics.to.hs

    hs_ics <- readxl::read_xlsx("C:/Users/sveng/Downloads/ICS_HS_mapping_table.xlsx")

    save(file = "ics.to.hs.rda", hs_ics)

    load("")
    hs_ics <- hs_ics
    ics.to.hs <- hs_ics
    # remove potential peridods in the supplied codes and in the conversion table
    codes <- gsub(pattern = "\\.", replacement = "", codes)
    ics.to.hs$ics <- sapply(ics.to.hs[["ics"]], \(x) gsub(pattern = "\\.", replacement = "", x))

    ics.to.hs |>
        filter(ics == "67120") |>
        view()
    # return error if codes cannot be converted to numeric without NA coercion (NAs are allowed)
    tryCatch(
        codes <- as.character(as.numeric(codes)),
        warning = \(w) {
            cli::cli_abort("{.var codes} must be convertible to numeric or XX.XXX / XX.XXX.XX", call = NULL)
        },
        error = \(e){
            cli::cli_abort("{.var codes} must be convertible to numeric or XX.XXX / XX.XXX.XX", call = NULL)
        }
    )

    # shorten codes which were longer than 5 characters & store truncated codes for warning output
    ignored.codes <- codes[nchar(codes) < 5 & !is.na(codes)]
    truncated.codes <- codes[nchar(codes) > 5 & !is.na(codes)]
    codes[nchar(codes) > 5 & !is.na(codes)] <- substr(codes[nchar(codes) > 5 & !is.na(codes)], 1, 5)

    # convert codes to 5 digit ics codes
    converted.codes <- lapply(
        codes,
        \(x) {
            if (is.na(x)) {
                integer(0)
            } else {
                ics.to.hs$hs4[(x == ics.to.hs$ics)]
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
                cli::style_bold("Codes with more than 5 figures provided. These will be reduced: "),
                paste0(unique(as.numeric(truncated.codes)), collapse = ", ")
            ))
        }
        if (length(ignored.codes > 0)) {
            cli::cli_alert_info(paste0(
                cli::style_bold("Codes with less than 5 figures provided. These will be ignored: "),
                paste0(unique(as.numeric(ignored.codes)), collapse = ", ")
            ))
        }
        if (!any(unconverted.codes) && all(!is.na(unconverted.codes))) {
            cli::cli_alert_success(cli::style_bold("Conversion successful. Returning vector of 4-digit HS codes."))
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
