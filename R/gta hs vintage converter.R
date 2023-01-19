# Roxygen documentation

#' Convert HS codes of unkown vintage into HS 2012.
#'
#' Returns it as a vector of 6-digit level HS 2012 codes.
#'
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed. Values with more than 6 digits will be limited to 6. Please input the codes as integer.
#' @param years State the origin vintage or the vintages which should be tested. Options inclue 1992, 1996, 2002, 2007, 2012, 2017, 2022. Default is (2002, 2007, 2012, 2017)
#' @param origin State whether codes shold be converted based on the year where most matches exist (best) or codes can be converted from multiple years (any). Default = "best"
#' @param as_list returns the results as a list with the same length as the codes input vector, FALSE by default to ensure compatability with earlier version
#' @param message Prints conversion results if TRUE
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_hs_vintage_converter <- function(codes, years = c(2002, 2007, 2012, 2017), as_list = FALSE, message = TRUE, origin = "best") {
    # return error if codes cannot be converted to numeric without NA coercion (NAs are allowed)
    tryCatch(
        codes <- as.numeric(codes),
        warning = \(w) {
            cli::cli_abort("{.var codes} must be convertible to a numeric vector", call = NULL)
        },
        error = \(e){
            cli::cli_abort("{.var codes} must be convertible to a numeric vector", call = NULL)
        }
    )

    # make sure that supplied years are valid
    if (!all(years %in% c(1992, 1996, 2002, 2007, 2012, 2017, 2022))) {
        cli::cli_abort(cli::style_bold("Invalid year provided. Make sure that year is in (1992, 1996, 2002, 2007, 2012, 2017, 2022)"))
    }

    # pad codes that do not have an even character length
    codes[!is.na(codes) & nchar(codes) %% 2 != 0] <- paste0("0", codes[!is.na(codes) & nchar(codes) %% 2 != 0])

    # store codes that were truncated to return in warning and truncate codes to max 6 characters length
    truncated.codes <- codes[nchar(codes) > 6 & !is.na(codes)]
    codes[nchar(codes) > 6 & !is.na(codes)] <- substr(codes[nchar(codes) > 6 & !is.na(codes)], 1, 6)

    # Load HS vintages"C:\Users\sveng\Downloads\hs_conversion_table.xlsx"
    hs.vintages <- readr::read_csv("C:/Users/sveng/Downloads/hs_conversion_table.csv")

    # hs.conversion.table <- gtalibrary::hs.conversion.table

    # assumption: the entire codes vector is from the same HS year
    # matches (for all years which should be tested)

    # calculate the number of maches of the codes in each HS year that is to be tested
    if (origin == "best") {
        matches <- vector()
        for (i in seq_len(length(years))) {
            year <- paste0("HS", years[i])
            matches[i] <-
                sum((substr(hs.vintages[[year]], 1, 2) %in% codes | substr(hs.vintages[[year]], 1, 4) %in% codes | hs.vintages[[year]] %in% codes), na.rm = TRUE)
        }

        # select the hs year (max year with the highest number of matches)
        use.hs <- max(years[matches == max(matches)])
        hs.origin <- hs.vintages[[paste0("HS", use.hs)]]

        # convert the codes
        codes.converted <- lapply(
            codes,
            \(x) {
                matches <- (x == substr(hs.origin, 1, 2) | x == substr(hs.origin, 1, 4) | x == hs.origin)
                converted <- unique(hs.vintages[["HS2012"]][matches])
                converted <- converted[!is.na(converted)]
                return(converted)
            }
        )
    }

    if (origin == "any") {
        # select years which should be tested
        years <- paste0("HS", years)

        # add base year in the first place, to exclude in pivot_longer()
        hs.vintages <- hs.vintages[c("HS2012", years)]
        colnames(hs.vintages)[1] <- "BASE"
        hs.vintages <- tidyr::pivot_longer(hs.vintages, cols = !BASE, names_to = "YEAR", values_to = "HS6")
        hs.origin <- hs.vintages[["HS6"]]

        # convert the codes
        codes.converted <- lapply(
            codes,
            \(x) {
                matches <- (x == substr(hs.origin, 1, 2) | x == substr(hs.origin, 1, 4) | x == hs.origin)
                converted <- unique(hs.vintages[["BASE"]][matches])
                converted <- converted[!is.na(converted)]
                return(converted)
            }
        )
    }

    # only print information if message = TRUE
    if (message) {
        not.converted <- sapply(codes.converted, \(x) all(x == 0))

        if (length(truncated.codes) != 0) {
            cli::cli_alert_warning(paste0(
                cli::style_bold("The following codes were cut to 6 characters: "),
                paste0(as.numeric(truncated.codes), collapse = ", ")
            ))
        }
        if (!any(not.converted)) {
            cli::cli_alert_success(cli::style_bold("All supplied codes were matched."))
        } else {
            cli::cli_alert_warning(paste0(
                cli::style_bold("Could not match the following codes: "),
                paste0(as.numeric(codes[not.converted]), collapse = ", ")
            ))
        }
    }

    # return values
    if (as_list) {
        return(lapply(codes.converted, as.numeric))
    } else {
        return(unique(as.numeric(unlist(codes.converted))))
    }
}
