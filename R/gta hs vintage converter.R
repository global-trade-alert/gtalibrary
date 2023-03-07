# Roxygen documentation

#' Convert HS codes of unkown vintage into HS6 2012.
#'
#' `gta_hs_vintage_converter()` takes a vector of HS codes of any vintage
#' as an input and converts them to 6-digit HS2012 codes. Available vintages to convert
#' from are: 1992, 1996, 2002, 2007, 2017, 2022
#' Input codes can be of any length (ie. HS2, HS4, ...). Input codes longer than 6 digits
#' are truncated to 6 digits.
#' @usage
#' gta_hs_vintage_converter(
#'     hs_codes,
#'     years = c(1992, 1996, 2002, 2007, 2012, 2017, 2022),
#'     as_list = FALSE,
#'     message = TRUE
#' )
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed.
#' Values with more than 6 digits will be limited to 6.
#' codes can be of type numeric or character
#' @param match_type If "best", the function checks which of the HS vintage years specified in `years`
#' produces the most matches with the supplied codes in `codes`.
#' This year is then used to convert all supplied codes. If "any", codes in `code` can be converted based on
#' all vintages specified in `years`. Eg. if "XXXXXX" in 2007 translates to HS2012 "YYYYYY" and "XXXXXX" also exists
#' in HS2002 but translates to "ZZZZZZ" in HS2012, both "YYYYYY" and "ZZZZZZ" are returned.
#' Default value is "best".
#' @param years State the origin vintage years which should be tested as candidates to convert from.
#' Options inclue 2002, 2007, 2012, 2017, 2022. Default is (2002, 2007, 2012, 2017).
#' @param as_list if TRUE, returns the result as a list of the same length as `codes`.
#' If FALSE, a vector of unique converted HS codes is returned
#' @param message if TRUE, conversion results are printed to the console.
#' This includes: The unique vintage year used for conversion (only if `orign` != "any"),
#' possible codes that were truncated to 6 digits and the codes that could not be converted.
#' @examples
#' # If you wish to convert every HS code into HS_2012 and append the result to an
#' # existing data frame, the as_list comes in handy. Use `dplyr::mutate()` to
#' # add a new row with the converted codes, and then use the `tidyr::unnest()` function.
#' df <- tibble(
#'     HS_unknown_vintage = c("010123", "02", "0345", "010101"),
#'     Country = c("USA", "CH", "DE", "CAN")
#' )
#'
#' df |>
#'     dplyr::mutate(HS_2012 = gta_hs_vintage_converter(codes = HS_2007, as_list = TRUE, years = c(2002, 2007, 2012, 2017))) |>
#'     tidyr::unnest(cols = HS_2012, keep_empty = TRUE) # keep_empty keeps the columns where the HS code could not be converted
#'
#' # If you know that the HS codes which you want to convert are from a specific year (eg. 2007), you
#' # can simply adjust the argument `years` and only include 2007. This ensures that all codes are converted
#' # form the HS2007 vintag
#' @import cli
#' @import tidyr
#' @references
#' The conversion Matrix underlying this function is stored in `gtalibrary::hs_vintages`
#' The data was obtained from: https://unstats.un.org/unsd/classifications/Econ
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_hs_vintage_converter <- function(codes, years = c(1992, 1996, 2002, 2007, 2012, 2017), as_list = FALSE, message = TRUE, match_type = "best") {
    # check that codes are either numeric or character vectors or scalars
    gta_logical_check(as_list, is.logical)
    gta_logical_check(message, is.logical)
    gta_parameter_check(origin, c("best", "any"))
    gta_parameter_check(years, c(1996, 2002, 2007, 2012, 2017, 2022))

    # pad codes that do not have an even character length
    codes <- stringr::str_pad(codes, width = ceiling(nchar(codes) / 2) * 2, pad = "0", side = "left")

    # store codes that were truncated to return in warning and truncate codes to max 6 characters length
    truncated.codes <- codes[nchar(codes) > 6 & !is.na(codes)]
    codes[nchar(codes) > 6 & !is.na(codes)] <- substr(codes[nchar(codes) > 6 & !is.na(codes)], 1, 6)

    # Load HS vintages (data table stored in this folder) /tempoary fix until on github
    hs.vintages <- gtalibrary::hs.vintages

    # calculate the number of maches of the codes in each HS year that is to be tested
    if (origin == "best") {
        matches <- vector()
        for (i in seq_len(length(years))) {
            year <- paste0("HS", years[i])
            matches[i] <-
                sum(unique(codes) %in% substr(hs.vintages[[year]], 1, 2) |
                    unique(codes) %in% substr(hs.vintages[[year]], 1, 4) |
                    unique(codes) %in% hs.vintages[[year]], na.rm = TRUE)
        }

        # select the hs year (max year with the highest number of matches)
        use.hs <- max(years[matches == max(matches)])
        hs.origin <- paste0("HS", use.hs)

        # call c++ function for much more efficient calculation (combinations must be unique)
        passed_df <- hs.vintages[c("HS2012", hs.origin)] |>
            tidyr::drop_na() |>
            dplyr::distinct()
        codes_conversion <- gta_code_converter_cpp(codes_2012 = passed_df$HS2012, codes_vintage = passed_df[[hs.origin]], codes = codes)
    }

    if (origin == "any") {
        # select years which should be tested
        years <- paste0("HS", years)

        # add base year in the first place, to exclude in pivot_longer()
        passed_df <- hs_vintages[c("HS2012", years)]
        colnames(passed_df)[1] <- "BASE"
        passed_df <- tidyr::pivot_longer(passed_df, cols = !HS2012, names_to = "YEAR", values_to = "HS6") |>
            dplyr::select(BASE, HS6) |>
            dplyr::distinct()

        codes_conversion <- gta_code_converter_cpp(codes_2012 = passed_df$BASE, codes_vintage = passed_df$HS6, codes = codes)
    }

    if (message) {
        not.converted <- codes_conversion$unconverted
        if (length(truncated.codes) != 0) {
            cli::cli_alert_warning(cli::style_bold("The following codes were cut to 6 characters: {unique(truncated.codes)}"), wrap = TRUE)
        }
        if (length(not.converted) == 0) {
            cli::cli_alert_success(cli::style_bold("All supplied codes were matched."))
        } else {
            cli::cli_alert_warning(cli::style_bold("Could not match {length(unique(not.converted)) / length(codes)} codes: {unique(not.converted)}"), wrap = TRUE)
        }

        if (!(length(not.converted) == length(codes))) {
            cli::cli_alert_info("Year used for conversion: {use.hs}")
        }
    }

    # return values
    if (as_list) {
        return(codes_conversion$converted)
    } else {
        return(unique(unlist(codes_conversion$converted)))
    }
}
