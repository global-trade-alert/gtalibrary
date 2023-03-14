# Roxygen documentation

#' Convert HS codes of unkown vintage into HS6 2012.
#'
#' `gta_hs_vintage_converter()` takes a vector of HS codes of any vintage
#' as an input and converts them to 6-digit HS2012 codes.
#'
#' Available vintages to convert from are: 1992, 1996, 2002, 2007, 2017, 2022.
#' Input codes can be of any length (ie. HS2, HS4, ...). Input codes longer than 6 digits
#' are truncated to 6 digits.
#' @usage
#' gta_hs_vintage_converter(
#'     codes,
#'     years = c(1992, 1996, 2002, 2007, 2012, 2017, 2022),
#'     as_list = FALSE,
#'     message = TRUE
#' )
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed.
#' Values with more than 6 digits will be limited to 6.
#' codes can be of type numeric or character
#' @param years State the origin vintage years which should be tested as candidates to convert from.
#' Options inclue 2002, 2007, 2012, 2017, 2022. Default is (2002, 2007, 2012, 2017).
#' @param as_list if TRUE, returns the result as a list of the same length as `codes`.
#' If FALSE, a vector of unique converted HS codes is returned
#' @param message if TRUE, conversion results are printed to the console.
#' This includes: The unique vintage year used for conversion (only if `orign` != "any"),
#' possible codes that were truncated to 6 digits and the codes that could not be converted.
#' @examples
#' # If you wish to convert every HS code into HS_2012 and append the result to an
#' # existing data frame, the as_list comes in handy. Use \code{dplyr::mutate()} to
#' # add a new row with the converted codes, and then use the \code{tidyr::unnest()} function.
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
#' @references
#' The conversion Matrix underlying this function is stored in \code{`gtalibrary::hs_vintages`}
#' The data was obtained from: https://unstats.un.org/unsd/classifications/Econ
#' @export
gta_hs_vintage_converter <- function(codes, years = c(1992, 1996, 2002, 2007, 2012, 2017), as_list = FALSE, message = TRUE) {
    # check validity of function arguments
    gta_logical_check(as_list, is.logical)
    gta_logical_check(message, is.logical)
    gta_parameter_check(years, c(1992, 1996, 2002, 2007, 2012, 2017, 2022))

    # pad codes that do not have an even character length with leading 0
    codes <- stringr::str_pad(codes, width = ceiling(nchar(codes) / 2) * 2, pad = "0", side = "left")

    # store codes that were truncated to return in warning and truncate codes to max 6 characters length
    truncated_codes <- codes[nchar(codes) > 6 & !is.na(codes)]
    codes[nchar(codes) > 6 & !is.na(codes)] <- substr(codes[nchar(codes) > 6 & !is.na(codes)], 1, 6)
    hs_vintages <- gtalibrary::hs_vintages

    # calculate the number of maches of the codes in each HS year that is to be tested
    matches <- vector()
    for (i in seq_len(length(years))) {
        year <- paste0("HS", years[i])
        matches[i] <-
            sum(unique(codes) %in% substr(hs_vintages[[year]], 1, 2) |
                unique(codes) %in% substr(hs_vintages[[year]], 1, 4) |
                unique(codes) %in% hs_vintages[[year]], na.rm = TRUE)
    }

    # select the hs year used for conversion (max year with the highest number of matches)
    use_hs <- max(years[matches == max(matches)])
    hs_origin <- paste0("HS", use_hs)

    # call c++ function for  more efficient calculation and pass unique HS2012 / HS_ORIGIN pairs
    passed_df <- hs_vintages |>
        dplyr::select(HS2012, hs_origin) |>
        tidyr::drop_na() |>
        dplyr::distinct()

    codes_conversion <- gta_code_converter_cpp(codes_2012 = passed_df$HS2012, codes_vintage = passed_df[[hs_origin]], codes = codes)

    # prepare console output if message = TRUE
    if (message) {
        not_converted <- codes_conversion$unconverted

        # print if codes were truncated
        if (length(truncated_codes) != 0) {
            cli::cli_alert_warning("The following codes were cut to 6 characters: {unique(truncated_codes)}", wrap = TRUE)
        }

        # print if all codes were converted
        if (length(not_converted) == 0) {
            cli::cli_alert_success("All supplied codes were matched.")

            # print if some codes were not converted
        } else {
            cli::cli_alert_warning("Could not match {length(unique(not_converted)) / length(codes)} codes: {unique(not_converted)}", wrap = TRUE)
        }
        # print year used for conversion if at least one code was converted
        if (!(length(not_converted) == length(codes))) {
            cli::cli_alert_info("Year used for conversion: {use_hs}")
        }
    }

    # return values
    if (as_list) {
        return(codes_conversion$converted)
    } else {
        return(unique(unlist(codes_conversion$converted)))
    }
}
