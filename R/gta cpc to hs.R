# Roxygen documentation
Rcpp::sourceCpp("c:\\Users\\sveng\\OneDrive\\Dokumente\\GitHub\\GTA\\gtalibrary\\src\\cpc_to_hs_CPP.cpp")
#' Convert CPC 2.1 codes into HS 2012 (6-digit level).
#'
#' This function converts vectors of CPC 2.1 codes (either 2 or 3 digit level) into a vector of 6-digit HS 2012 codes.
#' 2 and 3 digit codes cannot be mixed as interpreting which level is ment can be ambiguous (eg. input of 17 could be "017" or "17" respectively)
#' @usage gta_cpc_to_hs(
#'      codes,
#'      cpc_digits = c(2, 3),
#'      as_list = FALSE,
#'      message = TRUE
#' )
#' @param codes Supply the CPC codes you want to convert. Character and numeric is accepted.
#' @param cpc_digits Indicate whether the codes passed are 2 or 3 digit cpc codes
#' @param as_list Indicate whether the output is a vector of uniquely converted codes (False) or as a list with each ... (copy from hs_vintage_converter())
#' @param message If TRUE, print conversion results, including the codes that could not be converted
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_cpc_to_hs <- function(codes, cpc_digits = 3, as_list = FALSE, message = TRUE) {
    # check arguments
    gta_logical_check(as_list, is.logical)
    gta_logical_check(message, is.logical)
    gta_parameter_check(cpc_digits, c(2, 3))

    if (cpc_digits == 3) {
        gta_logical_check(codes, \(x) nchar(x) %in% c(2, 3), error_msg = "{.var codes} must be 2 or 3 digits/characters long")
    } else {
        gta_logical_check(codes, \(x) nchar(x) %in% c(1, 2), error_msg = "{.var codes} must be 1 or 2 digits/characters long")
    }

    # convert cpc codes to string and add potentially leading 0s
    codes <- stringr::str_pad(codes, cpc_digits, pad = "0", side = "left")

    # Load CPC/HS conversion matrix
    # cpc_hs_matrix <- gtalibrary::cpc_to_hs
    cpc_hs_matrix <- readRDS("c:\\Users\\sveng\\OneDrive\\Dokumente\\GitHub\\GTA\\gtalibrary\\data\\cpc_to_hs.rda")

    # prepare input for c++ function
    if (cpc_digits == 2) {
        # gives unique CPC(2digits)/ HS combinations
        passed_df <- cpc_hs_matrix |>
            dplyr::mutate(CPC_21 = stringr::str_sub(CPC_21, 1, 2)) |>
            tidyr::drop_na() |>
            dplyr::distinct()
    } else {
        # unique CPC (3digit)/ HS combinations
        passed_df <- cpc_hs_matrix
    }

    # convert codes with cpc_to_hs_CPP function
    codes_conversion <- cpc_to_hs_CPP(cpc_codes = passed_df$CPC_21, hs_2012_codes = passed_df$HS_2012, codes = codes)

    if (message) {
        # extract non-converted codes
        not.converted <- codes_conversion$unconverted

        if (length(not.converted) == 0) {
            cli::cli_alert_success(cli::style_bold("All supplied codes were matched."))
        } else {
            cli::cli_alert_warning(cli::style_bold("Could not match {length(unique(not.converted)) / length(codes)} codes: {unique(not.converted)}"), wrap = TRUE)
        }
    }

    # return values
    if (as_list) {
        return(codes_conversion$converted)
    } else {
        return(unique(unlist(codes_conversion$converted)))
    }
}

cpc_hs_matrix <- readr::read_csv("https://unstats.un.org/unsd/classifications/Econ/tables/CPC/CPCv21_HS12/cpc21-hs2012.txt")

cpc_hs_matrix <- cpc_hs_matrix |>
    dplyr::select(CPC21code, HS12code) |>
    dplyr::mutate(
        HS12code = str_remove(HS12code, "\\."),
        CPC21code = str_extract(CPC21code, pattern = "\\d{3}")
    ) |>
    dplyr::rename(HS_2012 = HS12code, CPC_21 = CPC21code) |>
    dplyr::distinct()
