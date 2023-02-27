# Roxygen documentation

#' Create a dataframe with CPC names out of a list of CPC codes.
#'
#' This function returns the names of CPC codes.
#'
#' @param codes Supply the CPC codes of which you want to get the name.
#' @param level Specify the level of the codes provided (2 or 3).
#' @usage gta_cpc_names(codes, levels = NULL)
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_cpc_names <- function(codes, level = NULL) {
    # check validity of parameters
    gta_parameter_check(level, c(2, 3))
    gta_parameter_check(codes, cpc_codes$cpc[cpc_codes$cpc.digit.level == level])

    # load relevant data frame
    cpc_codes <- tibble::as_tibble(gtalibrary::cpc.names)

    # filter data frame according to user input
    out <- cpc_codes |>
        dplyr::filter(cpc %in% codes, cpc.digit.level == level) |>
        dplyr::select(cpc, cpc.name) |>
        dplyr::rename(cpc_code = cpc, cpc_name = cpc.name)

    # return results
    return(out)
}
