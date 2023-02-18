# Roxygen documentation

#' Create a vector of UN country codes
#'
#' Returns a vector of codes based on country or group names.
#'
#' @param countries Vector with either country names, country group names or UN codes. Do not mix UN codes with country or group names. Please use function also for UN codes to ensure the ones you supply are consistent with the ones used by the GTA.
#' @param role This parameter tailors the error message e.g. "Unkown ROLE country value ...". Only useful if command is run for more than one set of countries.
#' @import cli
#' @import dplyr
#' @return A vector of UN country codes that is consistent with those used by the GTA.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

#' @export
gta_un_code_vector <- function(countries = NULL) {
    # un-code country name matrix
    country.correspondence <- gtalibrary::country.correspondence

    # if countries are not specified, return all the un codes
    if (is.null(countries)) {
        countries <- unique(country.correspondence$un_code)
    }

    # check if either (all numeric (un codes) or all string (country names))
    if (is.numeric(countries)) {
        permissible.values <- unique(country.correspondence$un_code)
        gtalibrary::gta_parameter_check(countries, permissible.values)
        un.codes <- countries
    } else if (is.character(countries)) {
        countries <- tolower(countries)
        permissible.values <- tolower(unique(country.correspondence$name))
        gtalibrary::gta_parameter_check(countries, permissible.values)
        un.codes <- country.correspondence |>
            dplyr::filter(tolower(name) %in% countries) |>
            dplyr::pull(un_code) |>
            unique()
    } else {
        cli::cli_abort("countries must either be vector of type character (names) or numeric (un codes)")
    }
    return(un.codes)
}
