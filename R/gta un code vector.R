# Roxygen documentation

#' Create a vector of UN country codes
#' Returns a vector of codes based on country or group names.
#'
#' @usage gta_un_code_vector(countries = "all")
#' @param countries Vector with either country names, country group names or UN codes. Do not mix UN codes with country or group names. Please use function also for UN codes to ensure the ones you supply are consistent with the ones used by the GTA.
#' @return A vector of UN country codes that is consistent with those used by the GTA.

#' @export
gta_un_code_vector <- function(countries = "all") {
  # check wheter input is numeric or character (only permissible types)
  gta_logical_check(
    countries,
    \(x) is.numeric(x) | is.character(x),
    error_msg = "countries must either be vector of type character (names) or numeric (un codes)"
  )

  # un-code country name matrix
  country.correspondence <- gtalibrary::country.correspondence

  # if countries are not specified, return all the un codes
  if (countries == "all") {
    countries <- unique(country.correspondence$un_code)
  }

  # check if either (all numeric (un codes) or all string (country names))
  if (is.numeric(countries)) {
    permissible.values <- unique(country.correspondence$un_code)
    gta_parameter_check(countries, permissible.values)
    un.codes <- countries
  } else {
    countries <- tolower(countries)
    permissible.values <- tolower(unique(country.correspondence$name))

    gta_parameter_check(countries, permissible.values)
    un.codes <- country.correspondence |>
      dplyr::filter(tolower(name) %in% countries) |>
      dplyr::pull(un_code) |>
      dplyr::distinct()
  }

  # return output
  return(un.codes)
}
