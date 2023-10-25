#' Convert country names to GTA nomenclature
#'
#' \code{gta_clean_country_names()} takes a vector of country names
#' as an input and converts them to the country names used in the GTA database.
#'
#' @details
#' If a country cannot or cannot uniquely be matched to one country in the gta nomenclature,
#' NA is returned.
#' If you want a table with unique country names from the input vector, the name
#' they were converted to and information on whether the function changed the name, set conversionTable
#' to true. This will output a tibble with these three columns.
#' @usage
#' gta_clean_country_names(
#'     country,
#'     conversionTable = FALSE
#' )
#' @param country a vector of country names
#' @param conversionTable set to TRUE if the function should return a conversion table
#' which shows how each unique value in `country` is converted by the function
#' @examples 
#' a_mess <- tibble::tible(
#'      country = c(
#'          "Korea, the one in the north",
#'          "Korea, this time in the south",
#'          "Burma", 
#'          "Germany (EU Country)", 
#'          "Bosnia", "
#'           "USA", 
#'     )
#' 
#' # a_mess |> 
#'      mutate(clean_names = gta_clean_country_names(country))
#' 
#' @references
#' The function uses gtalibrary::country_regex which contains a regex for each
#' country which inputs are matched against.
#' @export
gta_clean_country_names <- function(country, conversionTable = FALSE) {
    # only analyze unique input values and then use df join to output converted
    # vector of original length (faster, since join is implemented in C)
    country_distinct <- unique(country)
    match_sheet <- gtalibrary::country_regex
    matching_table <- tibble::tibble(input_country = NA, gta_country = NA, name_changed = NA)
    matched <- vector()

    country_cleaned <- stringr::str_remove_all(country_distinct, "[:punct:]|\\s") |> tolower()
    index <- 1 # used to access uncleaned values faster
    # apply to every value in country
    for (i in country_cleaned) {
        result <- match_sheet$gta_country[stringr::str_detect(i, match_sheet$regex)]
        # no match if country is ambiguous
        if (length(result) > 1 | length(result) == 0) {
            matched <- append(matched, NA_character_)
            matching_table <- matching_table |>
                tibble::add_row(gta_country = NA_character_, input_country = country_distinct[index])
        } else {
            matched <- append(matched, result)
            if (tolower(country_distinct[index]) == tolower(result)) {
                matching_table <- matching_table |>
                    tibble::add_row(gta_country = result, input_country = country_distinct[index], name_changed = FALSE)
            } else {
                matching_table <- matching_table |>
                    tibble::add_row(gta_country = result, input_country = country_distinct[index], name_changed = TRUE)
            }
        }
        index <- index + 1
    }

    if (conversionTable) {
        out <- matching_table
    } else {
        out <- tibble::tibble(country_distinct = country_distinct, country_gta = matched) |>
            dplyr::right_join(tibble::tibble(country), by = dplyr::join_by(country_distinct == country)) |>
            dplyr::pull(country_gta)
    }
    return(out)
}

gta_clean_country_names(c("uganda", "uganda", "uganda"))
