gta_clean_country_names <- function(country, conversionTable = FALSE) {
    # optimize with hash map or dict in the future
    match_sheet <- gtalibrary::match_sheet
    matching_table <- tibble::tibble(input_country = NA, gta_country = NA, name_changed = NA)
    not_matched <- vector()
    matched <- vector()

    country_cleaned <- stringr::str_replace_all(country, "[:punct:]", " ") |> tolower()
    index <- 1 # used to access uncleaned values faster
    # apply to every value in country
    for (i in country_cleaned) {
        result <- match_sheet$gta_country[stringr::str_detect(i, match_sheet$regex)]
        # no match if country is ambiguous
        if (length(result) > 1 | length(result) == 0) {
            matched <- append(matched, NA_character_)
            matching_table <- matching_table |> tibble::add_row(gta_country = NA_character_, input_country = country[index])
        } else {
            matched <- append(matched, i)
            if (tolower(country[index]) == tolower(result)) {
                matching_table <- matching_table |> tibble::add_row(gta_country = result, input_country = country[index], name_changed = FALSE)
            } else {
                matching_table <- matching_table |> tibble::add_row(gta_country = result, input_country = country[index], name_changed = TRUE)
            }
        }
        index <- index + 1
    }

    if (conversionTable){
        out <- list(results = matched, table = dplyr::distinct(matching_table) |> tidyr::drop_na(input_country) |> dplyr::arrange(dplyr::desc(name_changed)))
    } else {
        out  <- matched
    }
    return(out)
}
