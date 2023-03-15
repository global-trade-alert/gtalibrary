## Roxygen documentation
#
#' GTA intra-year duration calculator
#'
#' Computes the share of each calendar year that each intervention was in force.
#' @param data Allows to pass the data frame with interventions directly to the function. This is advisable if the function is run multiple times and avoids
#' a repeated loding of the data frame within the function.
#' @param data_path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata')
#' @param years The calendar years for which to calculate the shares. Calculation includes interventions based on enforcement status, not implementation date i.e. if you start in 2010, this function will also work with interventions implemneted in 2009 but still in force in 2010. Specify as c(start.year, end.year). Default is c(2008,CURRENT.YEAR).
#' @param current_year_todate Should the duration for the current year, if included, be calculated as 'share of year to date' (TRUE) or 'share of entire current year' (FALSE). Default is TRUE.
#' @export
gta_intervention_duration <- function(data = NULL,
                                      data.path = "data/master.rds", years = NULL,
                                      current_year_todate = TRUE) {
    if (is.null(data)) data <- data.table::as.data.table(readRDS(data.path))

    ## check years
    if (!is.null(years)) {
        current_year <- lubridate::year(Sys.Date())
        gta_logical_check(
            years,
            \(x) (length(x) == 2 & trunc(x) == x & between(years[1], 2008, current_year) & between(years[2], 2008, current_year)),
            "Years must be a vector of two numeric values between 2008 and {current_year}"
        )
    }

    # filter the data frame for unique interventions
    unique_interventions <- dtplyr::lazy_dt(data) |>
        dplyr::select(intervention.id, date.implemented, date.removed) |>
        unique() |>
        tibble::as_tibble() |>
        tidyr::drop_na(date.implemented) |>
        tidyr::replace_na(list(date.removed = Sys.Date())) |>
        dplyr::filter((date.implemented < date.removed))

    # calculate year in force and share with function from c++
    results <- datefunction(
        start = unique_interventions$date.implemented,
        end = unique_interventions$date.removed,
        current_date = Sys.Date(),
        current_year_todate = current_year_todate
    )

    # join values onto unique interventions and unnest
    unique_interventions <- unique_interventions |>
        dplyr::mutate(
            year_in_force = results$year_in_force,
            share_of_year = results$share_of_year
        ) |>
        tidyr::unnest(cols = c(year_in_force, share_of_year))

    # filter out desired yers if years is specified
    if (!is.null(years)) {
        unique_interventions |>
            dplyr::filter(dplyr::between(year_in_force, years[1], years[2]))
    } else {
        return(unique_interventions)
    }
}

### NOT FINISHED YET --> MORE TESTING NEEDED

# current speed: 500-600ms
# microbenchmark::microbenchmark(
#    times = 1,
#    gtalibrary::gta_intervention_duration()
# )
# )

## current speed gtalibrary version (without importing data set): 32 seconds
## --> Speed improvement: x 60
