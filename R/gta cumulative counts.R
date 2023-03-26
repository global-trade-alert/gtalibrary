# Roxygen documentation

#' GTA in force interventions counter
#'
#' Computes number of interventions in force.
#'
#' @param data requires a data frame obtained by running the \code{gta_data_slicer()}
#' @param counts.by Specify whether to count by month, quarter or year.'.
#' @param coverage.period Specify the range of years of interest.'.
#' @details all gta_data_slicer parameters are permissible.'.
### PUT INHERITS FROM DATA SLICER HERE!
#' @return Outputs in force interventions by different given periods.

#' @export
gta_cumulative_counts <- function(data = NULL, counts.by = "quarter", coverage.period = c(2008, lubridate::year(Sys.Date()))) {
    # check non- data slicer params
    gta_parameter_check(tolower(counts.by), c("quarter", "month", "year"), arg_name = "count.by")
    # gta_logical_check(coverage.period, \(x) length(x) == 2 & between(x[1], 2008, lubridate::year(Sys.Date()), XXX))

    data <- data |>
        dplyr::select(intervention.id, date.implemented, date.removed) |>
        tidyr::replace_na(list(date.removed = Sys.Date())) |>
        tidyr::drop_na() |> # done after filter statement ?
        dplyr::distinct() |>
        dplyr::mutate(date.removed = dplyr::case_when(
            date.removed > Sys.Date() ~ Sys.Date(),
            TRUE ~ date.removed
        )) |>
        dplyr::filter(date.implemented <= date.removed)

    # calculate results
    results <- count_interventions_cpp(
        start = sliced_data$date.implemented,
        end = sliced_data$date.removed,
        count_by = counts.by
    )
    # should be by intervention id, no ? --> Possible error in this code!
    # this only gives the unique implemented removed combos, but if two interventions
    # have the same combo, the result will be incorrect...
    # base <- unique(subset(master.sliced, select = c("date.implemented", "date.removed")))

    return(results)
}

results |>
    ggplot(aes(quarter, n_interventions)) +
    geom_line()
Rcpp::sourceCpp("src/intervention_count.cpp")
Rcpp::sourceCpp("date_test.cpp")

microbenchmark::microbenchmark(
    seq_months(Sys.Date(), Sys.Date() + 1000)
)

seq_months(Sys.Date(), Sys.Date() + 1000)
seq_quarters(Sys.Date(), Sys.Date() + 1000)
microbenchmark::microbenchmark(
    seq_years(Sys.Date(), Sys.Date() + 1000),
    seq_months(Sys.Date(), Sys.Date() + 1000),
    seq_quarters(Sys.Date(), Sys.Date() + 1000)
)

dates_in <- rep(Sys.Date())
dates_out <- rep(c(Sys.Date()))

## venchmark:: for the entire data set (without slicing) 300 ms
## with slicing can be reduced to < 200 ms

microbenchmark::microbenchmark(
    times = 2,
    a <- data |> gta_cumulative_counts()
)

duration()
gtalibrary::gta_setwd("H")
data <- readRDS("data/master.Rds")

head(a)
gta_cumulative_counts
library(gtalibrary)
