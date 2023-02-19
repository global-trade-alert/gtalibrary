# Roxygen documentation

#' Map geo data to a dataframe with un codes.
#'
#' This function facilitates map creation with ggplot.
#'
#' @param data Specify the dataframe with the relevant columns.
#' @param countries Specify the name of the column containing the countries.
#' @param values Specify the name of the column containing the value for each country.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

#' @export
gta_plot_map_df <- function(data = NULL, countries = NULL, values = NULL) {
    if (is.null(data) | is.null(countries) | is.null(values)) {
        stop("Please define column names (countries and values) and a dataframe!")
    }

    world <- gtalibrary::world.geo

    data[, c("UN", "value")] <- data[, c(countries, values)]
    data$UN <- gta_un_code_vector(data$UN)

    # merge data with map data
    world <- merge(world, data[, c("UN", "value")], by = "UN", all.x = T)

    ###### IMPORTANT, sort for X (id) again
    world <- world[with(world, order(X)), ]

    return(world)
}
