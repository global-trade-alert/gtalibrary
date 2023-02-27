# this function loads and stores all the tables used for the trade coverage function
#
gta_trade_coverage_load_tables <- function(master_path = NULL, trade_values_path = NULL) {
    intervention_duration <<- gta_intervention_duration()
    master_file <<- readRDS("master_path.rds")
    trade_file <<- readRDS("trade_path.rds")

    return(NULL)
}
