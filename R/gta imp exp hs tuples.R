# Roxygen documentation

#' GTA Importer-Exporter-Product tuples
#'
#' Generates the importer-exporter-product tuples for GTA interventions.
#' @usage gta_imp_exp_hs_tuples(
#'      master_data = NULL, 
#'      replica_data = NULL, 
#'      master_path = "data/master.rds", 
#'      replica_path = replica.path.tuple = "data/database replica/gta_tuple.Rdata"
#' )
#' @param master.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param replica.path.tuple Location of the database replica. Default is 'data/database replica/database replica - parts - base.Rdata'.
#' @return Output is two data frames. First data frame includes the importer-exporter-product relationships for the given interventions. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


## Question: WHat is the difference between data base copy, gta_tuple and master data frame ??? 

# Function infos and parameters  --------------------------------------------
#' @export
gta_imp_exp_hs_tuples <- function(
    master_data = NULL,
    replica_data = NULL
    master.path = "data/master.rds",
    replica.path.tuple = "data/database replica/gta_tuple.Rdata"
) {
    ## data file
    if (is.null(data)) {
        data <- data.table::as.data.table(readRDS(master.path))
    }

    gta_tuple <- tibble::as_tibble(gta_tuple)

    # why build a new tuple from the existing tuple ? where is difference ??
    ## results are the same with t.un being affected.jurisdiction in the tuple data ???
    names(gta_tuple) <- c("intervention.id", "un.ij", "un.dm", "un.aj", "affected.product")
    gta_tuple <- dtplyr::lazy_dt(gta_tuple) |> 
        dplyr::filter(intervention.id %in% master$intervention.id)

    
    inward_and_outward <- dtplyr::lazy_dt(master) |>
        dplyr::select(c("intervention.id", "i.un", "a.un", "affected.product", "affected.flow")) |>
        dplyr::mutate(
            affected.product = stringr::str_split(affected.product,
                pattern = ", ",
                affected_un = dplyr::case_when(
                    affected.flow == "inward" ~ importer_un,
                    affected.flow == "outward" ~ importer_un,
                    affected.flow == "outward subsidy" ~ importer_un,
                    TRUE ~ NA_character_
                ),
                importer_un = dplyr::case_when(
                    affected.flow == "inward" ~ implementing_un,
                    affected.flow == "outward" ~ affected_un,
                    affected.flow == "outward subsidy" ~ implementing_un,
                    TRUE ~ NA_character_
                ),
                exporter_un = dplyr::case_when(
                    affected.flow == "inward" ~ affected_un,
                    affected.flow == "outward" ~ implementing_un,
                    affected.flow == "outward subsidy" ~ affected_un,
                    TRUE ~ NA_character_
                )
            )
        ) |>
        data.table::as.data.table() |>
        tidyr::unnest(cols = affected.product) |>
        tidyr::drop_na(c(i.un, a.un, t.un, affected.product))
}
