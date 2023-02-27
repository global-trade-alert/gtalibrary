# Roxygen documentation

#' GTA Importer-Exporter-Product tuples
#'
#' Generates the importer-exporter-product tuples for GTA interventions.
#'
#' @param master.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param replica.path.tuple Location of the database replica. Default is 'data/database replica/database replica - parts - base.Rdata'.
#' @return Output is two data frames. First data frame includes the importer-exporter-product relationships for the given interventions. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

# Function infos and parameters  --------------------------------------------
#' @export
gta_imp_exp_hs_tuples <- function(data = NULL,
                                  master.path = "data/master.rds",
                                  replica.path.tuple = "data/database replica/gta_tuple.Rdata") {
    ## data file
    if (is.null(data)) {
        data <- data.table::as.data.table(readRDS(master.path))
    }

    gta_tuple <- tibble::as_tibble(gta_tuple)

    load(replica.path.tuple)

    # why build a new tuple from the existing tuple ? where is difference ?? 
    ## results are the same with t.un being affected.jurisdiction in the tuple data ??? 
    names(gta_tuple) <- c("intervention.id", "un.ij", "un.dm", "un.aj", "affected.product")
    gta_tuple <- subset(gta_tuple, intervention.id %in% master$intervention.id)

    ## transform IJ/DM/AJ so that we have i.un and a.un as importers and exporters
    master.tuple <- data.frame(intervention.id = numeric(), i.un = numeric(), a.un = numeric(), t.un = numeric(), affected.product = numeric())

    if ("inward" %in% master$affected.flow) {
        inward <- subset(master, affected.flow == "inward")[, c("intervention.id", "i.un", "a.un", "affected.product")]
        inward$t.un <- inward$i.un
        inward <- cSplit(inward, which(names(inward) == "affected.product"), direction = "long", sep = ",")
        master.tuple <- rbind(master.tuple, inward)
        rm(inward)
    }

    if ("outward" %in% master$affected.flow) {
        outward <- subset(master, affected.flow == "outward")[, c("intervention.id", "i.un", "a.un", "affected.product")]
        setnames(outward, "i.un", "t.un")
        outward$i.un <- outward$a.un
        outward$a.un <- outward$t.un
        outward <- cSplit(outward, which(names(outward) == "affected.product"), direction = "long", sep = ",")
        master.tuple <- rbind(master.tuple, outward)
        rm(outward)
    }

    if ("outward subsidy" %in% master$affected.flow) {
        os <- subset(master, affected.flow == "outward subsidy")[, c("intervention.id", "i.un", "a.un", "affected.product")]
        setnames(os, "i.un", "t.un")
        os <- cSplit(os, which(names(os) == "affected.product"), direction = "long", sep = ",")
        gt.os <- subset(gta_tuple, intervention.id %in% os$intervention.id)
        names(gt.os) <- c("intervention.id", "t.un", "i.un", "a.un", "affected.product")

        os <- merge(os, gt.os, by = c("intervention.id", "t.un", "a.un", "affected.product"), all.x = T)
        os <- subset(os, is.na(i.un) == F)
        master.tuple <- rbind(master.tuple, os)
        rm(gt.os, os)
    }

    master.tuple <- subset(master.tuple, is.na(i.un) == F & is.na(a.un) == F & is.na(t.un) == F & is.na(affected.product) == F)
}


gtalibrary::gta_setwd("H")
gtalibrary::gta_imp_exp_hs_tuples()
gta_tuple |>
    dplyr::filter(intervention.id == 57647 & un.aj == 116)
master.tuple |>
    dplyr::filter(intervention.id == 57647 & a.un == 116)
