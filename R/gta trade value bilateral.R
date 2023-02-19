# Roxygen documentation

#' GTA bilateral trade value
#'
#' Computes a data frame with the bilateral trade values in the GTA base period for the given importer-exporter-product combinations.
#'
#' @param importing.country Specify the importing countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.importer Specify whether to focus on ('TRUE') or exclude ('FALSE') trade with the stated importing country.
#' @param exporting.country Specify the exporting countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.exporter Specify whether to focus on ('TRUE') or exclude ('FALSE') trade with the stated exporting country.
#' @param cpc.sectors Provide a vector of CPC codes that you are interested in (version 2.1, 3-digit level).
#' @param keep.cpc Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
#' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
#' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
#' @param trade.data Choose the trade data underlying these calulations. Choices are individual years between 2007 and 2020, the GTA base period data ('base', averages for 2005-2007) as well as moving trade data as a function of coverage year ('prior year' and 'current year'). Default is 'base'.
#' @param trade.data.path Set path of trade data file (default is 'data/support tables/Goods support table for gtalibrary.Rdata').
#' @param df.name Set the name of the generated result data frame. Default is trade.base.
#' @param pc.name Set the name of the generated parameter choice data frame. Default is parameter.choice.trade.base.
#' @import data.table
#' @import dplyr
#' @import dtplyr
#' @return Output is two data frames. First data frame includes the trade values for the given importer-exporter-product combinations. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

# Function infos and parameters  --------------------------------------------
#' @export
gta_trade_value_bilateral <- function(importing.country = NULL, keep.importer = NULL,
                                      exporting.country = NULL, keep.exporter = NULL,
                                      cpc.sectors = NULL, keep.cpc = TRUE, hs.codes = NULL,
                                      keep.hs = TRUE, trade.data = "base",
                                      trade.data.path = "data/support tables/Goods support table for gtalibrary.Rdata",
                                      trade.data.file = NULL # ensure that this data.frame can also be fed from memory for better performance in loops!
) {
    gtalibrary::gta_parameter_check(trade.data, c("base", "prior year", "current year", "before announcement", "during announcement", as.character(2005:2020)))
    filter_statement <- vector("character")

    if (trade.data == "base") {
        trade.base <- gtalibrary::trade.base |>
            dplyr::mutate(trade.value = trade.value / 3)
    } else if (is.null(trade.data.file)) {
        trade.base <- data.table::as.data.table(readRDS(trade.data.path))
    } ## convert to data.table (analogous to gta.data.slicer())

    if (trade.data %in% as.character(2005:2020)) {
        base_year <- as.numeric(trade.data)
        trade.base <- trade.base |>
            dplyr::filter(year == base_year)
    }

    if (tolower(trade.data) == "prior year") {
        trade.base <- trade.base |>
            dplyr::mutate(year = year + 1)
    }

    ## importer
    if (!is.null(importing.country)) {
        gtalibrary::gta_logical_check(keep.importer, is.logical)
        importers_filter <- gtalibrary::gta_un_code_vector(importing.country)

        if (keep.importer) {
            filter_statement <- append(filter_statement, "i.un %in% importers_filter")
        } else {
            filter_statement <- append(filter_statement, "!i.un %in% importers_filter")
        }
    }

    ## exporter
    if (!is.null(exporting.country)) {
        gtalibrary::gta_logical_check(keep.exporter, is.logical)
        exporters_filter <- gtalibrary::gta_un_code_vector(exporting.country)
        if (keep.exporter) {
            filter_statement <- append(filter_statement, "a.un %in% exporters_filter")
        } else {
            filter_statement <- append(filter_statement, "a.un %in% exporters_filter")
        }

        ## hs codes
        if (!is.null(hs.codes)) {
            gtalibrary::gta_logical_check(keep.hs, is.logical)
            hs_codes_filter <- gtalibrary::gta_hs_code_check(codes = hs.codes, message = FALSE)

            if (keep.hs) {
                filter_statement <- append(filter_statement, "hs6 %in% hs_codes_filter") ## check if we need to pad the codes or make them numeric!!!
            } else {
                filter_statement <- append(filter_statement, "!hs6 %in% hs_codes_filter")
            }
        }

        ## cpc codes
        if (!is.null(cpc.sectors)) {
            gtalibrary::gta_logical_check(keep.cpc, is.loical)
            cpc_codes_filter <- gtalibrary::gta_cpc_to_hs(cpc.sectors)

            if (is.null(heep.cpc)) {
                filter_statement <- append(filter_statement, "hs6 %in% cpc_codes_filter")
            } else {
                filter_statement <- append(filter_statement, "!hs6 %in% cpc_codes_filter")
            }
        }

        # filter dataset
        trade.base |>
            dplyr::filter(eval(parse(text = filter_statement))) |>
            data.table::as.data.table()
    }
}
## could specify the year value here --> WOuld make it easier int he trade coverage function!
