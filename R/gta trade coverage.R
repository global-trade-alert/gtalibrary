## Roxygen documentation
#
##' GTA trade coverage function
##'
##' Computes the trade coverages of GTA measures.
##'
##' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
##' @param replica.path Location of the database replica. Default is 'data/database replica/database replica - parts - base.Rdata'.
##' @param coverage.period The calendar years for which to calculate the trade coverage shares. Default is c(2009,CURRENT.YEAR). Calculation includes interventions based on enforcement status, not implementation date i.e. if you start in 2010, this function will also work with interventions implemneted in 2009 but still in force in 2010. Use implementation.period parameter to change this.
##' @param current.year.todate Should the coverage statistics for the current year be calculated as 'coverage for year to date' (TRUE) or 'coverage for entire current year' (FALSE). Default is TRUE.
##' @param gta.evaluation Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
##' @param affected.flows Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is c('inward','outward subsidy'). Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
##' @param importers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for importers in the sample. Default: All importers.
##' @param keep.importers Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated importers.
##' @param incl.importers.strictness Specify whether to include interventions that affect only one of the selected importers ('ONE'), at least one of the selected importers ('ONEPLUS') or all of the selected importers ('ALL'). Default is 'ONEPLUS'.
##' @param group.importers Specify whether to aggregate the statistics for all remaining importers into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
##' @param separate.importer.groups Specifiy whether to separately calculate groups in chosen importers ('TRUE') or not ('FALSE'). Default: FALSE.
##' @param nr.importers Specify the range for the number of importers affected by an intervention. Default is any number i.e. c(1,999).
##' @param nr.importers.incl Specify whether in the number of importers affected by an intervention is calculated based only on the selected importers are included ('SELECTED'), only on the unselected importers ('UNSELECTED') or based on both ('ALL'). Default is 'ALL'.
##' @param exporters Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for exporters in the sample. Default: All exporters.
##' @param keep.exporters Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated exporters.
##' @param incl.exporters.strictness Specify whether to include interventions that affect only one of the selected exporters ('ONE'), at least one of the selected exporters ('ONEPLUS') or all of the selected exporters ('ALL'). Default is 'ONEPLUS'.
##' @param group.exporters Specify whether to aggregate the statistics for all remaining exporters into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
##' @param separate.exporter.groups Specifiy whether to separately calculate groups in chosen exporters ('TRUE') or not ('FALSE'). Default: FALSE.
##' @param nr.exporters Specify the range for the number of exporters affected by an intervention. Default is any number i.e. c(1,999).
##' @param nr.exporters.incl Specify whether in the number of exporters affected by an intervention is calculated based only on the selected exporters are included ('SELECTED'), only on the unselected exporters ('UNSELECTED') or based on both ('ALL'). Default is 'ALL'.
##' @param implementers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for implementers in the sample. Default: World (as in implemented by one).
##' @param implementer.role Bilateral trade flows can be affected by multiple actors. Specify which actor's interventions you want to include. There are three roles: importer, exporter and 3rd country. Combinations are permissible. Default: c('importer','3rd country').
##' @param keep.implementer Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementing country.
##' @param group.implementers Specify whether to aggregate the statistics for all remaining implementers into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
##' @param separate.implementer.groups Specifiy whether to separately calculate groups in chosen implementers ('TRUE') or not ('FALSE'). Default: FALSE.
##' @param implementer.trade Specify whether you want to receive import or export shares for the implementer. Default: As implied by selection in other fields.
##' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
##' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
##' @param revocation.period Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
##' @param submission.period Specify a period in which the interventions for your analysis have been submitted. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
##' @param in.force.on.date Specify the cutoff date to control for in force interventions. Default is the current date (Sys.Date).
##' @param keep.in.force.on.date Specify whether you want to focus on interventions in force on the specified date ('Yes') or no longer in force on the specified date ('No'). Default is 'any' i.e. regardless of enforcement status on the specified date.
##' @param intervention.types Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
##' @param keep.type Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
##' @param keep.devaluations Specify whether to include "Competitive devaluation" into your calculations. Default is FALSE.
##' @param group.type Specify whether to aggregate the statistics for all remaining intervention types into one group (TRUE) or whether create the statistics for every single type (FALSE). Default is TRUE.
##' @param mast.chapters Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
##' @param keep.mast Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
##' @param group.mast Specify whether to aggregate the statistics for all remaining MAST chapters into one group (TRUE) or whether create the statistics for every single chapter (FALSE). Default is TRUE.
##' @param implementation.level Specify the government level responsible for the intervention.  Default is 'any'. Permissible values are 'supranational', 'national', 'subnational', 'IFI', 'NFI' or combinations thereof. IFI and NFI refer to government-owned financial institutions that are owned by one ('NFI') or more ('IFI') governments.
##' @param keep.level Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementation levels.
##' @param eligible.firms Concentrate the analysis on interventions that are targeted at certain subsets. Default is 'any'. Permissible values are 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise' or combinations thereof.
##' @param keep.firms Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated firm subsets.
##' @param keep.firm.trade.finance Specify whether you want to keep firm-specific trade finance interventions inside the data set (Default: 'FALSE').
##' @param keep.firm.financial.assistance.ifm Specify whether you want to keep firm-specific 'financial assistance in a foreign market' interventions inside the data set (Default: 'FALSE').
##' @param cpc.sectors Provide a vector of CPC codes that you are interested in (version 2.1, 3-digit level).
##' @param keep.cpc Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
##' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
##' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
##' @param hit.brackets Specify whether to calculate the trade shares by the number of interventions affecting a importer-exporter-product combination e.g. c(1,2,3,4,5,999999) for the brackets '1-2,3-4,5 or more'. Default is c(1,99999).
##' @param intervention.ids Provide a vector of intervention IDs.
##' @param keep.interventions Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated intervention IDs.
##' @param lag.adjustment Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
##' @param reporting.period Specify the period in which an intervention was documented by the GTA team. Default c('2008-11-01',today).
##' @param intra.year.duration Adjust the estimates for the number of days the relevant intervention has been in force in the given year (TRUE/FALSE). Default is TRUE.
##' @param trade.statistic Choose to calculate trade shares ('share') or absolute USD values ('value'). Default is 'share'.
##' @param trade.data Choose the trade data underlying these calulations. Choices are individual years between 2005 and 2020, the GTA base period data ('base', averages for 2005-2007) as well as moving trade data as a function of coverage year ('prior year' and 'current year'). Default is 'base'.
##' @param trade.data.path Set path of trade data file (default is 'data/support tables/Goods support table for gtalibrary.Rdata'),
##' @import data.table
##' @import dplyr
#
##' @return Outputs a table with coverage shares ranging from 2009 to 2020 for each importer, exporter, implementer, instrument combination.
##' @references www.globaltradealert.org
##' @author Global Trade Alert
#
### Function infos and parameters  --------------------------------------------
##
 gta_trade_coverage <- function(
    data.path="data/master_plus.Rdata",
    replica.path="data/database replica/database replica - parts - base.Rdata",
    replica.path.atl="data/database replica/gta_affected_tariff_line.Rdata",
    replica.path.tuple="data/database replica/gta_tuple.Rdata",
    coverage.period=NULL, current.year.todate=TRUE, gta.evaluation= NULL,
    affected.flows = c("inward", "outward subsidy"), importers = NULL,
    keep.importers = NULL, incl.importers.strictness="ONEPLUS",group.importers = TRUE,
    separate.importer.groups = FALSE, nr.importers=c(0,999), nr.importers.incl="ALL",
    jointly.affected.importers=FALSE, exporters = NULL, keep.exporters = NULL,
    incl.exporters.strictness="ONEPLUS", group.exporters = TRUE, separate.exporter.groups = FALSE,
    nr.exporters=c(0,999), nr.exporters.incl="ALL", implementers = NULL, implementer.role = NULL,
    keep.implementer= TRUE, group.implementers=TRUE,separate.implementer.groups=FALSE,
    implementer.trade=NULL, announcement.period = NULL, implementation.period = NULL,
    revocation.period = NULL, keep.revocation.na = NULL, submission.period = NULL,
    in.force.on.date = Sys.Date(), keep.in.force.on.date = 'any', intervention.types = NULL,
    keep.type = NULL, keep.devaluations=FALSE, group.type=TRUE, mast.chapters = NULL,
    keep.mast = NULL, group.mast=TRUE, implementation.level = NULL, keep.level = NULL,
    eligible.firms = NULL, keep.firms = NULL, keep.firm.trade.finance=FALSE,
    keep.firm.financial.assistance.ifm = FALSE, cpc.sectors = NULL, keep.cpc = NULL,
    hs.codes = NULL, keep.hs = NULL, hit.brackets=c(1,99999), intervention.ids = NULL,
    keep.interventions = NULL, lag.adjustment=NULL, reporting.period=NULL,
    add.unpublished= FALSE, intra.year.duration=TRUE, get.hs.level.data=FALSE,
    trade.statistic="share", trade.data="base",
    trade.data.path="data/support tables/Goods support table for gtalibrary.Rdata",
    rdata = FALSE, xlsx = FALSE, output.path = NULL, xlsx.interventions = FALSE,
    output.path.interventions = NULL) {
    

  # Initialising Function ---------------------------------------------------
    filter_statement <- vector("character")
    data <- data |>
        gta_data_slicer(
            data.path = data.path,
            gta.evaluation = gta.evaluation, affected.flows = affected.flows, announcement.period = announcement.period,
            implementation.period = implementation.period, keep.implementation.na = FALSE, revocation.period = revocation.period,
            keep.revocation.na = keep.revocation.na, submission.period = submission.period, in.force.on.date = in.force.on.date,
            keep.in.force.on.date = keep.in.force.on.date, intervention.types = intervention.types,
            keep.type = keep.type, mast.chapters = mast.chapters,
            keep.mast = keep.mast, implementation.level = implementation.level, keep.level = keep.level,
            eligible.firms = eligible.firms, keep.firms = keep.firms, cpc.sectors = cpc.sectors,
            keep.cpc = keep.cpc, hs.codes = hs.codes, keep.hs = keep.hs, intervention.ids = intervention.ids,
            keep.interventions = keep.interventions, lag.adjustment = lag.adjustment, reporting.period = reporting.period,
            add.unpublished = add.unpublished
        )

    filter_statement <- append(filter_statement, "!intervention.id %in% out")

    if(!keep.firm.trade.finance) {
        filter_statement <- appened(filter_statement, "!(intervention.type == 'Trade finance' & eligible.firms == 'firm-specific')")
    }

    if(!keep.firm.financial.assistance.ifm) {
        filter_statement <- append(filter_statement, "!(intervention.type == 'Financial assistance in foreign markets' & eligible.firms == 'firm-specific'")
    }

    if(!keep.devaluations) {
        filter_statement <- append(filter_statement, "!intervention.type == 'Competitive devaluation'")
    }

    # filter data set from the data slicer again ? is this necessary ?
    data <- dtplyr::lazy_dt(data) |>
        dplyr::filter(eval(parse(text = filter_statement)))

    ### restricted the data set to the specified exporters.
    ## Can be done here since they are always either a.un or i.un.
    ## Cannot be done for the importers!

    #### imposing the exporting countries incl. the relevant conditions.

    # adjust functon or calculate more efficiently from current data frame
    data |> gta_intervention_duration(data = NULL
        data.path = "data/master.rds",
        years = c(year.start, year.end),
        current.year.todate = current.year.todate
    )



    if (!is.null(importers)) {
        # do we need to specify importers if it is not null??
        gta_logical_check(keep.importers, is.logical, error_msg = "{.var keep.importers} must be TRUE/FALSE")
        ## gta_un_code_vector returns all un codes if argument is false
        importing.country <- gta_un_code_vector(importers)

        if (!keep.importers) {
            ## specify filter statemenr
        } else {
            ## specify second filter statment if needed
        }
    }

    ### IMPLEMENTERS
    if(!is.null(implementers)){
        gta_logical_check(keep.implementer, is.logical, error_msg = "{.var keep.implementer must be TRUE/FALSE}")
        implementing.country <- gta_un_code_vector(implementing.country)
        
        if (!keep.implementer){
            # filter statement
        } else {
            ## other filter statement if needed 
        }

    if (!is.null(implementer.role)){

    }

    ## Importing relevant trade data
    if(is.null(implementer.trade)){

        trade_data |>
        gta_trade_value_bilateral(data = NULL, importing.country = importing.country,
            keep.importer = TRUE, exporting.country = exporting.country,
            keep.exporter = TRUE, cpc.sectors = cpc.sectors,
            keep.cpc = keep.cpc, hs.codes = hs.codes,
            keep.hs = keep.hs, trade.data=trade.data,
            trade.data.path = trade.data.path) # must all be specified in the overhread function! use piping again!

      # Add country groups and sum up their trade values


      trade_data |>
        gta_trade_value_bilateral(data = NULL
        importing.country = implementer.imports,
        keep.importer = TRUE, exporting.country = implementer.exports,
        keep.exporter = TRUE, cpc.sectors = cpc.sectors, keep.cpc = keep.cpc,
        hs.codes = hs.codes, keep.hs = keep.hs, trade.data = trade.data,
        trade.data.path = trade.data.path
      )

    }


load("data/database replica/gta_tuple.Rdata")
load("data/master_plus.Rdata")
load("data/database replica/database replica - parts - base (1).Rdata")

library(tidyverse)
gtalibrary::gta_setwd("H")

tuple <- as_tibble(gta_tuple)
rm(gta_tuple)
master <- as_tibble(master)
master

id_tuple <- unique(tuple$intervention_id)
id_master <- unique(master$intervention.id)

length(id_tuple)
length(id_master)

pryr::object_size(master)