# Roxygen documentation

#' GTA trade coverage function
#'
#' Computes the trade coverages of GTA measures.
#'
#' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param replica.path Location of the database replica. Default is 'data/database replica/database replica - parts - base.Rdata'.
#' @param coverage.period The calendar years for which to calculate the trade coverage shares. Default is c(2009,CURRENT.YEAR). Calculation includes interventions based on enforcement status, not implementation date i.e. if you start in 2010, this function will also work with interventions implemneted in 2009 but still in force in 2010. Use implementation.period parameter to change this.
#' @param current.year.todate Should the coverage statistics for the current year be calculated as 'coverage for year to date' (TRUE) or 'coverage for entire current year' (FALSE). Default is TRUE.
#' @param gta.evaluation Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
#' @param affected.flows Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is c('inward','outward subsidy'). Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
#' @param importers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for importers in the sample. Default: All importers.
#' @param keep.importers Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated importers.
#' @param incl.importers.strictness Specify whether to include interventions that affect only one of the selected importers ('ONE'), at least one of the selected importers ('ONEPLUS') or all of the selected importers ('ALL'). Default is 'ONEPLUS'.
#' @param group.importers Specify whether to aggregate the statistics for all remaining importers into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
#' @param separate.importer.groups Specifiy whether to separately calculate groups in chosen importers ('TRUE') or not ('FALSE'). Default: FALSE.
#' @param nr.importers Specify the range for the number of importers affected by an intervention. Default is any number i.e. c(1,999).
#' @param nr.importers.incl Specify whether in the number of importers affected by an intervention is calculated based only on the selected importers are included ('SELECTED'), only on the unselected importers ('UNSELECTED') or based on both ('ALL'). Default is 'ALL'.
#' @param exporters Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for exporters in the sample. Default: All exporters.
#' @param keep.exporters Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated exporters.
#' @param incl.exporters.strictness Specify whether to include interventions that affect only one of the selected exporters ('ONE'), at least one of the selected exporters ('ONEPLUS') or all of the selected exporters ('ALL'). Default is 'ONEPLUS'.
#' @param group.exporters Specify whether to aggregate the statistics for all remaining exporters into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
#' @param separate.exporter.groups Specifiy whether to separately calculate groups in chosen exporters ('TRUE') or not ('FALSE'). Default: FALSE.
#' @param nr.exporters Specify the range for the number of exporters affected by an intervention. Default is any number i.e. c(1,999).
#' @param nr.exporters.incl Specify whether in the number of exporters affected by an intervention is calculated based only on the selected exporters are included ('SELECTED'), only on the unselected exporters ('UNSELECTED') or based on both ('ALL'). Default is 'ALL'.
#' @param implementers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for implementers in the sample. Default: World (as in implemented by one).
#' @param implementer.role Bilateral trade flows can be affected by multiple actors. Specify which actor's interventions you want to include. There are three roles: importer, exporter and 3rd country. Combinations are permissible. Default: c('importer','3rd country').
#' @param keep.implementer Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementing country.
#' @param group.implementers Specify whether to aggregate the statistics for all remaining implementers into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
#' @param separate.implementer.groups Specifiy whether to separately calculate groups in chosen implementers ('TRUE') or not ('FALSE'). Default: FALSE.
#' @param implementer.trade Specify whether you want to receive import or export shares for the implementer. Default: As implied by selection in other fields.
#' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
#' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
#' @param revocation.period Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param submission.period Specify a period in which the interventions for your analysis have been submitted. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param in.force.on.date Specify the cutoff date to control for in force interventions. Default is the current date (Sys.Date).
#' @param keep.in.force.on.date Specify whether you want to focus on interventions in force on the specified date ('Yes') or no longer in force on the specified date ('No'). Default is 'any' i.e. regardless of enforcement status on the specified date.
#' @param intervention.types Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
#' @param keep.type Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
#' @param keep.devaluations Specify whether to include "Competitive devaluation" into your calculations. Default is FALSE.
#' @param group.type Specify whether to aggregate the statistics for all remaining intervention types into one group (TRUE) or whether create the statistics for every single type (FALSE). Default is TRUE.
#' @param mast.chapters Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
#' @param keep.mast Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
#' @param group.mast Specify whether to aggregate the statistics for all remaining MAST chapters into one group (TRUE) or whether create the statistics for every single chapter (FALSE). Default is TRUE.
#' @param implementation.level Specify the government level responsible for the intervention.  Default is 'any'. Permissible values are 'supranational', 'national', 'subnational', 'IFI', 'NFI' or combinations thereof. IFI and NFI refer to government-owned financial institutions that are owned by one ('NFI') or more ('IFI') governments.
#' @param keep.level Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementation levels.
#' @param eligible.firms Concentrate the analysis on interventions that are targeted at certain subsets. Default is 'any'. Permissible values are 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise' or combinations thereof.
#' @param keep.firms Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated firm subsets.
#' @param keep.firm.trade.finance Specify whether you want to keep firm-specific trade finance interventions inside the data set (Default: 'FALSE').
#' @param keep.firm.financial.assistance.ifm Specify whether you want to keep firm-specific 'financial assistance in a foreign market' interventions inside the data set (Default: 'FALSE').
#' @param cpc.sectors Provide a vector of CPC codes that you are interested in (version 2.1, 3-digit level).
#' @param keep.cpc Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
#' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
#' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
#' @param hit.brackets Specify whether to calculate the trade shares by the number of interventions affecting a importer-exporter-product combination e.g. c(1,2,3,4,5,999999) for the brackets '1-2,3-4,5 or more'. Default is c(1,99999).
#' @param intervention.ids Provide a vector of intervention IDs.
#' @param keep.interventions Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated intervention IDs.
#' @param lag.adjustment Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
#' @param reporting.period Specify the period in which an intervention was documented by the GTA team. Default c('2008-11-01',today).
#' @param intra.year.duration Adjust the estimates for the number of days the relevant intervention has been in force in the given year (TRUE/FALSE). Default is TRUE.
#' @param trade.statistic Choose to calculate trade shares ('share') or absolute USD values ('value'). Default is 'share'.
#' @param trade.data Choose the trade data underlying these calulations. Choices are individual years between 2005 and 2020, the GTA base period data ('base', averages for 2005-2007) as well as moving trade data as a function of coverage year ('prior year' and 'current year'). Default is 'base'.
#' @param trade.data.path Set path of trade data file (default is 'data/support tables/Goods support table for gtalibrary.Rdata'),
#' @param rdata Takes value TRUE or FALSE. If TRUE, Rdata file will be stored alongside xlsx. Default: FALSE
#' @param xlsx Takes value TRUE or FALSE. If TRUE, xlsx file will be stored. Default: FALSE
#' @param output.path Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.
#' @param xlsx.interventions Takes value TRUE or FALSE. If TRUE, xlsx file with a list of used interventions will be stored. Default: FALSE
#' @param output.path.interventions Takes the value of the output path for the interventions list file (without the filename) added to the working directory as a string starting with "/". Default: None.


#' @return Outputs a table with coverage shares ranging from 2009 to 2020 for each importer, exporter, implementer, instrument combination.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


# Function infos and parameters  --------------------------------------------
#' @export
gta_trade_coverage <- function(
  data.path="data/master_plus.Rdata",
  replica.path="data/database replica/database replica - parts - base.Rdata",
  replica.path.atl="data/database replica/gta_affected_tariff_line.Rdata",
  replica.path.tuple="data/database replica/gta_tuple.Rdata",
  coverage.period=NULL,
  current.year.todate=TRUE,
  gta.evaluation= NULL,
  affected.flows = c("inward", "outward subsidy"),
  importers = NULL,
  keep.importers = NULL,
  incl.importers.strictness="ONEPLUS",
  group.importers = TRUE,
  separate.importer.groups = FALSE,
  nr.importers=c(0,999),
  nr.importers.incl="ALL",
  jointly.affected.importers=FALSE,
  exporters = NULL,
  keep.exporters = NULL,
  incl.exporters.strictness="ONEPLUS",
  group.exporters = TRUE,
  separate.exporter.groups = FALSE,
  nr.exporters=c(0,999),
  nr.exporters.incl="ALL",
  implementers = NULL,
  implementer.role = NULL,
  keep.implementer= TRUE,
  group.implementers=TRUE,
  separate.implementer.groups=FALSE,
  implementer.trade=NULL,
  announcement.period = NULL,
  implementation.period = NULL,
  revocation.period = NULL,
  keep.revocation.na = NULL,
  submission.period = NULL,
  in.force.on.date = Sys.Date(),
  keep.in.force.on.date = 'any',
  intervention.types = NULL,
  keep.type = NULL,
  keep.devaluations=FALSE,
  group.type=TRUE,
  mast.chapters = NULL,
  keep.mast = NULL,
  group.mast=TRUE,
  implementation.level = NULL,
  keep.level = NULL,
  eligible.firms = NULL,
  keep.firms = NULL,
  keep.firm.trade.finance=FALSE,
  keep.firm.financial.assistance.ifm=FALSE,
  cpc.sectors = NULL,
  keep.cpc = NULL,
  hs.codes = NULL,
  keep.hs = NULL,
  hit.brackets=c(1,99999),
  intervention.ids = NULL,
  keep.interventions = NULL,
  lag.adjustment=NULL,
  reporting.period=NULL,
  add.unpublished=F,
  intra.year.duration=TRUE,
  get.hs.level.data=FALSE,
  trade.statistic="share",
  trade.data="base",
  trade.data.path="data/support tables/Goods support table for gtalibrary.Rdata",
  rdata = FALSE,
  xlsx = FALSE,
  output.path = NULL,
  xlsx.interventions = FALSE,
  output.path.interventions = NULL) {


  # Initialising Function ---------------------------------------------------

  # load libraries
  library("openxlsx")
  library("splitstackshape")
  library("data.table")

  ######## Feed data slicer

  ## Collecting parameter values
  parameter.choices=data.frame(parameter=character(), choice=character(),stringsAsFactors = F)

  tryCatch({

    ### SECTION 1: Loading the GTA data and filtering it
    print("Slicing GTA master data set ...")
    gta_data_slicer(data.path=data.path,
                    gta.evaluation= gta.evaluation,
                    affected.flows = affected.flows,
                    announcement.period = announcement.period,
                    implementation.period = implementation.period,
                    keep.implementation.na=F,
                    revocation.period = revocation.period,
                    keep.revocation.na = keep.revocation.na,
                    submission.period = submission.period,
                    in.force.on.date = in.force.on.date,
                    keep.in.force.on.date = keep.in.force.on.date,
                    intervention.types = intervention.types,
                    keep.type = keep.type,
                    mast.chapters = mast.chapters,
                    keep.mast = keep.mast,
                    implementation.level = implementation.level,
                    keep.level = keep.level,
                    eligible.firms = eligible.firms,
                    keep.firms = keep.firms,
                    cpc.sectors = cpc.sectors,
                    keep.cpc = keep.cpc,
                    hs.codes = hs.codes,
                    keep.hs = keep.hs,
                    intervention.ids = intervention.ids,
                    keep.interventions = keep.interventions,
                    lag.adjustment=lag.adjustment,
                    reporting.period=reporting.period,
                    add.unpublished=add.unpublished)


    ## removing certain problemtic, wide-reaching cases until further investigation
    out=c(20387, 20389, 16408, 16817, 15248, 20098, 56907,
          18891,70350,16819,71578,58794,18254,13633,15366,19899,13512,14328,
          18602,14104,17285,18601,19351,19347,15100,18638,57474,14017,20375,
          57843,57619,62121,70692,72278,60042,13631,72137,18795,71645,13707,
          19425,70751,15747,58726,18897,18649,72800,72384,69601, 70466,
          71010,60343,68840,62147,71561, 82519, 81141, 80982, 78338,80442,80985,
          79456, 80739, 80552, 81549, 79111,81832,
          78439,80057, 78044, 78764, 81428, 79740, 81439, 78706, 81786, 78033,
          62120,70561,73612,73866,77800,79201,79440,80064,80547,81030,81059,81093,81785,84331, 73056,81550, 82986,
          96411, 96453, 96559, 96560, 96561, 96562, 96315, 96728, 96729, 96730, 96731, 96732, 96735,
          71011, 60342, 83648
    )

    master.sliced=subset(master.sliced, ! intervention.id %in% out)

    if(keep.firm.trade.finance==F){

      master.sliced=subset(master.sliced, ! intervention.id %in% subset(master.sliced, intervention.type %in% c("Trade finance") & eligible.firms=="firm-specific")$intervention.id)

    }

    if(keep.firm.financial.assistance.ifm==F){

      master.sliced=subset(master.sliced, ! intervention.id %in% subset(master.sliced, intervention.type %in% c("Financial assistance in foreign market") & eligible.firms=="firm-specific")$intervention.id)

    }



    if(keep.devaluations==F){
      master.sliced=subset(master.sliced, intervention.id!="Competitive devaluation")

    }

    if(nrow(master.sliced)==0) {
      stop.print <- "Initial filtering of the GTA dataset yielded no results fitting all specified parameters."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }

    ##### Extracting Parameter Choices from data slicer
    parameter.choices <- rbind(parameter.choices, parameter.choice.slicer)
    # rm(parameter.choice.slicer)
    print("Slicing GTA master data set ... complete.")



    ### restricted the data set to the specified exporters.
    ## Can be done here since they are always either a.un or i.un.
    ## Cannot be done for the importers!



    if(is.null(exporters)){
      exporting.country=gtalibrary::country.names$un_code
      parameter.choices=rbind(parameter.choices, data.frame(parameter="Exporting countries:", choice="All"))

    }else {

      if(keep.exporters==T){
        exporting.country=gta_un_code_vector(exporters, "exporting")

        parameter.choices=rbind(parameter.choices, data.frame(parameter="Exporting countries:", choice=paste(exporters, collapse=", ")))
      }else{
        if(keep.exporters==F){

          exporting.country=setdiff(gtalibrary::country.names$un_code,gta_un_code_vector(exporters, "exporting"))
          parameter.choices=rbind(parameter.choices, data.frame(parameter="Exporting countries:", choice=paste("All except ",paste(exporters, collapse=", "),sep="")))

        } else {
          stop.print <- "Please specify whether you want to focus on the specified exporters or exclude them (keep.exporters=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)
        }

      }

    }


    #### imposing the exporting countries incl. the relevant conditions.

    ## restrict data to the combination of selected exporters
    if(! incl.exporters.strictness %in% c("ALL","ONE","ONEPLUS")){

      stop.print <- "Please choose how to include the chosen exporters (ONE/ALL/ONEPLUS)."
      error.message <<- c(T, stop.print)
      stop(stop.print)

    } else {

      ## this is the 'ONEPLUS' case.
      exporter.combinations=unique(master.sliced$intervention.id)

      if(incl.exporters.strictness=="ALL"){

        for(cty in exporting.country){

          e.c.ids=unique(subset(master.sliced, a.un == cty & affected.flow %in% c("inward","outward subsidy"))$intervention.id)
          e.c.ids=c(e.c.ids,unique(subset(master.sliced, i.un == cty & affected.flow %in% c("outward"))$intervention.id))

          exporter.combinations=intersect(exporter.combinations,
                                          e.c.ids)
          rm(e.c.ids)
        }

      }


      if(incl.exporters.strictness=="ONE"){
        in.os=subset(master.sliced, a.un %in% exporting.country & affected.flow %in% c("inward","outward subsidy"))
        setnames(in.os, "a.un","exporter.un")
        out=subset(master.sliced, i.un %in% exporting.country& affected.flow %in% c("outward"))
        setnames(out, "i.un","exporter.un")

        one.exp=rbind(unique(in.os[,c("intervention.id","exporter.un")]),
                      unique(out[,c("intervention.id","exporter.un")]))

        one.exp=aggregate(exporter.un ~intervention.id, one.exp, function(x) length(unique(x)))

        exporter.combinations=subset(one.exp, exporter.un==1)$intervention.id

        rm(in.os, out, one.exp)

      }


      if(length(exporter.combinations)==0){

        stop.print <- "No rows left for the selected exporter combintion (parameter incl.exporters.strictness)."
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }

    }

    exporter.interventions=exporter.combinations

    ## Nr of affected exporters

    interventions.by.exporter=unique(subset(master.sliced, affected.flow %in% c("inward","outward subsidy"))[,c("intervention.id","a.un")])
    names(interventions.by.exporter)=c("intervention.id", "i.un")
    interventions.by.exporter=rbind(interventions.by.exporter,
                                    unique(subset(master.sliced, ! affected.flow %in% c("inward","outward subsidy"))[,c("intervention.id","i.un")]))
    names(interventions.by.exporter)=c("intervention.id", "exporter.un")

    ## Nr of exporters
    nr.exporter.min=nr.exporters[1]
    nr.exporter.max=nr.exporters[2]
    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Nr. of affected exporters: ", choice=paste(nr.exporter.min, nr.exporter.max, sep=" - ")))


    ## Calculation form
    if(! nr.exporters.incl %in% c("ALL","SELECTED","UNSELECTED")){

      stop.print <- "Please choose which exports to include into the calculation for the number of affected exporters (ALL/SELECTED/UNSELECTED)."
      error.message <<- c(T, stop.print)
      stop(stop.print)

    }else {

      if(nr.exporters.incl=="ALL"){

        exp.count=aggregate(exporter.un ~ intervention.id,interventions.by.exporter,function(x) length(unique(x)))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected exporters calculated based on: ", choice="All exporters"))


      }

      if(nr.exporters.incl=="SELECTED"){

        exp.count=aggregate(exporter.un ~ intervention.id,subset(interventions.by.exporter, exporter.un %in% exporting.country),function(x) length(unique(x)))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected exporters calculated based on: ", choice="Selected exporters"))

      }

      if(nr.exporters.incl=="UNSELECTED"){

        exp.count=aggregate(exporter.un ~ intervention.id,subset(interventions.by.exporter, ! exporter.un %in% exporting.country),function(x) length(unique(x)))


        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected exporters calculated based on: ", choice="Unselected exporters"))

      }


      exporter.interventions=intersect(exporter.interventions,
                                       subset(exp.count, exporter.un>=nr.exporter.min &
                                                exporter.un<=nr.exporter.max)$intervention.id)

    }



    if(length(exporter.interventions)==0){
      stop.print <- "There are no interventions that satisfy your choices for nr.exporters, nr.exporters.incl and incl.exporters.strictness simultaneously."
      error.message <<- c(T, stop.print)
      stop(stop.print)}




    ## Changing the master df to only include interventions with the desired exporters & conditions.
    ms=master.sliced[0,]

    if(nrow(subset(master.sliced, affected.flow %in% c("inward","outward subsidy")))>0){
      ms=rbind(ms, subset(master.sliced, affected.flow %in% c("inward","outward subsidy") & a.un %in% exporting.country))
    }

    if(nrow(subset(master.sliced, affected.flow=="outward"))>0){
      ms=rbind(ms, subset(master.sliced, affected.flow=="outward" & i.un %in% exporting.country))
    }

    master.sliced=subset(ms, intervention.id %in% exporter.interventions)
    master.sliced<<-master.sliced
    rm(ms)

    if(nrow(master.sliced)==0) {
      stop.print <- "Filtering the data for the specified exporters yielded zero results."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }

    ##### Intervention durations
    ## relevant parameter: coverage.period
    if(is.null(coverage.period)){
      year.start=2009
      year.end=year(Sys.Date())
    } else {
      if(length(coverage.period)!=2){
        stop.print <- paste("Please only supply coverage period years between 2008 and ", year(Sys.Date()), sep="")
        error.message <<- c(T, stop.print)
        stop(stop.print)}
      if((min(coverage.period)<2008)|(max(coverage.period)>year(Sys.Date())) ){
        stop.print <- "Please specify whether you want to focus on the specified HS codes or exclude them (keep.hs=T/F)."
        error.message <<- c(T, stop.print)
        stop(stop.print)}
      if(is.numeric(coverage.period)==F){
        stop.print <- "Please supply a coverage period vector with two integer entries e.g. c(2008, 2020)"
        error.message <<- c(T, stop.print)
        stop(stop.print)}

      if(coverage.period[1]%%1==0){year.start=coverage.period[1]}else{
        stop.print <- "Please supply a coverage period vector with two integer entries e.g. c(2008, 2020)"
        error.message <<- c(T, stop.print)
        stop(stop.print)}
      if(coverage.period[2]%%1==0){year.end=coverage.period[2]}else{
        stop.print <- "Please supply a coverage period vector with two integer entries e.g. c(2008, 2020)"
        error.message <<- c(T, stop.print)
        stop(stop.print)}
    }
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Coverage period years:", choice=paste(year.start, " to ",year.end, sep="")))


    ## adding HS code-specific durations
    # keep.environment=ls()
    # load(replica.path)
    # rm(list = setdiff(ls(), c(keep.environment, "gta_affected_tariff_line")))
    load(replica.path.atl)

    gta_affected_tariff_line$inception_date=as.Date(gta_affected_tariff_line$inception_date, "%Y-%m-%d")
    gta_affected_tariff_line$removal_date=as.Date(gta_affected_tariff_line$removal_date, "%Y-%m-%d")
    gta_affected_tariff_line=subset(gta_affected_tariff_line, (is.na(removal_date)==F)|(is.na(inception_date)==F))

    master.dates=unique(master.sliced[,c("intervention.id","affected.product","date.implemented","date.removed")])
    master.dates=unique(cSplit(master.dates, which(names(master.dates)=="affected.product"), direction="long", sep=","))

    master.dates$id=paste(master.dates$intervention.id, master.dates$affected.product, sep="-")

    tl.start=subset(gta_affected_tariff_line, is.na(inception_date)==F)[,c("intervention_id", "affected_products","inception_date")]
    tl.start$id=paste(tl.start$intervention_id, tl.start$affected_products, sep="-")

    tl.end=subset(gta_affected_tariff_line, is.na(removal_date)==F)[,c("intervention_id", "affected_products","removal_date")]
    tl.end$id=paste(tl.end$intervention_id, tl.end$affected_products, sep="-")

    master.start=subset(master.dates, id %in% tl.start$id)
    master.start$date.implemented=NULL
    master.start=merge(master.start, tl.start[,c("id","inception_date")], by="id", all.x=T)
    data.table::setnames(master.start, "inception_date","date.implemented")

    master.dates=rbind(subset(master.dates, ! id %in% tl.start$id), master.start)

    master.end=subset(master.dates, id %in% tl.end$id)
    master.end$date.removed=NULL
    master.end=merge(master.end, tl.end[,c("id","removal_date")], by="id", all.x=T)
    data.table::setnames(master.end, "removal_date","date.removed")

    master.dates=rbind(subset(master.dates, ! id %in% tl.end$id), master.end)
    master.dates$id=NULL

    rm(tl.end, tl.start,gta_affected_tariff_line)

    d.id=unique(master.dates[,c("date.implemented", "date.removed")])
    d.id$date.id=1:nrow(d.id)
    master.dates=merge(master.dates, d.id, by=c("date.implemented", "date.removed"), all.x=T)

    ## calculate intervention durations
    print("Calculating intervention durations ...")

    ms.parked=master.sliced ## sorry for the dirty trick
    master.sliced=unique(master.dates[,c("date.id", "date.implemented", "date.removed")])

    master.sliced <<- master.sliced
    gta_intervention_duration(data.path='master.sliced[,c("date.id", "date.implemented", "date.removed")]',
                              is.data.frame=TRUE,
                              years=c(year.start,year.end),
                              current.year.todate=current.year.todate)
    master.sliced=ms.parked
    rm(ms.parked)
    data.table::setnames(intervention.duration, "intervention.id","date.id")
    master.dates=unique(master.dates[,c("intervention.id","affected.product","date.id")])
    # rm(parameter.choice.duration)
    print("Calculating intervention durations ... complete.")

    ######## calculate implementer-importer-exporter-product tuples
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Data base replica source:", choice=paste("Local copy from '",replica.path,"'.", sep="")))

    print("Building intervention-importer-exporter-product tuples ...")
    master.sliced <<- master.sliced
    gta_imp_exp_hs_tuples(master.path='master.sliced',
                          master.data.frame=TRUE,
                          replica.path.tuple=replica.path.tuple)
    print("Building intervention-importer-exporter-product tuples ... complete.")
    # rm(parameter.tuple)

    if(nrow(master.tuple)==0) {
      stop.print <- "There are no affected trading relationships for your parameters."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }


    ## correct for user choice of implementers and roles
    ## relevant parameters: importers, exporters, implementers/roles.
    ### IMPORTERS

    if(is.null(importers)){
      importing.country=gtalibrary::country.names$un_code
      parameter.choices=rbind(parameter.choices, data.frame(parameter="Importing countries:", choice="All"))
    }else {

      if(keep.importers==T){
        importing.country=gta_un_code_vector(importers, "importing")
        parameter.choices=rbind(parameter.choices, data.frame(parameter="Importing countries:", choice=paste(importers, collapse=", ")))

      }else{
        if(keep.importers==F){

          importing.country=setdiff(gtalibrary::country.names$un_code,gta_un_code_vector(importers, "importing"))
          parameter.choices=rbind(parameter.choices, data.frame(parameter="Importing countries:", choice=paste("All except ",paste(importers, collapse=", "),sep="")))

        } else {
          stop.print <- "Please specify whether you want to focus on the specified importers or exclude them (keep.importers=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)
        }

      }

    }



    ### IMPLEMENTERS
    if(is.null(implementers)){
      implementing.country=gtalibrary::country.names$un_code
      parameter.choices=rbind(parameter.choices, data.frame(parameter="Implementing countries:", choice="All"))
    }else {
      if(keep.implementer){
        implementing.country=gta_un_code_vector(implementers, "implementing")
        parameter.choices=rbind(parameter.choices, data.frame(parameter="Implementing countries:", choice=paste(implementers, collapse=", ")))
      } else {
        implementing.country=setdiff(gtalibrary::country.names$un_code,gta_un_code_vector(implementers, "implementing"))
        parameter.choices=rbind(parameter.choices, data.frame(parameter="Implementing countries:", choice=paste("all except ",paste(implementers, collapse=", "), sep="")))
      }
    }



    ###### IMPLEMENTER ROLES
    if (is.null(implementer.role)==T) {
      implementer.role=c("importer", "3rd country")
    } else {
      implementer.role= tolower(implementer.role)

      if("any" %in% implementer.role){
        implementer.role=c("importer","exporter", "3rd country")
      }

      if(sum(as.numeric((implementer.role %in% c("any","importer","exporter", "3rd country"))==F))>0){
        role.error=paste("'",paste(implementer.role[(implementer.role %in% c("any","importer","exporter", "3rd country"))==F], collapse="; "),"'", sep="")
        stop.print <- paste("Unknown implementer role(s): ", role.error, ".", sep="")
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }
    }
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Implementing country role(s):", choice=paste(implementer.role, collapse=", ")))




    ## restrict data to the combination of selected importers
    if(! incl.importers.strictness %in% c("ALL","ONE","ONEPLUS")){

      stop.print <- "Please choose how to include the chosen importers (ONE/ALL/ONEPLUS)."
      error.message <<- c(T, stop.print)
      stop(stop.print)

    } else {

      ## this is the 'ONEPLUS' case.
      importer.combinations=unique(master.sliced$intervention.id)

      if(incl.importers.strictness=="ALL"){

        for(cty in importing.country){

          e.c.ids=unique(subset(master.tuple, i.un==cty)$intervention.id)

          importer.combinations=intersect(importer.combinations,
                                          e.c.ids)
          rm(e.c.ids)
        }

      }


      if(incl.importers.strictness=="ONE"){

        one.imp=subset(master.tuple, i.un %in% importing.country)
        one.imp=unique(one.imp[,c("intervention.id","i.un")])

        one.imp=aggregate(i.un ~intervention.id, one.imp, function(x) length(unique(x)))

        importer.combinations=subset(one.imp, i.un==1)$intervention.id

        rm(in.os, out, one.imp)

      }


      if(length(importer.combinations)==0){

        stop.print <- "No rows left for the selected importer combintion (parameter incl.importers.strictness)."
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }

    }
    importer.interventions=importer.combinations



    ## Nr of affected importers
    interventions.by.importer=unique(master.tuple[,c("intervention.id","i.un")])
    names(interventions.by.importer)=c("intervention.id", "importer.un")

    ## min/max nr of importers
    nr.importer.min=nr.importers[1]
    nr.importer.max=nr.importers[2]
    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Nr. of affected importers: ", choice=paste(nr.importer.min, nr.importer.max, sep=" - ")))


    ## Calculation form
    if(! nr.importers.incl %in% c("ALL","SELECTED","UNSELECTED")){

      stop.print <- "Please choose which imports to include into the calculation for the number of affected importers (ALL/SELECTED/UNSELECTED)."
      error.message <<- c(T, stop.print)
      stop(stop.print)

    }else {

      if(nr.importers.incl=="ALL"){

        imp.count=aggregate(importer.un ~ intervention.id,interventions.by.importer,function(x) length(unique(x)))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected importers calculated based on: ", choice="All importers"))


      }

      if(nr.importers.incl=="SELECTED"){

        imp.count=aggregate(importer.un ~ intervention.id,subset(interventions.by.importer, importer.un %in% importing.country),function(x) length(unique(x)))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected importers calculated based on: ", choice="Selected importers"))

      }

      if(nr.importers.incl=="UNSELECTED"){

        imp.count=aggregate(importer.un ~ intervention.id,subset(interventions.by.importer, ! importer.un %in% importing.country),function(x) length(unique(x)))


        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected importers calculated based on: ", choice="Unselected importers"))

      }


      importer.interventions=intersect(importer.interventions,
                                       subset(imp.count, importer.un>=nr.importer.min &
                                                importer.un<=nr.importer.max)$intervention.id)

    }



    if(length(importer.interventions)==0){
      stop.print <- "There are no interventions that satisfy your choices for nr.importers, nr.importers.incl and incl.importers.strictness simultaneously."
      error.message <<- c(T, stop.print)
      stop(stop.print)}

    # filter master.tuple
    print("Restricting set to stated importers/exporters ...")
    master.tuple=subset(master.tuple, i.un %in% importing.country & intervention.id %in% importer.interventions)
    master.tuple<<-master.tuple
    print("Restricting set to stated importers/exporters ... complete.")

    if(nrow(master.tuple)==0) {
      stop.print <- "Filtering the data for the specified importers yielded zero results."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }


    print("Restricting set to stated implementers and their roles ...")
    mt=data.frame(intervention.id=numeric(),i.un=numeric(), a.un=numeric(), t.un=numeric(), affected.product=numeric())
    if("importer" %in% implementer.role){
      mt=rbind(mt, subset(master.tuple, i.un==t.un & t.un %in% implementing.country))
    }

    if("exporter" %in% implementer.role){
      mt=rbind(mt, subset(master.tuple, a.un==t.un & t.un %in% implementing.country))
    }

    if("3rd country" %in% implementer.role){
      mt=rbind(mt, subset(master.tuple, a.un!=t.un & i.un!=t.un & t.un %in% implementing.country))
    }

    master.tuple=mt
    master.tuple<<-master.tuple
    rm(mt)
    print("Restricting set to stated implementers and their roles ... complete.")

    # Check # of rows
    if(nrow(master.tuple)==0) {
      stop.print <- "Unfortunately no rows remaining after filtering for implementers"
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }



    # Optional output of interventions list
    if (xlsx.interventions) {
      ## writing to disk
      print("Saving Interventions list ...")
      cols <- c("intervention.id", "implementing.jurisdiction", "title", "intervention.type", "gta.evaluation", "date.announced", "date.implemented", "date.removed")
      if (add.unpublished) {
        cols = c(cols, "current.status")
      }
      interventions.list <- subset(master.sliced, intervention.id %in% master.tuple$intervention.id)
      interventions.list <- unique(interventions.list[,cols])
      interventions.list$url <- paste0("http://www.globaltradealert.org/intervention/",interventions.list$intervention.id)

      if(is.null(output.path.interventions)){
        openxlsx::write.xlsx(interventions.list, file=paste("GTA coverage interventions list from ", Sys.Date(),".xlsx", sep=""), rowNames = F)
        print("Saving Interventions list ... completed in working directory")
      } else {
        openxlsx::write.xlsx(interventions.list, file=output.path.interventions, rowNames = F)
        # write.xlsx(parameter.choices, file=output.path, sheetName = "Parameter choices", row.names = F, append=T)
        print("Saving Interventions list ... completed in output path")
      }
    }

    ### SECTION X: Processing the data
    ## create max duration for all instruments and per importer-exporter-product year

    print("Identifying the maximum duration per year and importer-exporter-product tuple ... (this will take a while)")


    ### adjusting master tuple in case of implementer.trade calculation

    if(is.null(implementer.trade)==F){
      i.inward=subset(master.tuple, i.un==t.un)
      i.outward=subset(master.tuple, a.un==t.un)
      i.os=subset(master.tuple, i.un!=t.un & a.un!=t.un)

      if(nrow(i.inward)==0){i.inward=master.tuple[0,]}
      if(nrow(i.outward)==0){i.outward=master.tuple[0,]}
      if(nrow(i.os)==0){i.os=master.tuple[0,]}

      if(implementer.trade=="import"){
        ## inward cases remain valid.

        ## outward and OS:
        ### importing country = implementer
        i.outward$i.un=i.outward$t.un
        i.os$i.un=i.os$t.un

        ## exporting country = ALL
        if(nrow(i.outward)>0){
          i.outward$a.un=NULL
          i.outward=unique(i.outward)
          i.outward$a.un=paste(exporting.country, collapse=",")
          i.outward=cSplit(i.outward, which(names(i.outward)=="a.un"), direction="long", sep=",")
          i.outward=subset(i.outward, i.un!=a.un)
        }

        if(nrow(i.os)>0){
          i.os$a.un=NULL
          i.os=unique(i.os)
          i.os$a.un=paste(exporting.country, collapse=",")
          i.os=cSplit(i.os, which(names(i.os)=="a.un"), direction="long", sep=",")
          i.os=subset(i.os, i.un!=a.un)
        }

      }

      if(implementer.trade=="export"){

        ## outward cases remain valid.

        ## inward & outward subsidy cases:
        ### exporter = implmementer
        i.inward$a.un=i.inward$t.un
        i.os$a.un=i.os$t.un
        i.os=unique(i.os)

        ### importing country = all for inward cases
        if(nrow(i.inward)>0){
          i.inward$i.un=NULL
          i.inward=unique(i.inward)
          i.inward$i.un=paste(importing.country, collapse=",")
          i.inward=cSplit(i.inward, which(names(i.inward)=="i.un"), direction="long", sep=",")
          i.inward=subset(i.inward, i.un!=a.un)
        }

        ### importing country remains valid for OS cases (=distorted market)

      }

      if(nrow(i.inward)==0){i.inward=master.tuple[0,]}
      if(nrow(i.outward)==0){i.outward=master.tuple[0,]}
      if(nrow(i.os)==0){i.os=master.tuple[0,]}

      master.tuple=unique(rbind(i.inward, i.outward, i.os))


    }


    master.tuple$identifier=paste(master.tuple$i.un,master.tuple$a.un, master.tuple$affected.product, sep="-")

    ## need to inlcude the identifier in the case of trade coverage calculation where group.implementer==F
    ## all implementer trade calculations, group.implementer is the same as either group.exporters or group.importers for the code below (when generating the final coverage file).
    if(is.null(implementer.trade)){
      if(group.implementers==F){
        master.tuple$identifier=paste(master.tuple$i.un,master.tuple$t.un,master.tuple$a.un, master.tuple$affected.product, sep="-")
      }
    }


    duration.max=data.frame(identifier=character(), year=numeric(), share=numeric(), nr.of.hits=numeric())

    for(yr in c(year.start:year.end)){
      print(paste("Calculating maximum coverage per importer-exporter-product combination across all instruments in year ",yr,".",sep=""))
      interventions.in.force=unique(subset(master.tuple, intervention.id %in% subset(master.dates, date.id %in% unique(subset(intervention.duration, year==yr & share>0)$date.id))$intervention.id)$intervention.id)

      mt.temp=subset(master.tuple, intervention.id %in% interventions.in.force)

      md.temp=subset(master.dates, intervention.id %in% interventions.in.force)
      md.temp=merge(md.temp, subset(intervention.duration, year==yr & share>0)[,c("date.id","share")],by="date.id")

      mt.temp=merge(mt.temp, md.temp[,c("intervention.id", "affected.product","share")], by=c("intervention.id","affected.product"))
      rm(md.temp)

      relevant.mt.ids=unique(mt.temp$identifier)

      if(length(relevant.mt.ids)>0){
        nr.hits=as.data.frame(table(unique(mt.temp[,c("identifier","intervention.id")])$identifier))
        names(nr.hits)=c("identifier","nr.of.hits")
        mt.temp=unique(mt.temp[,c("identifier","share")])

        duration.temp=data.frame(identifier=character(),
                                 share=numeric())
        multiple.mention=as.character(subset(as.data.frame(table(mt.temp$identifier)), Freq>1)$Var1)

        if(length(multiple.mention)>0){
          duration.temp=subset(mt.temp, ! identifier %in% multiple.mention)

          multiple.interventions=subset(mt.temp, identifier %in% multiple.mention)

          if(length(unique(multiple.interventions$identifier[multiple.interventions$share==1]))>0){
            full.coverage=data.frame(identifier=unique(multiple.interventions$identifier[multiple.interventions$share==1]),
                                     share=1)
            duration.temp=rbind(duration.temp, full.coverage)
            multiple.interventions=subset(multiple.interventions, !identifier %in% full.coverage$identifier)

          }

          if(nrow(multiple.interventions)>0){

            tuple=multiple.interventions$identifier
            shrs =multiple.interventions$share

            dt=data.frame(identifier=unique(tuple), share=NA)
            output = character(nrow(dt))
            for(i in 1:nrow(dt)){
              output[i]=max(shrs[tuple==dt$identifier[i]])
            }
            dt$share=output
            rm(output)

            duration.temp=rbind(duration.temp, dt)
          }
        }else{
          duration.temp=mt.temp
        }

        duration.temp$year=yr

        duration.temp=merge(duration.temp, nr.hits, by="identifier", all.x=T)
        duration.max=rbind(duration.max,duration.temp)
        rm(duration.temp, nr.hits)
      }

      print(paste("Calculation of maximum coverage in year ",yr," is complete.",sep=""))

    }

    print(paste("Calculating maximum coverage per importer-exporter-product combination across all instruments complete.",sep=""))


    ## Add individual instervention.types, if called for.
    if(group.type==F){
      duration.max$intervention.type="All included instruments"

      for(inst in unique(master.sliced$intervention.type)){
        for(yr in c(year.start:year.end)){
          interventions.in.force=unique(subset(master.tuple, intervention.id %in% subset(master.dates, date.id %in% unique(subset(intervention.duration, year==yr & share>0)$date.id))$intervention.id &
                                                 intervention.id %in% subset(master.sliced, intervention.type==inst)$intervention.id)$intervention.id)

          mt.temp=subset(master.tuple, intervention.id %in% interventions.in.force)

          md.temp=subset(master.dates, intervention.id %in% interventions.in.force)
          md.temp=merge(md.temp, subset(intervention.duration, year==yr & share>0)[,c("date.id","share")],by="date.id")

          mt.temp=merge(mt.temp, md.temp[,c("intervention.id", "affected.product","share")], by=c("intervention.id","affected.product"))
          rm(md.temp)

          relevant.mt.ids=unique(mt.temp$identifier)

          if(length(relevant.mt.ids)>0){
            nr.hits=as.data.frame(table(unique(mt.temp[,c("identifier","intervention.id")])$identifier))
            names(nr.hits)=c("identifier","nr.of.hits")
            mt.temp=unique(mt.temp[,c("identifier","share")])

            duration.temp=data.frame(identifier=character(),
                                     share=numeric())
            multiple.mention=as.character(subset(as.data.frame(table(mt.temp$identifier)), Freq>1)$Var1)

            if(length(multiple.mention)>0){
              duration.temp=subset(mt.temp, ! identifier %in% multiple.mention)

              multiple.interventions=subset(mt.temp, identifier %in% multiple.mention)

              if(length(unique(multiple.interventions$identifier[multiple.interventions$share==1]))>0){
                full.coverage=data.frame(identifier=unique(multiple.interventions$identifier[multiple.interventions$share==1]),
                                         share=1)
                duration.temp=rbind(duration.temp, full.coverage)

                multiple.interventions=subset(multiple.interventions, !identifier %in% full.coverage$identifier)
              }

              if(nrow(multiple.interventions)>0){

                tuple=multiple.interventions$identifier
                shrs =multiple.interventions$share

                dt=data.frame(identifier=unique(tuple), share=NA)
                output = character(nrow(dt))
                for(i in 1:nrow(dt)){
                  output[i]=max(shrs[tuple==dt$identifier[i]])
                }
                dt$share=output
                rm(output)

                duration.temp=rbind(duration.temp, dt)
              }
            }else{
              duration.temp=mt.temp
            }

            duration.temp$year=yr
            duration.temp$intervention.type=inst
            duration.temp=merge(duration.temp, nr.hits, by="identifier", all.x=T)
            duration.max=rbind(duration.max,duration.temp)
          }

          rm(duration.temp, nr.hits)
          print(paste("Calculating maximum coverage per importer-exporter-product combination for '", inst,"' in year ",yr,".",sep=""))

        }
      }
      print(paste("Calculating maximum coverage per importer-exporter-product combination for all instruments individually complete.",sep=""))

    }

    # Check # of rows
    if(nrow(duration.max)==0) {
      stop.print <- "Unfortunately no rows remaining after filtering for intervention.types"
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }


    ## Add individual MAST chapters if called for.
    if(group.mast==F){
      duration.max$mast.chapter="All included instruments"

      for(inst in unique(master.sliced$mast.chapter)){
        for(yr in c(year.start:year.end)){
          interventions.in.force=unique(subset(master.tuple, intervention.id %in% subset(master.dates, date.id %in% unique(subset(intervention.duration, year==yr & share>0)$date.id))$intervention.id &
                                                 intervention.id %in% subset(master.sliced, mast.chapter==inst)$intervention.id)$intervention.id)

          mt.temp=subset(master.tuple, intervention.id %in% interventions.in.force)

          md.temp=subset(master.dates, intervention.id %in% interventions.in.force)
          md.temp=merge(md.temp, subset(intervention.duration, year==yr & share>0)[,c("date.id","share")],by="date.id")

          mt.temp=merge(mt.temp, md.temp[,c("intervention.id", "affected.product","share")], by=c("intervention.id","affected.product"))
          rm(md.temp)

          relevant.mt.ids=unique(mt.temp$identifier)

          if(length(relevant.mt.ids)>0){
            nr.hits=as.data.frame(table(unique(mt.temp[,c("identifier","intervention.id")])$identifier))
            names(nr.hits)=c("identifier","nr.of.hits")
            mt.temp=unique(mt.temp[,c("identifier","share")])

            duration.temp=data.frame(identifier=character(),
                                     share=numeric())
            multiple.mention=as.character(subset(as.data.frame(table(mt.temp$identifier)), Freq>1)$Var1)

            if(length(multiple.mention)>0){
              duration.temp=subset(mt.temp, ! identifier %in% multiple.mention)

              multiple.interventions=subset(mt.temp, identifier %in% multiple.mention)

              if(length(unique(multiple.interventions$identifier[multiple.interventions$share==1]))>0){
                full.coverage=data.frame(identifier=unique(multiple.interventions$identifier[multiple.interventions$share==1]),
                                         share=1)
                duration.temp=rbind(duration.temp, full.coverage)

                multiple.interventions=subset(multiple.interventions, !identifier %in% full.coverage$identifier)
              }

              if(nrow(multiple.interventions)>0){

                tuple=multiple.interventions$identifier
                shrs =multiple.interventions$share

                dt=data.frame(identifier=unique(tuple), share=NA)
                output = character(nrow(dt))
                for(i in 1:nrow(dt)){
                  output[i]=max(shrs[tuple==dt$identifier[i]])
                }
                dt$share=output
                rm(output)

                duration.temp=rbind(duration.temp, dt)
              }
            }else{
              duration.temp=mt.temp
            }

            duration.temp$year=yr
            duration.temp$mast.chapter=inst
            duration.temp=merge(duration.temp, nr.hits,by="identifier", all.x=T)
            duration.max=rbind(duration.max,duration.temp)
            rm(nr.hits)
          }

          rm(duration.temp)
          print(paste("Calculating maximum coverage per importer-exporter-product combination for MAST chapter '", inst,"' in year ",yr,".",sep=""))

        }
      }

      print(paste("Calculating maximum coverage per importer-exporter-product combination for all MAST chapters individually complete.",sep=""))

    }

    duration.max$share=as.numeric(duration.max$share)

    print("Identifying the maximum duration per year and importer-exporter-product tuple ... complete.")

    # Check # of rows
    if(nrow(duration.max)==0) {
      stop.print <- "Unfortunately no rows remaining after filtering for mast.chapters"
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }

    ##### multiply in base values
    print("Importing trade base values ...")

    if(!trade.data %in% c("base","prior year","current year", "before announcement","during announcement", paste(2005:2020))){
      stop.print <- "Please specify proper trade data choice (i.e. 'base', a year between 2005 and 2020, 'prior year' or 'current year')."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    } else{
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Underlying trade data:", choice=trade.data))

      if(is.null(trade.data.path)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Location of underlying trade data:", choice='data/support tables/Goods support table for gtalibrary.Rdata'))

      }else {
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Location of underlying trade data:", choice=trade.data.path))

      }

    }



    ## Importing relevant trade data

    if(is.null(implementer.trade)){
      gta_trade_value_bilateral(importing.country = importing.country,
                                keep.importer = TRUE,
                                exporting.country = exporting.country,
                                keep.exporter = TRUE,
                                cpc.sectors = cpc.sectors,
                                keep.cpc = keep.cpc,
                                hs.codes = hs.codes,
                                keep.hs = keep.hs,
                                trade.data=trade.data,
                                trade.data.path='data/support tables/Goods support table for gtalibrary.Rdata')
      parameter.choices=unique(rbind(parameter.choices, parameter.choice.trade.base))


      print("Importing trade base values ... completed.")
      if(nrow(trade.base.bilateral)==0) {
        stop.print <- "No trade found for the selected specifications (GTA & trade data choice)."
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }

      # Add country groups and sum up their trade values

      if (separate.importer.groups) {
        importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])
        for (i in importer.country.groups){
          trade.base.bilateral.temp <- subset(trade.base.bilateral, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])
          if ("year" %in% names(trade.base.bilateral)) {
            trade.base.bilateral.temp <- aggregate(trade.value~a.un+year+hs6, trade.base.bilateral.temp, sum) } else { trade.base.bilateral.temp <- aggregate(trade.value~a.un+hs6, trade.base.bilateral.temp, sum) }
          trade.base.bilateral.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]
          trade.base.bilateral = rbind(trade.base.bilateral, trade.base.bilateral.temp)
          rm(trade.base.bilateral.temp)
        }
      }

      if (separate.exporter.groups) {
        exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])
        for (i in exporter.country.groups){
          trade.base.bilateral.temp <- subset(trade.base.bilateral, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])
          if ("year" %in% names(trade.base.bilateral)) {
            trade.base.bilateral.temp <- aggregate(trade.value~i.un+year+hs6, trade.base.bilateral.temp, sum) } else { trade.base.bilateral.temp <- aggregate(trade.value~i.un+hs6, trade.base.bilateral.temp, sum) }
          trade.base.bilateral.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == i]
          trade.base.bilateral = rbind(trade.base.bilateral, trade.base.bilateral.temp)
          rm(trade.base.bilateral.temp)
        }
      }
    } else {

      if(implementer.trade=="import"){

        implementer.imports=unique(master.tuple$t.un)
        implementer.exports=exporting.country

      } else{
        if(implementer.trade=="export"){

          implementer.imports=importing.country
          implementer.exports=unique(master.tuple$t.un)

        } else {
          stop.print <- "Please specify proper implementer trade choice choice ('import' or 'export')."
          error.message <<- c(T, stop.print)
          stop(stop.print)
        }
      }

      gta_trade_value_bilateral(importing.country = implementer.imports,
                                keep.importer = TRUE,
                                exporting.country = implementer.exports,
                                keep.exporter = TRUE,
                                cpc.sectors = cpc.sectors,
                                keep.cpc = keep.cpc,
                                hs.codes = hs.codes,
                                keep.hs = keep.hs,
                                trade.data=trade.data,
                                trade.data.path='data/support tables/Goods support table for gtalibrary.Rdata')
      parameter.choices=unique(rbind(parameter.choices, parameter.choice.trade.base))

      if(nrow(trade.base.bilateral)==0) {
        stop.print <- "No trade found for the selected implementer specifications."
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }

      ## group data importer groups, exporter groups and implementer groups
      if (separate.importer.groups) {
        importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])
        for (i in importer.country.groups){
          trade.base.bilateral.temp <- subset(trade.base.bilateral, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])
          if ("year" %in% names(trade.base.bilateral)) {
            trade.base.bilateral.temp <- aggregate(trade.value~a.un+year+hs6, trade.base.bilateral.temp, sum) } else { trade.base.bilateral.temp <- aggregate(trade.value~a.un+hs6, trade.base.bilateral.temp, sum) }
          trade.base.bilateral.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]
          trade.base.bilateral = rbind(trade.base.bilateral, trade.base.bilateral.temp)
          rm(trade.base.bilateral.temp)
        }
      }

      if (separate.exporter.groups) {
        exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])
        for (i in exporter.country.groups){
          trade.base.bilateral.temp <- subset(trade.base.bilateral, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])
          if ("year" %in% names(trade.base.bilateral)) {
            trade.base.bilateral.temp <- aggregate(trade.value~i.un+year+hs6, trade.base.bilateral.temp, sum) } else { trade.base.bilateral.temp <- aggregate(trade.value~i.un+hs6, trade.base.bilateral.temp, sum) }
          trade.base.bilateral.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == i]
          trade.base.bilateral = rbind(trade.base.bilateral, trade.base.bilateral.temp)
          rm(trade.base.bilateral.temp)
        }
      }


      if (separate.implementer.groups) {
        implementer.country.groups = tolower(implementers[tolower(implementers) %in% tolower(country.groups$country.groups)])

        if(implementer.trade=="import"){

          for (i in implementer.country.groups){
            trade.base.bilateral.temp <- subset(trade.base.bilateral, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])
            if ("year" %in% names(trade.base.bilateral)) {
              trade.base.bilateral.temp <- aggregate(trade.value~a.un+year+hs6, trade.base.bilateral.temp, sum) } else { trade.base.bilateral.temp <- aggregate(trade.value~a.un+hs6, trade.base.bilateral.temp, sum) }
            trade.base.bilateral.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]
            trade.base.bilateral = rbind(trade.base.bilateral, trade.base.bilateral.temp)
            rm(trade.base.bilateral.temp)
          }

        }

        if(implementer.trade=="export"){

          for (i in implementer.country.groups){
            trade.base.bilateral.temp <- subset(trade.base.bilateral, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])

            if ("year" %in% names(trade.base.bilateral)) {
              trade.base.bilateral.temp <- aggregate(trade.value~i.un+year+hs6, trade.base.bilateral.temp, sum)
            } else {
              trade.base.bilateral.temp <- aggregate(trade.value~i.un+hs6, trade.base.bilateral.temp, sum)
            }

            trade.base.bilateral.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == i]
            trade.base.bilateral = rbind(trade.base.bilateral, trade.base.bilateral.temp)
            rm(trade.base.bilateral.temp)
          }

        }
      }

      rm(parameter.choice.trade.base,implementer.imports, implementer.exports)


    }


    print("Merging base values into working data frame ...")

    computational.threshold=1000000 # nr of rows I think the server can take.
    nr.splits=round(nrow(duration.max)/computational.threshold+.5,0)

    print(paste("Splitting the data set into",nr.splits,"parts for computational ease."))

    duration.max$iahs=duration.max$identifier

    if(is.null(implementer.trade)){
      if(group.implementers==F){
        duration.max$iahs=gsub("(^\\d+)-(\\d+)-(\\d+)-(\\d+$)","\\1-\\3\\-\\4",duration.max$identifier)
      }
    }


    if(trade.data %in% c("base", paste(2005:2020))){
      trade.base.bilateral$iahs=paste(trade.base.bilateral$i.un,trade.base.bilateral$a.un, trade.base.bilateral$hs6, sep="-")

      dm.split <- split(duration.max, sample(1:nr.splits, nrow(duration.max), replace=T))
      dm.final=data.frame()
      for(y in 1:nr.splits){
        dm.t = dm.split[[y]]
        dm.t=merge(dm.t, trade.base.bilateral, by="iahs", all.x=T)
        dm.t=subset(dm.t, is.na(trade.value)==F)
        dm.final = rbind(dm.final, dm.t)
        rm(dm.t)
        print(paste("Processed split", y))
      }
      master.coverage = dm.final




    } else {
      trade.base.bilateral=subset(trade.base.bilateral, year %in% c(year.start:year.end))
      trade.base.bilateral$iahs=paste(trade.base.bilateral$i.un,trade.base.bilateral$a.un, trade.base.bilateral$hs6, sep="-")

      dm.split <- split(duration.max, sample(1:nr.splits, nrow(duration.max), replace=T))
      dm.final=data.frame()
      for(y in 1:nr.splits){
        dm.t = dm.split[[y]]
        dm.t=merge(dm.t, trade.base.bilateral, by=c("iahs","year"), all.x=T)
        dm.t=subset(dm.t, is.na(trade.value)==F)
        dm.final = rbind(dm.final, dm.t)
        rm(dm.t)
        print(paste("Processed split", y))
      }
      master.coverage = dm.final

    }


    if(intra.year.duration){
      master.coverage$trade.value.affected=master.coverage$share* master.coverage$trade.value
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Adjusted for intra-year duration:", choice="Yes"))
    } else {
      master.coverage$trade.value.affected=master.coverage$trade.value
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Adjusted for intra-year duration:", choice="No"))
    }


    if(nrow(master.coverage)>computational.threshold){

      nr.splits=round(nrow(master.coverage)/computational.threshold+.5,0)

      print(paste("Splitting the remaining data set into",nr.splits,"parts for computational ease."))

      if("mast.chapter" %in% names(master.coverage) & "intervention.type" %in% names(master.coverage)){

        mc.unique=data.frame(i.un=numeric(), a.un=numeric(), affected.product=numeric(), year=numeric(), trade.value.affected=numeric(), mast.chapter=character(), intervention.type=character(), nr.of.hits=numeric())

        mc.split <- split(master.coverage, sample(1:nr.splits, nrow(master.coverage), replace=T))

        for(y in 1:nr.splits){
          mc.t = mc.split[[y]]
          mc.t = unique(mc.t[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "mast.chapter", "intervention.type","nr.of.hits")])
          names(mc.t) = c("i.un", "a.un", "affected.product","year", "trade.value.affected", "mast.chapter","intervention.type","nr.of.hits")
          mc.unique = rbind(mc.unique, mc.t)
          rm(mc.t)

          print(paste("Processed split", y))
        }

        master.coverage = unique(mc.unique)

      }else{

        if("mast.chapter" %in% names(master.coverage)){

          mc.unique=data.frame(i.un=numeric(), a.un=numeric(), affected.product=numeric(), year=numeric(), trade.value.affected=numeric(), mast.chapter=character(), nr.of.hits=numeric())

          mc.split <- split(master.coverage, sample(1:nr.splits, nrow(master.coverage), replace=T))

          for(y in 1:nr.splits){
            mc.t = mc.split[[y]]
            mc.t = unique(mc.t[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "mast.chapter","nr.of.hits")])
            names(mc.t) = c("i.un", "a.un", "affected.product","year", "trade.value.affected", "mast.chapter","nr.of.hits")
            mc.unique = rbind(mc.unique, mc.t)
            rm(mc.t)
          }

          master.coverage = unique(mc.unique)

        } else {

          if("intervention.type" %in% names(master.coverage)){

            mc.unique=data.frame(i.un=numeric(), a.un=numeric(), affected.product=numeric(), year=numeric(), trade.value.affected=numeric(), intervention.type=character(), nr.of.hits=numeric())

            mc.split <- split(master.coverage, sample(1:nr.splits, nrow(master.coverage), replace=T))

            for(y in 1:nr.splits){
              mc.t = mc.split[[y]]
              mc.t = unique(mc.t[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "intervention.type","nr.of.hits")])
              names(mc.t) = c("i.un", "a.un", "affected.product","year", "trade.value.affected","intervention.type","nr.of.hits")
              mc.unique = rbind(mc.unique, mc.t)
              rm(mc.t)
            }

            master.coverage = unique(mc.unique)


          } else {

            mc.unique=data.frame(i.un=numeric(), a.un=numeric(), affected.product=numeric(), year=numeric(), trade.value.affected=numeric(), nr.of.hits=numeric())


            mc.split <- split(master.coverage, sample(1:nr.splits, nrow(master.coverage), replace=T))

            for(y in 1:nr.splits){
              mc.t = mc.split[[y]]
              mc.t = unique(mc.t[,c("i.un", "a.un", "hs6","year", "trade.value.affected","nr.of.hits")])
              names(mc.t) = c("i.un", "a.un", "affected.product","year", "trade.value.affected","nr.of.hits")
              mc.unique = rbind(mc.unique, mc.t)
              rm(mc.t)
            }

            master.coverage = unique(mc.unique)

          }

        }

      }
    }



    print("Merging base values into working data frame ... complete")

    master.coverage=subset(master.coverage, is.na(trade.value.affected)==F)
    # Check # of rows
    if(nrow(master.coverage)==0) {
      stop.print <- "Unfortunately no rows remaining after merging trade base values into working data frame"
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }

    ### some housekeeping
    rm(duration.max)
    if(exists("full.coverage")){rm(full.coverage)}

    #### aggregating to trade coverage table
    if(grepl("share|value", trade.statistic, ignore.case = F)==F){
      stop.print <- "Please re-specify the desired trade statistic as either trade share ('share') or absolute USD value ('value')."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    } else{
      if(grepl("share", trade.statistic, ignore.case = F)){share=T}else{share=F}
    }


    print("Creating hit count brackets ...")
    if(length(hit.brackets)%%2!=0){
      stop.print <- "Please specify even hit brackets e.g. c(1,2,3,4,5,999999) for the brackets 1-2,3-4,5 or more."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    } else{

      hit.frequency=data.frame(min=hit.brackets[seq(1, length(hit.brackets),2)],
                               max=hit.brackets[seq(2, length(hit.brackets),2)])

      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Brackets for the number of interventions affecting an importer-exporter-product relationship:",
                                         choice=gsub("- 9999+","or more",paste(paste(hit.frequency$min, hit.frequency$max, sep=" - "), collapse=", "))))
    }

    print("Creating hit count brackets ... complete")

    print("Calculating aggregate annual trade coverage ...")

    base.coverage=master.coverage

    coverage.estimate=data.frame(i.un=numeric(), a.un=numeric(), year=numeric(), trade.value.affected=numeric(), hit.bracket=character(), stringsAsFactors = F)
    hit.frequency$min <- as.numeric(as.character(hit.frequency$min))
    hit.frequency$max <- as.numeric(as.character(hit.frequency$max))

    for(brkt in 1:nrow(hit.frequency)){
      final.coverage=data.frame(i.un=numeric(), a.un=numeric(), year=numeric(), trade.value.affected=numeric())
      master.coverage=subset(base.coverage, nr.of.hits>=hit.frequency$min[brkt] & nr.of.hits<=hit.frequency$max[brkt])
      total.trade=sum(subset(trade.base.bilateral, i.un<10000 & a.un<10000)$trade.value)
      tbb.yr = trade.base.bilateral
      tbb.base.yr = subset(trade.base.bilateral, i.un<10000 & a.un<10000)


      ### Aggregates across intervention types or MAST chapters


      ### have to define the importer/exporter grouping.
      ### in implementer trade calculations, this ungrouping the implementers is the same as ungrouping the ex/importer

      if(is.null(implementer.trade)==F & group.implementers==F){
        if(implementer.trade=="import"){group.importers=F}
        if(implementer.trade=="export"){group.exporters=F}
      }


      for(yr in year.start:year.end){

        ## subsetting the master.coverage DF to account for MAST/intervention types, if present.
        mc.yr=subset(master.coverage, year==yr)

        if("mast.chapter" %in% names(mc.yr)){

          ## MAST chapters
          mc.yr=subset(mc.yr, mast.chapter=="All included instruments")

        } else {

          if("intervention.type" %in% names(mc.yr)){
            ## intervention types
            mc.yr=subset(mc.yr, intervention.type=="All included instruments")
          }

        }


        if ("year" %in% names(trade.base.bilateral)) {
          total.trade=sum(subset(subset(trade.base.bilateral, i.un<10000 & a.un<10000), year == yr)$trade.value)
          tbb.yr = subset(trade.base.bilateral, year == yr)
          tbb.base.yr = subset(subset(trade.base.bilateral, i.un<10000 & a.un<10000), year == yr)
        }

        if(nrow(mc.yr)==0){

          if(group.importers==T & group.exporters==T){
            final.coverage=rbind(final.coverage, data.frame(i.un=999,
                                                            a.un=999,
                                                            year=yr,
                                                            trade.value.affected=0))
          }

          if(group.importers==F & group.exporters==T){

            final.coverage=rbind(final.coverage, data.frame(i.un=unique(master.coverage$i.un),
                                                            a.un=999,
                                                            year=yr,
                                                            trade.value.affected=0))
          }

          if(group.importers==T & group.exporters==F){
            final.coverage=rbind(final.coverage, data.frame(i.un=999,
                                                            a.un=unique(master.coverage$a.un),
                                                            year=yr,
                                                            trade.value.affected=0))
          }

          if(group.importers==F & group.exporters==F){
            fc.temp=expand.grid(unique(master.coverage$i.un), unique(master.coverage$a.un))
            names(fc.temp)=c("i.un","a.un")
            fc.temp$year=yr
            fc.temp$trade.value.affected=0

            final.coverage=rbind(final.coverage, fc.temp)
          }

          if(separate.importer.groups | separate.exporter.groups){

            fc.temp.groups = final.coverage[0,]

            if(separate.importer.groups==T & separate.exporter.groups==T){

              importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])
              exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])

              for (i in importer.country.groups) {
                for (e in exporter.country.groups){
                  final.coverage=rbind(final.coverage, data.frame(i.un=country.groups$code[tolower(country.groups$country.groups) == i],
                                                                  a.un=country.groups$code[tolower(country.groups$country.groups) == e],
                                                                  year=yr,
                                                                  trade.value.affected=0))
                }
              }
            }

            if(separate.importer.groups==T) {

              importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])

              for (i in importer.country.groups){

                if(group.exporters==T){
                  final.coverage=rbind(final.coverage, data.frame(i.un=country.groups$code[tolower(country.groups$country.groups) == i],
                                                                  a.un=999,
                                                                  year=yr,
                                                                  trade.value.affected=0))
                } else {

                  final.coverage=rbind(final.coverage, data.frame(i.un=country.groups$code[tolower(country.groups$country.groups) == i],
                                                                  a.un=unique(subset(master.coverage, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])$a.un),
                                                                  year=yr,
                                                                  trade.value.affected=0))
                }
              }
            }

            if(separate.exporter.groups==T) {

              exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])

              for (e in exporter.country.groups){

                if(group.importers==T){
                  final.coverage=rbind(final.coverage, data.frame(i.un=999,
                                                                  a.un=country.groups$code[tolower(country.groups$country.groups) == e],
                                                                  year=yr,
                                                                  trade.value.affected=0))
                } else {

                  final.coverage=rbind(final.coverage, data.frame(i.un=unique(subset(master.coverage, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e])$i.un),
                                                                  a.un=country.groups$code[tolower(country.groups$country.groups) == e],
                                                                  year=yr,
                                                                  trade.value.affected=0))
                }
              }
            }
          }

        } else {

          if(group.importers==T & group.exporters==T){
            fc.temp=aggregate(trade.value.affected ~ year, mc.yr, sum)
            fc.temp$i.un=999
            fc.temp$a.un=999

            if(share){fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade}

            final.coverage=rbind(final.coverage, fc.temp)
          }

          if(group.importers==F & group.exporters==T){
            fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.yr, sum)
            if(share){
              fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, subset(trade.base.bilateral, i.un<10000 & a.un<10000), sum), by="i.un", all.x=T)
              fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
              fc.temp$trade.value=NULL
            }

            fc.temp$a.un=999
            final.coverage=rbind(final.coverage, fc.temp)
          }

          if(group.importers==T & group.exporters==F){
            fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.yr, sum)

            if(share){
              fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, tbb.base.yr, sum), by="a.un", all.x=T)
              fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
              fc.temp$trade.value=NULL
            }

            fc.temp$i.un=999
            final.coverage=rbind(final.coverage, fc.temp)
          }

          if(group.importers==F & group.exporters==F){
            fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.yr, sum)


            if(share){
              fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.base.yr, sum), by=c("i.un","a.un"), all.x=T)
              fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
              fc.temp$trade.value=NULL
            }

            final.coverage=rbind(final.coverage, fc.temp)
          }

          if(separate.importer.groups | separate.exporter.groups){

            fc.temp.groups = final.coverage[0,]

            if(separate.importer.groups==T & separate.exporter.groups==T){

              importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])
              exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])

              for (i in importer.country.groups) {
                for (e in exporter.country.groups){
                  if(nrow(subset(mc.yr, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i] & a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]))>0) {
                    fc.temp=aggregate(trade.value.affected ~ year, subset(mc.yr, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i] & a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]), sum)
                    fc.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]
                    fc.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == e]

                    if(share){
                      fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un", "a.un"), all.x=T)
                      fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                      fc.temp$trade.value=NULL
                    }

                    fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                  }
                }
              }
            }

            if(separate.importer.groups==T) {

              importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])

              for (i in importer.country.groups){
                if(nrow(subset(mc.yr, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i]))>0){
                  if(group.exporters==T){
                    fc.temp=aggregate(trade.value.affected ~ year, subset(mc.yr, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i]), sum)
                    fc.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]

                    if(share){
                      fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, subset(tbb.yr, ! a.un %in% country.groups$code), sum), by=c("i.un"), all.x=T)
                      fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                      fc.temp$trade.value=NULL
                    }

                    fc.temp$a.un = 999
                    fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                  } else {

                    fc.temp=aggregate(trade.value.affected ~ a.un + year, subset(mc.yr, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i]), sum)
                    fc.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]

                    if(share){
                      fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un", "a.un"), all.x=T)
                      fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                      fc.temp$trade.value=NULL
                    }
                    fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                  }
                }
              }
            }

            if(separate.exporter.groups==T) {

              exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])

              for (e in exporter.country.groups){
                if(nrow(subset(mc.yr, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]))>0){
                  if(group.importers==T){
                    fc.temp=aggregate(trade.value.affected ~ year, subset(mc.yr, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]), sum)
                    fc.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == e]

                    if(share){
                      fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, subset(tbb.yr, ! i.un %in% country.groups$code), sum), by=c("a.un"), all.x=T)
                      fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                      fc.temp$trade.value=NULL
                    }

                    fc.temp$i.un = 999
                    fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                  } else {

                    fc.temp=aggregate(trade.value.affected ~ i.un + year, subset(mc.yr, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]), sum)
                    fc.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == e]

                    if(share){
                      fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un", "a.un"), all.x=T)
                      fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                      fc.temp$trade.value=NULL
                    }
                    fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                  }
                }
              }

            }

            final.coverage = rbind(final.coverage, fc.temp.groups)
            rm(fc.temp.groups)

          }

        }
      }
      print("Calculating aggregate annual trade coverage ... completed")

      # Check # of rows
      if(nrow(final.coverage)==0) {
        stop.print <- "Unfortunately no rows remaining after calculating aggregate annual trade coverage"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }

      ### by intervention type or MAST chapter, if necessary
      if(group.type==F | group.mast==F){

        ## decide over what classification to loop.
        ## I ignore the case where both MAST & intervention types are ungrouped, for now.
        if(group.mast==F){
          ungroup.mast=T
        } else {
          ungroup.mast=F
        }

        if(ungroup.mast){
          final.coverage$instrument="All included MAST chapters"
          print("Calculating aggregate annual trade coverage per included MAST chapter ...")
          master.coverage$instrument=master.coverage$mast.chapter
          loop.instruments=unique(master.sliced$mast.chapter)

        } else {
          final.coverage$instrument="All included instruments"
          print("Calculating aggregate annual trade coverage per included intervention type ...")
          master.coverage$instrument=master.coverage$intervention.type
          loop.instruments=unique(master.sliced$intervention.type)

        }



        for(inst in loop.instruments){
          mc.inst=subset(master.coverage, instrument==inst)

          if(group.importers==T & group.exporters==T){

            if(nrow(mc.inst)==0){
              final.coverage=rbind(final.coverage,
                                   data.frame(year=unique(master.coverage$year),
                                              trade.value.affected=0,
                                              instrument=inst,
                                              i.un=999,
                                              a.un=999))


            } else {

              fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
              if(share){fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade}
              fc.temp$instrument=inst
              fc.temp$i.un=999
              fc.temp$a.un=999
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          if(group.importers==F & group.exporters==T){


            if(nrow(mc.inst)==0){
              fc.temp=expand.grid(unique(master.coverage$year), unique(master.coverage$i.un))
              names(fc.temp)=c("year","i.un")
              fc.temp$trade.value.affected=0
              fc.temp$instrument=inst
              fc.temp$a.un=999

              final.coverage=rbind(final.coverage,
                                   fc.temp)


            } else {
              fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, tbb.base.yr, sum), by="i.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$instrument=inst
              fc.temp$a.un=999
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          if(group.importers==T & group.exporters==F){

            if(nrow(mc.inst)==0){
              fc.temp=expand.grid(unique(master.coverage$year), unique(master.coverage$a.un))
              names(fc.temp)=c("year","a.un")
              fc.temp$trade.value.affected=0
              fc.temp$instrument=inst
              fc.temp$i.un=999

              final.coverage=rbind(final.coverage,
                                   fc.temp)



            } else {

              fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, tbb.base.yr, sum), by="a.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$instrument=inst
              fc.temp$i.un=999
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          if(group.importers==F & group.exporters==F){


            if(nrow(mc.inst)==0){
              fc.temp=data.frame(i.un=numeric(),
                                 a.un=numeric(),
                                 year=numeric())

              for(yy in unique(master.coverage$year)){
                ft=expand.grid(unique(master.coverage$i.un), unique(master.coverage$a.un))
                names(ft)=c("i.un", "a.un")
                ft$year=yy

                fc.temp=rbind(fc.temp,ft)
                rm(ft)
              }

              fc.temp$trade.value.affected=0
              fc.temp$instrument=inst

              final.coverage=rbind(final.coverage,
                                   fc.temp)

            }  else{

              fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.base.yr, sum), by=c("i.un","a.un"), all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$instrument=inst
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          if(separate.importer.groups | separate.exporter.groups){

            if(nrow(mc.inst)==0){

              if(separate.importer.groups==T & separate.exporter.groups==T){

                importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])
                exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])

                for (i in importer.country.groups) {
                  for (e in exporter.country.groups){
                    final.coverage=rbind(final.coverage, data.frame(i.un=country.groups$code[tolower(country.groups$country.groups) == i],
                                                                    a.un=country.groups$code[tolower(country.groups$country.groups) == e],
                                                                    year=unique(master.coverage$year),
                                                                    instrument=inst,
                                                                    trade.value.affected=0))
                  }
                }
              }

              if(separate.importer.groups==T) {

                importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])

                for (i in importer.country.groups){

                  if(group.exporters==T){

                    fc.temp=expand.grid(unique(master.coverage$year), country.groups$code[tolower(country.groups$country.groups) == i])
                    names(fc.temp)=c("year","i.un")
                    fc.temp$trade.value.affected=0
                    fc.temp$instrument=inst
                    fc.temp$a.un=999

                    final.coverage=rbind(final.coverage,
                                         fc.temp)

                  } else {

                    fc.temp=expand.grid(unique(master.coverage$year), unique(subset(master.coverage, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i])$a.un))
                    names(fc.temp)=c("year","a.un")
                    fc.temp$trade.value.affected=0
                    fc.temp$instrument=inst
                    fc.temp$i.un=country.groups$code[tolower(country.groups$country.groups) == i]

                    final.coverage=rbind(final.coverage,
                                         fc.temp)

                  }
                }
              }

              if(separate.exporter.groups==T) {

                exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])

                for (e in exporter.country.groups){

                  if(group.importers==T){

                    fc.temp=expand.grid(unique(master.coverage$year), country.groups$code[tolower(country.groups$country.groups) == e])
                    names(fc.temp)=c("year","a.un")
                    fc.temp$trade.value.affected=0
                    fc.temp$instrument=inst
                    fc.temp$i.un=999

                    final.coverage=rbind(final.coverage,
                                         fc.temp)
                  } else {

                    fc.temp=expand.grid(unique(master.coverage$year), unique(subset(master.coverage, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e])$i.un))
                    names(fc.temp)=c("year","i.un")
                    fc.temp$trade.value.affected=0
                    fc.temp$instrument=inst
                    fc.temp$a.un=country.groups$code[tolower(country.groups$country.groups) == e]

                    final.coverage=rbind(final.coverage,
                                         fc.temp)

                  }
                }
              }

            } else {

              fc.temp.groups = final.coverage[0,]

              if(separate.importer.groups==T & separate.exporter.groups==T){

                importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])
                exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(country.groups$country.groups)])

                for (i in importer.country.groups) {
                  for (e in exporter.country.groups){
                    if(nrow(subset(mc.inst, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i] & a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]))>0) {
                      fc.temp=aggregate(trade.value.affected ~ year, subset(mc.inst, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i] & a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]), sum)
                      fc.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]
                      fc.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == e]

                      if(share){
                        fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un", "a.un"), all.x=T)
                        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                        fc.temp$trade.value=NULL
                      }
                      fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                    }
                  }
                }
              }

              if(separate.importer.groups==T) {

                importer.country.groups = tolower(importers[tolower(importers) %in% tolower(country.groups$country.groups)])

                for (i in importer.country.groups){
                  if(nrow(subset(mc.inst, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i]))>0) {
                    if(group.exporters==T){
                      fc.temp=aggregate(trade.value.affected ~ year, subset(mc.inst, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i]), sum)
                      fc.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]

                      if(share){
                        fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, subset(tbb.yr, ! a.un %in% country.groups$code), sum), by=c("i.un"), all.x=T)
                        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                        fc.temp$trade.value=NULL
                      }

                      fc.temp$a.un = 999
                      fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                    } else {

                      fc.temp=aggregate(trade.value.affected ~ a.un + year, subset(mc.inst, i.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == i]), sum)
                      fc.temp$i.un = country.groups$code[tolower(country.groups$country.groups) == i]

                      if(share){
                        fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un", "a.un"), all.x=T)
                        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                        fc.temp$trade.value=NULL
                      }
                      fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                    }
                  }
                }
              }

              if(separate.exporter.groups==T) {

                exporter.country.groups = tolower(exporters[tolower(exporters) %in% tolower(tolower(country.groups$country.groups))])

                for (e in exporter.country.groups){
                  if(nrow(subset(mc.inst, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]))>0) {

                    if(group.importers==T){
                      fc.temp=aggregate(trade.value.affected ~ year, subset(mc.inst, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]), sum)
                      fc.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == e]

                      if(share){
                        fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, subset(tbb.yr, ! i.un %in% country.groups$code), sum), by=c("a.un"), all.x=T)
                        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                        fc.temp$trade.value=NULL
                      }

                      fc.temp$i.un = 999
                      fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                    } else {

                      fc.temp=aggregate(trade.value.affected ~ i.un + year, subset(mc.inst, a.un %in% country.correspondence$un_code[tolower(country.correspondence$name) == e]), sum)
                      fc.temp$a.un = country.groups$code[tolower(country.groups$country.groups) == e]

                      if(share){
                        fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un", "a.un"), all.x=T)
                        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                        fc.temp$trade.value=NULL
                      }
                      fc.temp.groups = rbind(fc.temp.groups, fc.temp)
                    }
                  }
                }

              }

              fc.temp.groups$instrument=inst
              final.coverage = rbind(final.coverage, fc.temp.groups)
              rm(fc.temp.groups)
            }
          }

          print(paste("Calculated aggregate annual trade coverage for ", inst, sep=""))
        }

        if(ungroup.mast){
          setnames(final.coverage, "instrument","mast.chapter")
        }else {
          setnames(final.coverage, "instrument","intervention.type")
        }
        master.coverage$instrument=NULL
        print("Calculating aggregate annual trade coverage per included intervention type ... concluded")
      }

      # Check # of rows
      if(nrow(final.coverage)==0) {
        stop.print <- "Unfortunately no rows remaining after calculating aggregate annual trade coverage per intervention type/MAST chapter."
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }

      ce=final.coverage
      ce$hit.bracket=gsub("- 9999+","or more",paste(hit.frequency$min[brkt], hit.frequency$max[brkt], sep=" - "))
      coverage.estimate=rbind(coverage.estimate, ce)
    }
    final.coverage=coverage.estimate
    rm(coverage.estimate)


    ## Concluding cosmetics
    print("Making it pretty ...")
    ## importer names
    countries=gtalibrary::country.names[,c("un_code","name","jurisdiction.id")]
    countries$name=as.character(countries$name)

    # rbind country group names if necessary
    if (separate.importer.groups | separate.exporter.groups) {
      cty.groups = gtalibrary::country.groups
      cty.groups$un_code = cty.groups$code
      cty.groups$name = cty.groups$country.groups
      cty.groups$jurisdiction.id = cty.groups$code
      cty.groups = cty.groups[,c("un_code","name","jurisdiction.id")]
      countries = rbind(countries, cty.groups)
      rm(cty.groups)
    }

    countries=rbind(countries[,c("name","un_code")], data.frame(name="All included importers", un_code=999))
    names(countries)=c("importer", "i.un")
    final.coverage=merge(final.coverage, countries, by="i.un", all.x=T)
    final.coverage$i.un=NULL

    ## exporter names
    names(countries)=c("exporter", "a.un")
    countries$exporter[countries$a.un==999]="All included exporters"
    final.coverage=merge(final.coverage, countries, by="a.un", all.x=T)
    final.coverage$a.un=NULL



    ## making it nice
    if(sum(as.numeric(c("intervention.type","mast.chapter") %in% names(final.coverage)))==0){

      final.coverage=reshape(final.coverage, idvar=c("importer","exporter","hit.bracket"), timevar = "year", direction="wide")
      # pretty column names
      setnames(final.coverage, "importer", "Importing country")
      setnames(final.coverage, "exporter", "Exporting country")
      setnames(final.coverage, "hit.bracket", "Number of interventions affecting exported product")
      column.order=c("Importing country", "Exporting country","Number of interventions affecting exported product")
      for(yr in year.start:year.end){
        names(final.coverage)[grepl(yr, names(final.coverage))==T]=paste("Trade coverage estimate for ",yr, sep="")
        column.order=c(column.order, paste("Trade coverage estimate for ",yr, sep=""))
      }

      final.coverage[is.na(final.coverage)]=0
      trade.coverage.estimates<<-final.coverage[,column.order]
    }

    ## MAST chapter names, if necessary, else reshape with or without intervention types
    if("mast.chapter" %in% names(final.coverage)){
      mast.names=unique(gtalibrary::int.mast.types[,c("mast.chapter.id","mast.chapter.name")])
      names(mast.names)=c("mast.chapter", "mast.chapter.name")
      mast.names$mast.chapter=as.character(mast.names$mast.chapter)
      mast.names$mast.chapter.name=as.character(mast.names$mast.chapter.name)


      final.coverage=merge(final.coverage, mast.names, by="mast.chapter", all.x=T)
      final.coverage$mast.chapter.name[final.coverage$mast.chapter=="All included MAST chapters"]="All included MAST chapters"

      final.coverage=reshape(final.coverage, idvar=c("importer","exporter","mast.chapter", "mast.chapter.name","hit.bracket"), timevar = "year", direction="wide")


      # pretty column names
      setnames(final.coverage, "importer", "Importing country")
      setnames(final.coverage, "exporter", "Exporting country")
      setnames(final.coverage, "mast.chapter", "MAST chapter ID")
      setnames(final.coverage, "mast.chapter.name", "MAST chapter name")
      setnames(final.coverage, "hit.bracket", "Number of interventions affecting exported product")
      column.order=c("Importing country", "Exporting country","MAST chapter ID", "MAST chapter name","Number of interventions affecting exported product")
      for(yr in year.start:year.end){
        names(final.coverage)[grepl(yr, names(final.coverage))==T]=paste("Trade coverage estimate for ",yr, sep="")
        column.order=c(column.order, paste("Trade coverage estimate for ",yr, sep=""))
      }

      final.coverage[is.na(final.coverage)]=0
      trade.coverage.estimates<<-final.coverage[,column.order]
    }


    if("intervention.type" %in% names(final.coverage)){

      final.coverage=reshape(final.coverage, idvar=c("importer","exporter","intervention.type","hit.bracket"), timevar = "year", direction="wide")


      # pretty column names
      setnames(final.coverage, "importer", "Importing country")
      setnames(final.coverage, "exporter", "Exporting country")
      setnames(final.coverage, "intervention.type", "Intervention type")
      setnames(final.coverage, "hit.bracket", "Number of interventions affecting exported product")

      column.order=c("Importing country", "Exporting country","Intervention type", "Number of interventions affecting exported product")
      for(yr in year.start:year.end){
        names(final.coverage)[grepl(yr, names(final.coverage))==T]=paste("Trade coverage estimate for ",yr, sep="")
        column.order=c(column.order, paste("Trade coverage estimate for ",yr, sep=""))
      }

      final.coverage[is.na(final.coverage)]=0
      trade.coverage.estimates<<-final.coverage[,column.order]
    }

    if(get.hs.level.data){
      coverage.by.hs.tp.yr.tuple<<-master.coverage
    }

    print("Oooh that's pretty ...")

    ## writing to disk
    if (xlsx==T) {
      print("Saving XLSX ...")
      if(is.null(output.path)){
        xlsxList = list("Estimates" = trade.coverage.estimates, "Parameter choices" = parameter.choices)
        openxlsx::write.xlsx(xlsxList, file=paste("GTA trade coverage estimates from ", Sys.Date(),".xlsx", sep=""), rowNames = F)
        # write.xlsx(trade.coverage.estimates, file=paste("GTA trade coverage estimates from ", Sys.Date(),".xlsx", sep=""), sheetName = "Estimates", row.names = F)
        # write.xlsx(parameter.choices, file=paste("GTA trade coverage estimates from ", Sys.Date(),".xlsx", sep=""), sheetName = "Parameter choices", row.names = F, append=T)
        print("Saving XLSX ... completed in working directory")
      } else {
        xlsxList = list("Estimates" = trade.coverage.estimates, "Parameter choices" = parameter.choices)
        openxlsx::write.xlsx(xlsxList, file=output.path, rowNames = F)
        # write.xlsx(parameter.choices, file=output.path, sheetName = "Parameter choices", row.names = F, append=T)
        print("Saving XLSX ... completed in output path")
      }
    }


    # bilateral.trade<<-trade.base.bilateral
    parameter.choices<<-parameter.choices
    error.message <<- FALSE
    if(xlsx.interventions) {
      interventions.list <<- interventions.list
    }

  },

  error = function(error.msg) {
    if(exists("stop.print")){
      error.message <<- c(T, stop.print)
      print(paste("[ERROR TRADE COVERAGE]: ",stop.print, sep=""))
    } else {
      error.message <<- c(T,error.msg$message)
      print(paste("[ERROR TRADE COVERAGE]: ",error.msg$message, sep=""))

    }
  })

}
