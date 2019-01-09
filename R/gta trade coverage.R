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
#' @param affected.flows Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is 'any'. Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
#' @param importers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for importers in the sample. Default: All importers.
#' @param keep.importers Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated importers.
#' @param group.importers Specify whether to aggregate the statistics for all remaining importers into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
#' @param nr.also.importers Specify the maximum number of importers affected in addition to the specified affected countries. Default is any number. Provide value as integer.
#' @param jointly.affected.importers Specify whether included interventions shall affect all specified importing countries jointly ('TRUE') or jointly as well as individually ('FALSE'). Default is 'FALSE'.
#' @param exporters Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for exporters in the sample. Default: All exporters.
#' @param keep.exporters Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated exporters.
#' @param group.exporters Specify whether to aggregate the statistics for all remaining exporters into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
#' @param nr.also.exporters Specify the maximum number of exporters affected in addition to the specified affected countries. Default is any number. Provide value as integer.
#' @param jointly.affected.exporters Specify whether included interventions shall affect all specified exporting countries jointly ('TRUE') or jointly as well as individually ('FALSE'). Default is 'FALSE'.
#' @param implementers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for implementers in the sample. Default: World (as in implemented by one).
#' @param implementer.role Bilateral trade flows can be affected by multiple actors. Specify which actor's interventions you want to include. There are three roles: importer, exporter and 3rd country. Combinations are permissible. Default: c('importer','3rd country').
#' @param keep.implementer Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementing country.
#' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
#' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
#' @param revocation.period Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param in.force.today Specify whether you want to focus on interventions in force today ('TRUE') or no longer in force today ('FALSE'). Default is 'any'.
#' @param intervention.types Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
#' @param keep.type Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
#' @param group.type Specify whether to aggregate the statistics for all remaining intervention types into one group (TRUE) or whether create the statistics for every single type (FALSE). Default is TRUE.
#' @param mast.chapters Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
#' @param keep.mast Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
#' @param group.mast Specify whether to aggregate the statistics for all remaining MAST chapters into one group (TRUE) or whether create the statistics for every single chapter (FALSE). Default is TRUE.
#' @param implementation.level Specify the government level responsible for the intervention.  Default is 'any'. Permissible values are 'supranational', 'national', 'subnational', 'IFI', 'NFI' or combinations thereof. IFI and NFI refer to government-owned financial institutions that are owned by one ('NFI') or more ('IFI') governments.
#' @param keep.level Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementation levels.
#' @param eligible.firms Concentrate the analysis on interventions that are targeted at certain subsets. Default is 'any'. Permissible values are 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise' or combinations thereof.
#' @param keep.firms Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated firm subsets.
#' @param cpc.sectors Provide a vector of CPC codes that you are interested in (version 2.1, 3-digit level).
#' @param keep.cpc Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
#' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
#' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
#' @param hit.brackets Specify whether to calculate the trade shares by the number of interventions affecting a importer-exporter-product combination e.g. c(1,2,3,4,5,999999) for the brackets '1-2,3-4,5 or more'. Default is c(1,99999).
#' @param intervention.ids Provide a vector of intervention IDs.
#' @param keep.interventions Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated intervention IDs.
#' @param lag.adjustment Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
#' @param intra.year.duration Adjust the estimates for the number of days the relevant intervention has been in force in the given year (TRUE/FALSE). Default is TRUE.
#' @param trade.statistic Choose to calculate trade shares ('share') or absolute USD values ('value'). Default is 'share'.
#' @param trade.data Choose the trade data underlying these calulations. Choices are individual years between 2007 and 2017, the GTA base period data ('base', averages for 2005-2007) as well as moving trade data as a function of coverage year ('prior year' and 'current year'). Default is 'base'.
#' @param trade.data.path Set path of trade data file (default is 'data/support tables/Goods support table for gtalibrary.Rdata'),
#' @param rdata Takes value TRUE or FALSE. If TRUE, Rdata file will be stored alongside xlsx. Default: FALSE
#' @param xlsx Takes value TRUE or FALSE. If TRUE, xlsx file will be stored. Default: FALSE
#' @param output.path Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.

#' @return Outputs a table with coverage shares ranging from 2009 to 2018 for each importer, exporter, implementer, instrument combination.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


# Function infos and parameters  --------------------------------------------

gta_trade_coverage <- function(
  data.path="data/master_plus.Rdata",
  replica.path="data/database replica/database replica - parts - base.Rdata",
  coverage.period=NULL,
  current.year.todate=TRUE,
  gta.evaluation= NULL,
  affected.flows = c("inward", "outward subsidy"),
  importers = NULL,
  keep.importers = NULL,
  group.importers = TRUE,
  nr.also.importers=NULL,
  jointly.affected.importers=FALSE,
  exporters = NULL,
  keep.exporters = NULL,
  group.exporters = TRUE,
  nr.also.exporters=NULL,
  jointly.affected.exporters=FALSE,
  implementers = NULL,
  implementer.role = NULL,
  keep.implementer= TRUE,
  announcement.period = NULL,
  implementation.period = NULL,
  revocation.period = NULL,
  in.force.today = NULL,
  intervention.types = NULL,
  keep.type = NULL,
  group.type=TRUE,
  mast.chapters = NULL,
  keep.mast = NULL,
  group.mast=TRUE,
  implementation.level = NULL,
  keep.level = NULL,
  eligible.firms = NULL,
  keep.firms = NULL,
  cpc.sectors = NULL,
  keep.cpc = NULL,
  hs.codes = NULL,
  keep.hs = NULL,
  hit.brackets=c(1,99999),
  intervention.ids = NULL,
  keep.interventions = NULL,
  lag.adjustment=NULL,
  intra.year.duration=TRUE,
  trade.statistic="share",
  trade.data="base",
  trade.data.path="data/support tables/Goods support table for gtalibrary.Rdata",
  rdata = FALSE,
  xlsx = FALSE,
  output.path = NULL) {


  # Initialising Function ---------------------------------------------------

  # load libraries
  library("xlsx")
  library("splitstackshape")
  library("data.table")

  ######## Feed data slicer

  ## Collecting parameter values
  parameter.choices=data.frame(parameter=character(), choice=character(),stringsAsFactors = F)

  tryCatch({

    print("Slicing GTA master data set ...")
    gta_data_slicer(data.path=data.path,
                    gta.evaluation= gta.evaluation,
                    affected.flows = affected.flows,
                    announcement.period = announcement.period,
                    implementation.period = implementation.period,
                    keep.implementation.na=F,
                    revocation.period = revocation.period,
                    in.force.today = in.force.today,
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
                    lag.adjustment=lag.adjustment)


    ## removing certain problemtic, wide-reaching cases until further investigation
    out=c(20387, 20389, 16408, 16817, 15248, 20098, 56907)
    master.sliced=subset(master.sliced, ! intervention.id %in% out)

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


    ## imposing the exporting countries incl. the relevant conditions.

    interventions.by.exporter=unique(subset(master.sliced, affected.flow %in% c("inward","outward subsidy"))[,c("intervention.id","a.un")])
    names(interventions.by.exporter)=c("intervention.id", "i.un")
    interventions.by.exporter=rbind(interventions.by.exporter,
                                    unique(subset(master.sliced, ! affected.flow %in% c("inward","outward subsidy"))[,c("intervention.id","i.un")]))
    names(interventions.by.exporter)=c("intervention.id", "exporter.un")


    if(is.null(exporters)){
      exporter.count=aggregate(exporter.un ~ intervention.id, interventions.by.exporter,  function(x) length(unique(x)))
      names(exporter.count)=c("intervention.id", "nr.exporters.hit")
      sole.exporter=numeric()

    }else {
      sole.exporter=setdiff(unique(subset(interventions.by.exporter, exporter.un %in% exporting.country)$intervention.id),
                            unique(subset(interventions.by.exporter, ! exporter.un %in% exporting.country)$intervention.id))
      exporter.count=aggregate(exporter.un ~ intervention.id, subset(interventions.by.exporter,! exporter.un %in% exporting.country),  function(x) length(unique(x)))
      names(exporter.count)=c("intervention.id", "nr.exporters.hit")

    }


    if(is.null(nr.also.exporters)){
      exporter.interventions=unique(interventions.by.exporter$intervention.id)
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Nr. of also affected exporters:", choice="Any"))
    }else{

      exporter.interventions=c(sole.exporter,unique(subset(exporter.count, nr.exporters.hit<=nr.also.exporters)$intervention.id))
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Nr. of also affected exporters:", choice=paste(nr.also.exporters, " or less", sep="")))
    }




    if(is.null(jointly.affected.exporters)){
      exporter.interventions=exporter.interventions
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Only include interventions where all specified exporters are affected:", choice="No, just at least one of them."))
    }else{
      if(jointly.affected.exporters){
        exporter.interventions=intersect(exporter.interventions, subset(interventions.by.exporter, exporter.un==exporting.country[1])$intervention.id)

        if(length(exporting.country)>1){
          for(k in 2:length(exporting.country)){
            exporter.interventions=intersect(exporter.interventions, subset(interventions.by.exporter, exporter.un==exporting.country[k])$intervention.id)
          }
        }

        if(length(exporter.interventions)==0){
          stop.print <- "There are no interventions that jointly affect all specified exporters."
          error.message <<- c(T, stop.print)
          stop(stop.print)}

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Only include interventions where all specified exporters are affected:", choice="Yes."))

      }else{
        exporter.interventions=exporter.interventions
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Only include interventions where all specified exporters are affected:", choice="No, just at least one of them."))
      }
    }

    ms=master.sliced[0,]

    if(nrow(subset(master.sliced, affected.flow %in% c("inward","outward subsidy")))>0){
      ms=rbind(ms, subset(master.sliced, affected.flow %in% c("inward","outward subsidy") & a.un %in% exporting.country & intervention.id %in% exporter.interventions))
    }

    if(nrow(subset(master.sliced, affected.flow=="outward"))>0){
      ms=rbind(ms, subset(master.sliced, affected.flow=="outward" & i.un %in% exporting.country & intervention.id %in% exporter.interventions))
    }
    master.sliced=ms
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
        stop.print <- "Please supply a coverage period vector with two integer entries e.g. c(2008, 2018)"
        error.message <<- c(T, stop.print)
        stop(stop.print)}

      if(coverage.period[1]%%1==0){year.start=coverage.period[1]}else{
        stop.print <- "Please supply a coverage period vector with two integer entries e.g. c(2008, 2018)"
        error.message <<- c(T, stop.print)
        stop(stop.print)}
      if(coverage.period[2]%%1==0){year.end=coverage.period[2]}else{
        stop.print <- "Please supply a coverage period vector with two integer entries e.g. c(2008, 2018)"
        error.message <<- c(T, stop.print)
        stop(stop.print)}
    }
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Coverage period years:", choice=paste(year.start, " to ",year.end, sep="")))


    ## calculate intervention durations
    print("Calculating intervention durations ...")
    gta_intervention_duration(data.path='master.sliced[,c("intervention.id", "date.implemented", "date.removed")]',
                              is.data.frame=TRUE,
                              years=c(year.start,year.end),
                              current.year.todate=current.year.todate)
    # rm(parameter.choice.duration)
    print("Calculating intervention durations ... complete.")

    ######## calculate implementer-importer-exporter-product tuples
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Data base replica source:", choice=paste("Local copy from '",replica.path,"'.", sep="")))

    print("Building intervention-importer-exporter-product tuples ...")
    gta_imp_exp_hs_tuples(master.path='master.sliced',
                          master.data.frame=TRUE)
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


    ## adding the filters for nr.also.affected.importer and jointly.affected.importers
    interventions.by.importer=unique(master.tuple[,c("intervention.id","i.un")])
    names(interventions.by.importer)=c("intervention.id", "importer.un")


    if(is.null(importers)){
      importer.count=aggregate(i.un ~ intervention.id,subset(master.tuple, i.un %in% importing.country), function(x) length(unique(x)))
      names(importer.count)=c("intervention.id", "nr.importers.hit")
      sole.importer=numeric()

    }else{
      importer.count=aggregate(i.un ~ intervention.id,subset(master.tuple, ! i.un %in% importing.country), function(x) length(unique(x)))
      names(importer.count)=c("intervention.id", "nr.importers.hit")

      sole.importer=setdiff(unique(subset(master.tuple, i.un %in% importing.country)$intervention.id),
                            unique(subset(master.tuple, ! i.un %in% importing.country)$intervention.id))

    }


    if(is.null(nr.also.importers)){
      importer.interventions=unique(interventions.by.importer$intervention.id)
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Nr. of also affected importers:", choice="Any"))
    }else{
      importer.interventions=c(sole.importer,unique(subset(importer.count, nr.importers.hit<=nr.also.importers)$intervention.id))
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Nr. of also affected importers:", choice=paste(nr.also.importers, " or less", sep="")))
    }



    if(is.null(jointly.affected.importers)){
      importer.interventions=importer.interventions
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Only include interventions where all specified importers are affected:", choice="No, just at least one of them."))
    }else{
      if(jointly.affected.importers){
        importer.interventions=intersect(importer.interventions, subset(interventions.by.importer, importer.un==importing.country[1])$intervention.id)

        if(length(importing.country)>1){
          for(k in 2:length(importing.country)){
            importer.interventions=intersect(importer.interventions, subset(interventions.by.importer, importer.un==importing.country[k])$intervention.id)
          }
        }

        if(length(importer.interventions)==0){
          stop.print <- "There are no interventions that jointly affect all specified importers."
          error.message <<- c(T, stop.print)
          stop(stop.print)}

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Only include interventions where all specified importers are affected:", choice="Yes."))

      }else{
        importer.interventions=importer.interventions
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Only include interventions where all specified importers are affected:", choice="No, just at least one of them."))
      }
    }



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
      mt=rbind(mt, subset(master.tuple, a.un==t.un  & t.un %in% implementing.country))
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


    ## create max duration for all instruments and per import-export-product year

    print("Identifying the maximum duration per year and importer-exporter-product tuple ... (this will take a while)")
    master.tuple$iahs=paste(master.tuple$i.un,master.tuple$a.un, master.tuple$affected.product, sep="-")



    duration.max=data.frame(iahs=character(), year=numeric(), share=numeric(), nr.of.hits=numeric())

    for(yr in c(year.start:year.end)){
      print(paste("Calculating maximum coverage per importer-exporter-product combination across all instruments in year ",yr,".",sep=""))
      int.iahs=unique(subset(master.tuple, intervention.id %in% subset(intervention.duration, year==yr & share>0)$intervention.id)$intervention.id)

      mt.temp=subset(master.tuple, intervention.id %in% int.iahs)
      int.temp=subset(intervention.duration, intervention.id %in% int.iahs)
      mt.temp=merge(mt.temp, subset(int.temp, year==yr & share>0)[,c("intervention.id","share")], by="intervention.id")

      v.iahs=unique(mt.temp$iahs)

      if(length(v.iahs)>0){
        nr.hits=as.data.frame(table(unique(mt.temp[,c("iahs","intervention.id")])$iahs))
        names(nr.hits)=c("iahs","nr.of.hits")
        mt.temp=unique(mt.temp[,c("iahs","share")])

        duration.temp=data.frame(iahs=character(),
                                 share=numeric())
        multiple.mention=as.character(subset(as.data.frame(table(mt.temp$iahs)), Freq>1)$Var1)

        if(length(multiple.mention)>0){
          duration.temp=subset(mt.temp, ! iahs %in% multiple.mention)

          multiple.interventions=subset(mt.temp, iahs %in% multiple.mention)

          if(length(unique(multiple.interventions$iahs[multiple.interventions$share==1]))>0){
            full.coverage=data.frame(iahs=unique(multiple.interventions$iahs[multiple.interventions$share==1]),
                                     share=1)
            duration.temp=rbind(duration.temp, full.coverage)
            multiple.interventions=subset(multiple.interventions, !iahs %in% full.coverage$iahs)

          }

          if(nrow(multiple.interventions)>0){

            tuple=multiple.interventions$iahs
            shrs =multiple.interventions$share

            dt=data.frame(iahs=unique(tuple), share=NA)
            output = character(nrow(dt))
            for(i in 1:nrow(dt)){
              output[i]=max(shrs[tuple==dt$iahs[i]])
            }
            dt$share=output
            rm(output)

            duration.temp=rbind(duration.temp, dt)
          }
        }else{
          duration.temp=mt.temp
        }

        duration.temp$year=yr

        duration.temp=merge(duration.temp, nr.hits, by="iahs", all.x=T)
        duration.max=rbind(duration.max,duration.temp)
        rm(duration.temp, nr.hits)
      }

      print(paste("Calculation of maximum coverage in year ",yr," is complete.",sep=""))

    }

    print(paste("Calculating maximum coverage per importer-exporter-product combination across all instruments complete.",sep=""))


    ## Add individual instervention.types, if called for.
    if(is.null(intervention.types)==F & group.type==F){
      duration.max$intervention.type="All included instruments"

      for(inst in unique(master.sliced$intervention.type)){
        for(yr in c(year.start:year.end)){
          int.iahs=unique(subset(master.tuple, intervention.id %in% subset(intervention.duration, year==yr & share>0)$intervention.id  &
                                   intervention.id %in% subset(master.sliced, intervention.type==inst)$intervention.id)$intervention.id)

          mt.temp=subset(master.tuple, intervention.id %in% int.iahs)
          int.temp=subset(intervention.duration, intervention.id %in% int.iahs)
          mt.temp=merge(mt.temp, subset(int.temp, year==yr & share>0)[,c("intervention.id","share")], by="intervention.id")

          v.iahs=unique(mt.temp$iahs)

          if(length(v.iahs)>0){
            nr.hits=as.data.frame(table(unique(mt.temp[,c("iahs","intervention.id")])$iahs))
            names(nr.hits)=c("iahs","nr.of.hits")
            mt.temp=unique(mt.temp[,c("iahs","share")])

            duration.temp=data.frame(iahs=character(),
                                     share=numeric())
            multiple.mention=as.character(subset(as.data.frame(table(mt.temp$iahs)), Freq>1)$Var1)

            if(length(multiple.mention)>0){
              duration.temp=subset(mt.temp, ! iahs %in% multiple.mention)

              multiple.interventions=subset(mt.temp, iahs %in% multiple.mention)

              if(length(unique(multiple.interventions$iahs[multiple.interventions$share==1]))>0){
                full.coverage=data.frame(iahs=unique(multiple.interventions$iahs[multiple.interventions$share==1]),
                                         share=1)
                duration.temp=rbind(duration.temp, full.coverage)

                multiple.interventions=subset(multiple.interventions, !iahs %in% full.coverage$iahs)
              }

              if(nrow(multiple.interventions)>0){

                tuple=multiple.interventions$iahs
                shrs =multiple.interventions$share

                dt=data.frame(iahs=unique(tuple), share=NA)
                output = character(nrow(dt))
                for(i in 1:nrow(dt)){
                  output[i]=max(shrs[tuple==dt$iahs[i]])
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
            duration.temp=merge(duration.temp, nr.hits, by="iahs", all.x=T)
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
    if(is.null(mast.chapters)==F & group.mast==F){
      duration.max$mast.chapter="All included instruments"

      for(inst in unique(master.sliced$mast.chapter)){
        for(yr in c(year.start:year.end)){
          int.iahs=unique(subset(master.tuple, intervention.id %in% subset(intervention.duration, year==yr & share>0)$intervention.id &
                                   intervention.id %in% subset(master.sliced, mast.chapter==inst)$intervention.id)$intervention.id)

          mt.temp=subset(master.tuple, intervention.id %in% int.iahs)
          int.temp=subset(intervention.duration, intervention.id %in% int.iahs)
          mt.temp=merge(mt.temp, subset(int.temp, year==yr & share>0)[,c("intervention.id","share")], by="intervention.id")

          v.iahs=unique(mt.temp$iahs)

          if(length(v.iahs)>0){
            nr.hits=as.data.frame(table(unique(mt.temp[,c("iahs","intervention.id")])$iahs))
            names(nr.hits)=c("iahs","nr.of.hits")
            mt.temp=unique(mt.temp[,c("iahs","share")])

            duration.temp=data.frame(iahs=character(),
                                     share=numeric())
            multiple.mention=as.character(subset(as.data.frame(table(mt.temp$iahs)), Freq>1)$Var1)

            if(length(multiple.mention)>0){
              duration.temp=subset(mt.temp, ! iahs %in% multiple.mention)

              multiple.interventions=subset(mt.temp, iahs %in% multiple.mention)

              if(length(unique(multiple.interventions$iahs[multiple.interventions$share==1]))>0){
                full.coverage=data.frame(iahs=unique(multiple.interventions$iahs[multiple.interventions$share==1]),
                                         share=1)
                duration.temp=rbind(duration.temp, full.coverage)

                multiple.interventions=subset(multiple.interventions, !iahs %in% full.coverage$iahs)
              }

              if(nrow(multiple.interventions)>0){

                tuple=multiple.interventions$iahs
                shrs =multiple.interventions$share

                dt=data.frame(iahs=unique(tuple), share=NA)
                output = character(nrow(dt))
                for(i in 1:nrow(dt)){
                  output[i]=max(shrs[tuple==dt$iahs[i]])
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
            duration.temp=merge(duration.temp, nr.hits,by="iahs", all.x=T)
            duration.max=rbind(duration.max,duration.temp)
            rm(nr.hits)
            }

          rm(duration.temp)
          print(paste("Calculating maximum coverage per importer-exporter-product combination for MAST chapter '", inst,"' in year ",yr,".",sep=""))

        }
      }

      print(paste("Calculating maximum coverage per importer-exporter-product combination for all MAST chapters individually complete.",sep=""))

    }
    print("Identifying the maximum duration per year and importer-exporter-product tuple ... complete.")

    # Check # of rows
    if(nrow(duration.max)==0) {
      stop.print <- "Unfortunately no rows remaining after filtering for mast.chapters"
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }

    ##### multiply in base values
    print("Importing trade base values ...")

    if(!trade.data %in% c("base","prior year","current year", "before announcement","during announcement", paste(2007:2017))){
      stop.print <- "Please specify proper trade data choice (i.e. 'base', a year between 2007 and 2017, 'prior year' or 'current year'."
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


    print("Merging base values into working data frame ...")
    duration.max$share=as.numeric(duration.max$share)

    if(trade.data %in% c("base", paste(2007:2017))){
      trade.base.bilateral$iahs=paste(trade.base.bilateral$i.un,trade.base.bilateral$a.un, trade.base.bilateral$hs6, sep="-")
      master.coverage=merge(duration.max, trade.base.bilateral, by="iahs", all.x=T)

    } else {
      trade.base.bilateral=subset(trade.base.bilateral, year %in% c(year.start:year.end))
      trade.base.bilateral$iahs=paste(trade.base.bilateral$i.un,trade.base.bilateral$a.un, trade.base.bilateral$hs6, sep="-")
      master.coverage=merge(duration.max, trade.base.bilateral, by=c("iahs","year"), all.x=T)
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


    if("mast.chapter" %in% names(master.coverage) & "intervention.type" %in% names(master.coverage)){

      mc.unique=data.frame(i.un=numeric(), a.un=numeric(), affected.product=numeric(), year=numeric(), trade.value.affected=numeric(), mast.chapter=character(), intervention.type=character(), nr.of.hits=numeric())

      mc.split <- split(master.coverage, sample(1:5, nrow(master.coverage), replace=T))

      for(y in 1:5){
        mc.t = mc.split[[y]]
        mc.t = unique(mc.t[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "mast.chapter", "intervention.type","nr.of.hits")])
        names(mc.t) = c("i.un", "a.un", "affected.product","year", "trade.value.affected", "mast.chapter","intervention.type","nr.of.hits")
        mc.unique = rbind(mc.unique, mc.t)
        rm(mc.t)
      }

      master.coverage = unique(mc.unique)

    }else{

      if("mast.chapter" %in% names(master.coverage)){

        mc.unique=data.frame(i.un=numeric(), a.un=numeric(), affected.product=numeric(), year=numeric(), trade.value.affected=numeric(), mast.chapter=character(), nr.of.hits=numeric())

        mc.split <- split(master.coverage, sample(1:5, nrow(master.coverage), replace=T))

        for(y in 1:5){
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

          mc.split <- split(master.coverage, sample(1:5, nrow(master.coverage), replace=T))

          for(y in 1:5){
            mc.t = mc.split[[y]]
            mc.t = unique(mc.t[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "intervention.type","nr.of.hits")])
            names(mc.t) = c("i.un", "a.un", "affected.product","year", "trade.value.affected","intervention.type","nr.of.hits")
            mc.unique = rbind(mc.unique, mc.t)
            rm(mc.t)
          }

          master.coverage = unique(mc.unique)


        } else {

          mc.unique=data.frame(i.un=numeric(), a.un=numeric(), affected.product=numeric(), year=numeric(), trade.value.affected=numeric(), nr.of.hits=numeric())


          mc.split <- split(master.coverage, sample(1:5, nrow(master.coverage), replace=T))

          for(y in 1:5){
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
    master.coverage=subset(master.coverage, is.na(trade.value.affected)==F)
    print("Merging base values into working data frame ... complete")

    # Check # of rows
    if(nrow(master.coverage)==0) {
      stop.print <- "Unfortunately no rows remaining after merging trade base values into working data frame"
      error.message <<- c(T, stop.print)
      stop(stop.print)
    }

    ### some housekeeping
    rm(duration.max, int.temp)
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

    for(brkt in 1:nrow(hit.frequency)){
      final.coverage=data.frame(i.un=numeric(), a.un=numeric(), year=numeric(), trade.value.affected=numeric())
      master.coverage=subset(base.coverage, nr.of.hits>=hit.frequency$min[brkt] & nr.of.hits<=hit.frequency$max[brkt])
      total.trade=sum(trade.base.bilateral$trade.value)
      tbb.yr = trade.base.bilateral

      for(yr in year.start:year.end){
        mc.yr=subset(master.coverage, year==yr)

        if ("year" %in% names(trade.base.bilateral)) {
          total.trade=sum(subset(trade.base.bilateral, year == yr)$trade.value)
          tbb.yr = subset(trade.base.bilateral, year == yr)
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




        } else{

          if(length(intersect(c("mast.chapter","intervention.type"),names(master.coverage)))==0){

            ## if nrow(mc.yr)>0
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
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, tbb.yr, sum), by="i.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$a.un=999
              final.coverage=rbind(final.coverage, fc.temp)
            }

            if(group.importers==T & group.exporters==F){
              fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.yr, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, tbb.yr, sum), by="a.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$i.un=999
              final.coverage=rbind(final.coverage, fc.temp)
            }

            if(group.importers==F & group.exporters==F){
              fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.yr, sum)


              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un","a.un"), all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              final.coverage=rbind(final.coverage, fc.temp)
            }




          } else {

            ## mast.chapters
            if("mast.chapter" %in% names(mc.yr)){
              ## MAST chapters
              mc.inst=subset(mc.yr, mast.chapter=="All included instruments")

              if(nrow(mc.inst)==0){

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




              } else{

                if(group.importers==T & group.exporters==T){
                  fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
                  if(share){fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade}
                  fc.temp$i.un=999
                  fc.temp$a.un=999
                  final.coverage=rbind(final.coverage, fc.temp)
                }

                if(group.importers==F & group.exporters==T){
                  fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)

                  if(share){
                    fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, tbb.yr, sum), by="i.un", all.x=T)
                    fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                    fc.temp$trade.value=NULL
                  }

                  fc.temp$a.un=999
                  final.coverage=rbind(final.coverage, fc.temp)
                }

                if(group.importers==T & group.exporters==F){
                  fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)


                  if(share){
                    fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, tbb.yr, sum), by="a.un", all.x=T)
                    fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                    fc.temp$trade.value=NULL
                  }

                  fc.temp$i.un=999
                  final.coverage=rbind(final.coverage, fc.temp)
                }

                if(group.importers==F & group.exporters==F){
                  fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)

                  if(share){
                    fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un","a.un"), all.x=T)
                    fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                    fc.temp$trade.value=NULL
                  }

                  final.coverage=rbind(final.coverage, fc.temp)
                }

              }
            } else {
              ## intervention types

              mc.inst=subset(mc.yr, intervention.type=="All included instruments")

              if(nrow(mc.inst)==0){

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




              } else{
                if(group.importers==T & group.exporters==T){
                  fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
                  if(share){fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade}
                  fc.temp$i.un=999
                  fc.temp$a.un=999
                  final.coverage=rbind(final.coverage, fc.temp)
                }

                if(group.importers==F & group.exporters==T){
                  fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)


                  if(share){
                    fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, tbb.yr, sum), by="i.un", all.x=T)
                    fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                    fc.temp$trade.value=NULL
                  }

                  fc.temp$a.un=999
                  final.coverage=rbind(final.coverage, fc.temp)
                }

                if(group.importers==T & group.exporters==F){
                  fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)


                  if(share){
                    fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, tbb.yr, sum), by="a.un", all.x=T)
                    fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                    fc.temp$trade.value=NULL
                  }

                  fc.temp$i.un=999
                  final.coverage=rbind(final.coverage, fc.temp)
                }

                if(group.importers==F & group.exporters==F){
                  fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)


                  if(share){
                    fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un","a.un"), all.x=T)
                    fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                    fc.temp$trade.value=NULL
                  }

                  final.coverage=rbind(final.coverage, fc.temp)
                }


              }

            }

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

      ### by intervention type, if necessary
      if(is.null(intervention.types)==F & group.type==F){
        final.coverage$intervention.type="All included instruments"
        print("Calculating aggregate annual trade coverage per included intervention type ...")

        for(inst in unique(master.sliced$intervention.type)){
          mc.inst=subset(master.coverage, intervention.type==inst)

          if(group.importers==T & group.exporters==T){

            if(nrow(mc.inst)==0){
              final.coverage=rbind(final.coverage,
                                   data.frame(year=unique(master.coverage$year),
                                              trade.value.affected=0,
                                              intervention.type=inst,
                                              i.un=999,
                                              a.un=999))


            } else {

              fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
              if(share){fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade}
              fc.temp$intervention.type=inst
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
              fc.temp$intervention.type=inst
              fc.temp$a.un=999

              final.coverage=rbind(final.coverage,
                                   fc.temp)


            } else {
              fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, tbb.yr, sum), by="i.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$intervention.type=inst
              fc.temp$a.un=999
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          if(group.importers==T & group.exporters==F){

            if(nrow(mc.inst)==0){
              fc.temp=expand.grid(unique(master.coverage$year), unique(master.coverage$a.un))
              names(fc.temp)=c("year","a.un")
              fc.temp$trade.value.affected=0
              fc.temp$intervention.type=inst
              fc.temp$i.un=999

              final.coverage=rbind(final.coverage,
                                   fc.temp)



            } else {
              fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, tbb.yr, sum), by="a.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$intervention.type=inst
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
              fc.temp$intervention.type=inst

              final.coverage=rbind(final.coverage,
                                   fc.temp)

            }  else{

              fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un","a.un"), all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$intervention.type=inst
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          print(paste("Calculated aggregate annual trade coverage for ", inst, sep=""))
        }
        print("Calculating aggregate annual trade coverage per included intervention type ... concluded")
      }

      # Check # of rows
      if(nrow(final.coverage)==0) {
        stop.print <- "Unfortunately no rows remaining after calculating aggregate annual trade coverage per intervention type"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }

      ### by MAST chapter, if necessary
      if(is.null(mast.chapters)==F & group.mast==F){
        final.coverage$mast.chapter="All included MAST chapters"
        print("Calculating aggregate annual trade coverage per included MAST chapter ...")

        for(inst in unique(master.sliced$mast.chapter)){
          mc.inst=subset(master.coverage, mast.chapter==inst)

          if(group.importers==T & group.exporters==T){

            if(nrow(mc.inst)==0){
              final.coverage=rbind(final.coverage,
                                   data.frame(year=unique(master.coverage$year),
                                              trade.value.affected=0,
                                              mast.chapter=inst,
                                              i.un=999,
                                              a.un=999))


            } else {

              fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
              if(share){
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade
              }
              fc.temp$mast.chapter=inst
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
              fc.temp$mast.chapter=inst
              fc.temp$a.un=999

              final.coverage=rbind(final.coverage,
                                   fc.temp)


            } else {
              fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, tbb.yr, sum), by="i.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$mast.chapter=inst
              fc.temp$a.un=999
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          if(group.importers==T & group.exporters==F){

            if(nrow(mc.inst)==0){
              fc.temp=expand.grid(unique(master.coverage$year), unique(master.coverage$a.un))
              names(fc.temp)=c("year","a.un")
              fc.temp$trade.value.affected=0
              fc.temp$mast.chapter=inst
              fc.temp$i.un=999

              final.coverage=rbind(final.coverage,
                                   fc.temp)



            } else {
              fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, tbb.yr, sum), by="a.un", all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$mast.chapter=inst
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
              fc.temp$mast.chapter=inst

              final.coverage=rbind(final.coverage,
                                   fc.temp)

            }  else{

              fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)

              if(share){
                fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, tbb.yr, sum), by=c("i.un","a.un"), all.x=T)
                fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
                fc.temp$trade.value=NULL
              }

              fc.temp$mast.chapter=inst
              final.coverage=rbind(final.coverage, fc.temp)
            }
          }

          print(paste("Calculated aggregate annual trade coverage for ", inst, sep=""))
        }
        print("Calculating aggregate annual trade coverage per included MAST chapter ... concluded")
      }

      # Check # of rows
      if(nrow(final.coverage)==0) {
        stop.print <- "Unfortunately no rows remaining after calculating aggregate annual trade coverage per included MAST chapter"
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
    countries=gtalibrary::country.names
    countries$name=as.character(countries$name)

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

    print("Oooh that's pretty ...")

    ## writing to disk
    if (xlsx==T) {
      print("Saving XLSX ...")
      if(is.null(output.path)){
        write.xlsx(trade.coverage.estimates, file=paste("GTA trade coverage estimates from ", Sys.Date(),".xlsx", sep=""), sheetName = "Estimates", row.names = F)
        write.xlsx(parameter.choices, file=paste("GTA trade coverage estimates from ", Sys.Date(),".xlsx", sep=""), sheetName = "Parameter choices", row.names = F, append=T)
        print("Saving XLSX ... completed in working directory")
      } else {
        write.xlsx(trade.coverage.estimates, file=output.path, sheetName = "Estimates", row.names = F)
        write.xlsx(parameter.choices, file=output.path, sheetName = "Parameter choices", row.names = F, append=T)
        print("Saving XLSX ... completed in output path")
      }
    }


  # bilateral.trade<<-trade.base.bilateral
  parameter.choices<<-parameter.choices
  error.message <<- FALSE

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
