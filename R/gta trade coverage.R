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
#' @param exporters Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for exporters in the sample. Default: All exporters.
#' @param keep.exporters Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated exporters.
#' @param group.exporters Specify whether to aggregate the statistics for all remaining exporters into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
#' @param implementers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for implementers in the sample. Default: World (as in implemented by one).
#' @param implementer.role Bilateral trade flows can be affected by multiple actors. Specify which actor's interventions you want to include. There are three roles: importer, exporter and 3rd country. Combinations are permissible. Default: c('importer','3rd country').
#' @param rdata Takes value TRUE or FALSE. If TRUE, Rdata file will be stored alongside xlsx. Default: FALSE
#' @param output.path Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.
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
#' @param cpc.sectors Provide a vector of CPC codes that you are interested in (version 2.1, any digit level).
#' @param keep.cpc Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
#' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
#' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
#' @param intervention.ids Provide a vector of intervention IDs.
#' @param keep.interventions Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated intervention IDs.
#' @param lag.adjustment Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
#'
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
  affected.flows = NULL,
  importers = NULL,
  keep.importers = NULL,
  group.importers = TRUE,
  exporters = NULL,
  keep.exporters = NULL,
  group.exporters = TRUE,
  implementers = NULL,
  implementer.role = NULL,
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
  intervention.ids = NULL,
  keep.interventions = NULL,
  lag.adjustment=NULL,
  rdata = FALSE,
  output.path = NULL) {


  # Initialising Function ---------------------------------------------------

  # load libraries
  library("xlsx")
  library("splitstackshape")
  library("data.table")

  ######## Feed data slicer

  ## Collecting parameter values
  parameter.choices=data.frame(parameter=character(), choice=character(),stringsAsFactors = F)


  print("Slicing GTA master data set ...")
  gta_data_slicer(data.path=data.path,
                  gta.evaluation= gta.evaluation,
                  affected.flows = affected.flows,
                  announcement.period = announcement.period,
                  implementation.period = implementation.period,
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

  ms=master.sliced[0,]

  if(nrow(subset(master.sliced, affected.flow %in% c("inward","outward subsidy")))>0){
    ms=rbind(ms, subset(master.sliced, affected.flow %in% c("inward","outward subsidy") & a.un %in% exporting.country))
  }

  if(nrow(subset(master.sliced, affected.flow=="outward"))>0){
    ms=rbind(ms, subset(master.sliced, affected.flow=="outward" & i.un %in% exporting.country))
  }
  master.sliced=ms
  master.sliced<<-master.sliced
  rm(ms)

  ##### Intervention durations
  ## relevant parameter: coverage.period
  if(is.null(coverage.period)){
    year.start=2009
    year.end=year(Sys.Date())
  } else {
    if(length(coverage.period)!=2){stop("Please supply a coverage period vector with two entries e.g. c(2009, 2018)")}
    if((min(coverage.period)<2008)|(max(coverage.period)>year(Sys.Date())) ){stop(paste("Please only supply coverage period years between 2008 and ", year(Sys.Date()), sep=""))}
    if(is.numeric(coverage.period)==F){stop("Please supply a coverage period vector with two integer entries e.g. c(2008, 2018)")}

    if(coverage.period[1]%%1==0){year.start=coverage.period[1]}else{stop("Please supply a coverage period vector with two integer entries e.g. c(2008, 2018)")}
    if(coverage.period[2]%%1==0){year.end=coverage.period[2]}else{stop("Please supply a coverage period vector with two integer entries e.g. c(2008, 2018)")}
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
    implementing.country=gta_un_code_vector(implementers, "implementing")
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Implementing countries:", choice=paste(implementers, collapse=", ")))
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
      stop(paste("Unknown implementer role(s): ", role.error, ".", sep=""))
    }
  }
  parameter.choices=rbind(parameter.choices, data.frame(parameter="Implementing country role(s):", choice=paste(implementer.role, collapse=", ")))


  # filter master.tuple
  print("Restricting set to stated importers/exporters ...")
  master.tuple=subset(master.tuple, i.un %in% importing.country)
  master.tuple<<-master.tuple
  print("Restricting set to stated importers/exporters ... complete.")

  print("Restricting set to stated implementers and their roles ...")
  mt=data.frame(intervention.id=numeric(),i.un=numeric(), a.un=numeric(), t.un=numeric(), affected.product=numeric())
  if("importer" %in% implementer.role){
    mt=rbind(mt, subset(master.tuple, i.un==t.un))
  }

  if("exporter" %in% implementer.role){
    mt=rbind(mt, subset(master.tuple, a.un==t.un))
  }

  if("3rd country" %in% implementer.role){
    mt=rbind(mt, subset(master.tuple, a.un!=t.un & i.un!=t.un))
  }
  master.tuple=mt
  master.tuple<<-master.tuple
  rm(mt)
  print("Restricting set to stated implementers and their roles ... complete.")


  ## create max duration for all instruments and per import-export-product year

  print("Identifying the maximum duration per year and importer-exporter-product tuple ... (this will take a while)")
  master.tuple$iahs=paste(master.tuple$i.un,master.tuple$a.un, master.tuple$affected.product, sep="-")

  duration.max=data.frame(iahs=character(), year=numeric(), share=numeric())

  for(yr in c(year.start:year.end)){
    print(paste("Calculating maximum coverage per importer-exporter-product combination across all instruments in year ",yr,".",sep=""))
    int.iahs=unique(subset(master.tuple, intervention.id %in% subset(intervention.duration, year==yr & share>0)$intervention.id)$intervention.id)

    mt.temp=subset(master.tuple, intervention.id %in% int.iahs)
    int.temp=subset(intervention.duration, intervention.id %in% int.iahs)
    mt.temp=merge(mt.temp, subset(int.temp, year==yr & share>0)[,c("intervention.id","share")], by="intervention.id")
    mt.temp=unique(mt.temp[,c("iahs","share")])
    v.iahs=unique(mt.temp$iahs)

    if(length(v.iahs)>0){
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

      duration.max=rbind(duration.max,duration.temp)
      rm(duration.temp)
    }

    print(paste("Calculation of maximum coverage in year ",yr," is complete.",sep=""))

  }

  # duration.max$mast.chapter="All included MAST chapters"
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
        mt.temp=unique(mt.temp[,c("iahs","share")])
        v.iahs=unique(mt.temp$iahs)

        if(length(v.iahs)>0){
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
          duration.max=rbind(duration.max,duration.temp)
        }

        rm(duration.temp)
        print(paste("Calculating maximum coverage per importer-exporter-product combination for '", inst,"' in year ",yr,".",sep=""))

      }
    }
    print(paste("Calculating maximum coverage per importer-exporter-product combination for all instruments individually complete.",sep=""))

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
        mt.temp=unique(mt.temp[,c("iahs","share")])
        v.iahs=unique(mt.temp$iahs)

        if(length(v.iahs)>0){
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
          duration.max=rbind(duration.max,duration.temp)
        }

        rm(duration.temp)
        print(paste("Calculating maximum coverage per importer-exporter-product combination for MAST chapter '", inst,"' in year ",yr,".",sep=""))

      }
    }
    print(paste("Calculating maximum coverage per importer-exporter-product combination for all MAST chapters individually complete.",sep=""))

  }
  print("Identifying the maximum duration per year and importer-exporter-product tuple ... complete.")

  ##### multiply in base values
  print("Importing trade base values ...")

  gta_trade_value_bilateral(importing.country = importing.country,
                            keep.importer = TRUE,
                            exporting.country = exporting.country,
                            keep.exporter = TRUE,
                            hs.codes = hs.codes,
                            keep.hs = TRUE)

  trade.base.bilateral$iahs=paste(trade.base.bilateral$i.un,trade.base.bilateral$a.un, trade.base.bilateral$hs6, sep="-")
  # rm(parameter.choice.trade.base)
  print("Importing trade base values ... completed.")


  print("Merging base values into working data frame ...")
  duration.max$share=as.numeric(duration.max$share)
  master.coverage=merge(duration.max, trade.base.bilateral, by="iahs", all.x=T)
  master.coverage$trade.value.affected=master.coverage$share* master.coverage$trade.value

  if("mast.chapter" %in% names(master.coverage) & "intervention.type" %in% names(master.coverage)){
    master.coverage=unique(master.coverage[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "mast.chapter", "intervention.type")])
    names(master.coverage)=c("i.un", "a.un", "affected.product","year", "trade.value.affected", "mast.chapter","intervention.type")

  }else{

    if("mast.chapter" %in% names(master.coverage)){

      master.coverage=unique(master.coverage[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "mast.chapter")])
      names(master.coverage)=c("i.un", "a.un", "affected.product","year", "trade.value.affected", "mast.chapter")

    } else {

      if("intervention.type" %in% names(master.coverage)){

        master.coverage=unique(master.coverage[,c("i.un", "a.un", "hs6","year", "trade.value.affected", "intervention.type")])
        names(master.coverage)=c("i.un", "a.un", "affected.product","year", "trade.value.affected", "intervention.type")

      } else {
        master.coverage=unique(master.coverage[,c("i.un", "a.un", "hs6","year", "trade.value.affected")])
        names(master.coverage)=c("i.un", "a.un", "affected.product","year", "trade.value.affected")
      }

    }

  }

  master.coverage=subset(master.coverage, is.na(trade.value.affected)==F)
  print("Merging base values into working data frame ... complete")

  ### some housekeeping
  rm(duration.max, int.temp)
  if(exists("full.coverage")){rm(full.coverage)}

  #### aggregating to trade coverage table
  print("Calculating aggregate annual trade coverage ...")

  final.coverage=data.frame(i.un=numeric(), a.un=numeric(), year=numeric(), trade.value.affected=numeric())


  if(("mast.chapter" %in% names(master.coverage) | "intervention.type" %in% names(master.coverage))==F){

    total.trade=sum(trade.base.bilateral$trade.value)

    for(yr in year.start:year.end){

      # splitting the set to avoid memory overload
      mc.yr=subset(master.coverage, year==yr)

      if(group.importers==T & group.exporters==T){
        fc.temp=aggregate(trade.value.affected ~ year, mc.yr, sum)
        fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade
        fc.temp$i.un=999
        fc.temp$a.un=999
        final.coverage=rbind(final.coverage, fc.temp)
      }

      if(group.importers==F & group.exporters==T){
        fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.yr, sum)
        fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, trade.base.bilateral, sum), by="i.un", all.x=T)
        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
        fc.temp$trade.value=NULL
        fc.temp$a.un=999
        final.coverage=rbind(final.coverage, fc.temp)
      }

      if(group.importers==T & group.exporters==F){
        fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.yr, sum)
        fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, trade.base.bilateral, sum), by="a.un", all.x=T)
        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
        fc.temp$trade.value=NULL
        fc.temp$i.un=999
        final.coverage=rbind(final.coverage, fc.temp)
      }

      if(group.importers==F & group.exporters==F){
        fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.yr, sum)
        fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, trade.base.bilateral, sum), by=c("i.un","a.un"), all.x=T)
        fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
        fc.temp$trade.value=NULL
        final.coverage=rbind(final.coverage, fc.temp)
      }
    }

  } else {


    # splitting the set to avoid memory overload
    for(yr in year.start:year.end){

      mc.yr=subset(master.coverage, year==yr)

      if("mast.chapter" %in% names(mc.yr)){

        mc.inst=subset(mc.yr, mast.chapter=="All included instruments")
        if(group.importers==T & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade
          fc.temp$i.un=999
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, trade.base.bilateral, sum), by="i.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==T & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, trade.base.bilateral, sum), by="a.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$i.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, trade.base.bilateral, sum), by=c("i.un","a.un"), all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          final.coverage=rbind(final.coverage, fc.temp)
        }

      } else {

        mc.inst=subset(mc.yr, intervention.type=="All included instruments")
        if(group.importers==T & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/total.trade
          fc.temp$i.un=999
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, trade.base.bilateral, sum), by="i.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==T & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, trade.base.bilateral, sum), by="a.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$i.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, trade.base.bilateral, sum), by=c("i.un","a.un"), all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          final.coverage=rbind(final.coverage, fc.temp)
        }

      }
    }
  }
  print("Calculating aggregate annual trade coverage ... completed")

  ### by intervention type, if necessary
  if(is.null(intervention.types)==F & group.type==F){
    final.coverage$intervention.type="All included instruments"
    print("Calculating aggregate annual trade coverage per included intervention type ...")

    for(inst in unique(master.sliced$intervention.type)){
      mc.inst=subset(master.coverage, intervention.type==inst)

      if(nrow(mc.inst)==0){

        print(paste("Calculated aggregate annual trade coverage for ", inst, sep=""))

      } else {
        if(group.importers==T & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/sum(trade.base.bilateral$trade.value)
          fc.temp$intervention.type=inst
          fc.temp$i.un=999
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, trade.base.bilateral, sum), by="i.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$intervention.type=inst
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==T & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, trade.base.bilateral, sum), by="a.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$intervention.type=inst
          fc.temp$i.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, trade.base.bilateral, sum), by=c("i.un","a.un"), all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$intervention.type=inst
          final.coverage=rbind(final.coverage, fc.temp)
        }
      }

      print(paste("Calculated aggregate annual trade coverage for ", inst, sep=""))
    }
    print("Calculating aggregate annual trade coverage per included intervention type ... concluded")
  }


  ### by MAST chapter, if necessary
  if(is.null(mast.chapters)==F & group.mast==F){
    final.coverage$mast.chapter="All included MAST chapters"
    print("Calculating aggregate annual trade coverage per included MAST chapter ...")

    for(inst in unique(master.sliced$mast.chapter)){
      mc.inst=subset(master.coverage, mast.chapter==inst)

      if(nrow(mc.inst)==0){

        print(paste("Calculated aggregate annual trade coverage for ", inst, sep=""))

      } else {
        if(group.importers==T & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ year, mc.inst, sum)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/sum(trade.base.bilateral$trade.value)
          fc.temp$mast.chapter=inst
          fc.temp$i.un=999
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==T){
          fc.temp=aggregate(trade.value.affected ~ i.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un, trade.base.bilateral, sum), by="i.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$mast.chapter=inst
          fc.temp$a.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==T & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ a.un, trade.base.bilateral, sum), by="a.un", all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$mast.chapter=inst
          fc.temp$i.un=999
          final.coverage=rbind(final.coverage, fc.temp)
        }

        if(group.importers==F & group.exporters==F){
          fc.temp=aggregate(trade.value.affected ~ i.un + a.un + year, mc.inst, sum)
          fc.temp=merge(fc.temp, aggregate(trade.value ~ i.un + a.un, trade.base.bilateral, sum), by=c("i.un","a.un"), all.x=T)
          fc.temp$trade.value.affected=fc.temp$trade.value.affected/fc.temp$trade.value
          fc.temp$trade.value=NULL
          fc.temp$mast.chapter=inst
          final.coverage=rbind(final.coverage, fc.temp)
        }
      }

      print(paste("Calculated aggregate annual trade coverage for ", inst, sep=""))
    }
    print("Calculating aggregate annual trade coverage per included MAST chapter ... concluded")
  }


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

    final.coverage=reshape(final.coverage, idvar=c("importer","exporter"), timevar = "year", direction="wide")
    # pretty column names
    setnames(final.coverage, "importer", "Importing country")
    setnames(final.coverage, "exporter", "Exporting country")
    column.order=c("Importing country", "Exporting country")
    for(yr in year.start:year.end){
      names(final.coverage)[grepl(yr, names(final.coverage))==T]=paste("Trade coverage estimate for ",yr, sep="")
      column.order=c(column.order, paste("Trade coverage estimate for ",yr, sep=""))
    }

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

    final.coverage=reshape(final.coverage, idvar=c("importer","exporter","mast.chapter", "mast.chapter.name"), timevar = "year", direction="wide")


    # pretty column names
    setnames(final.coverage, "importer", "Importing country")
    setnames(final.coverage, "exporter", "Exporting country")
    setnames(final.coverage, "mast.chapter", "MAST chapter ID")
    setnames(final.coverage, "mast.chapter.name", "MAST chapter name")

    column.order=c("Importing country", "Exporting country","MAST chapter ID", "MAST chapter name")
    for(yr in year.start:year.end){
      names(final.coverage)[grepl(yr, names(final.coverage))==T]=paste("Trade coverage estimate for ",yr, sep="")
      column.order=c(column.order, paste("Trade coverage estimate for ",yr, sep=""))
    }

    trade.coverage.estimates<<-final.coverage[,column.order]
  }


  if("intervention.type" %in% names(final.coverage)){

    final.coverage=reshape(final.coverage, idvar=c("importer","exporter","intervention.type"), timevar = "year", direction="wide")


    # pretty column names
    setnames(final.coverage, "importer", "Importing country")
    setnames(final.coverage, "exporter", "Exporting country")
    setnames(final.coverage, "intervention.type", "Intervention type")

    column.order=c("Importing country", "Exporting country","Intervention type")
    for(yr in year.start:year.end){
      names(final.coverage)[grepl(yr, names(final.coverage))==T]=paste("Trade coverage estimate for ",yr, sep="")
      column.order=c(column.order, paste("Trade coverage estimate for ",yr, sep=""))
    }

    trade.coverage.estimates<<-final.coverage[,column.order]
  }

  print("Oooh that's pretty ...")

  ## writing to disk
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






  bilateral.trade<<-trade.base.bilateral
  parameter.choices<<-parameter.choices



}
