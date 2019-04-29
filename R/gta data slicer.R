# Roxygen documentation

#' This function allows you to extract a tailored subset of the GTA database.
#'
#' Make use of the many variables captured in the taxonomy of the Global Trade Alert and tailor the data to your needs. This function returns a list of 2 data frames. The first, master, contains the subset of GTA data you requested. The second, parameter.choices, lists all implicit or exlicited choices that led to the subset.
#'
#' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param gta.evaluation Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
#' @param affected.flows Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is 'any'. Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
#' @param implementing.country Specify the implementing countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.implementer Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementing country.
#' @param affected.country Specify the affected countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.affected Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated affected country.
#' @param incl.affected.strictness Specify whether to include interventions that affect only one of the selected affected jurisdictions ('ONE'), at least one of the selected affected jurisdictions ('ONEPLUS') or all of the selected affected jurisdictions ('ALL'). Default is 'ONEPLUS'.
#' @param keep.others Specify whether to keep the data for the other jurisdictions that happen to be affected alongside those you specified (T/F). Default is 'TRUE'.
#' @param nr.affected Specify the range for the number of importers affected by an intervention. Default is any number i.e. c(0,999).
#' @param nr.affected.incl Specify whether in the number of importers affected by an intervention is calculated based only on the selected importers are included ('SELECTED'), only on the unselected importers ('UNSELECTED') or based on both ('ALL'). Default is 'ALL'.
#' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
#' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
#' @param keep.implementation.na Specify whether to keep ('TRUE') or remove ('FALSE') interventions with missing implementation.date.
#' @param revocation.period Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param submission.period Specify a period in which the interventions for your analysis have been submitted. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param keep.revocation.na Specify whether to keep ('TRUE') or remove ('FALSE') interventions with missing revocation.date.
#' @param in.force.today Specify whether you want to focus on interventions in force today ('Yes') or no longer in force today ('No'). Default is 'any' i.e. regardless of current enforcement status.
#' @param intervention.types Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
#' @param keep.type Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
#' @param mast.chapters Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
#' @param keep.mast Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
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
#' @param reporting.period Specify the period in which an intervention was documented by the GTA team. Default c('2008-11-01',today).
#' @param df.name Set the name of the generated result data frame. Default is master.sliced.
#' @param pc.name Set the name of the generated parameter choice data frame. Default is parameter.choice.slicer.
#' @param xlsx Takes value TRUE or FALSE. If TRUE, xlsx file will be stored. Default: FALSE
#' @param output.path Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.

#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_data_slicer=function(data.path="data/master_plus.Rdata",
                         gta.evaluation= NULL,
                         affected.flows = NULL,
                         implementing.country = NULL,
                         keep.implementer = TRUE,
                         affected.country = NULL,
                         keep.affected = NULL,
                         incl.affected.strictness="ONEPLUS",
                         keep.others=TRUE,
                         nr.affected=c(0,999),
                         nr.affected.incl="ALL",
                         announcement.period = NULL,
                         implementation.period = NULL,
                         keep.implementation.na = NULL,
                         revocation.period = NULL,
                         keep.revocation.na = NULL,
                         submission.period = NULL,
                         in.force.today = NULL,
                         intervention.types = NULL,
                         keep.type = NULL,
                         mast.chapters = NULL,
                         keep.mast = NULL,
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
                         reporting.period=NULL,
                         df.name="master.sliced",
                         pc.name="parameter.choice.slicer",
                         xlsx = FALSE,
                         output.path = NULL
){

  library("httr")
  library("splitstackshape")
  library("lubridate")
  library("data.table")

  ## Collecting parameter values
  parameter.choices=data.frame(parameter=character(), choice=character(),stringsAsFactors = F)

  ## data path
  if(data.path=="online"){
    print("Downloading the latest copy of the GTA dataset.The file is deleted after loading the data into your environment.")
    download.file("https://www.dropbox.com/s/78kpe232p2b36ze/GTA%20full%20data%20export.Rdata?dl=1","GTA data.Rdata")
    load("GTA data.Rdata")
    unlink("GTA data.Rdata")
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Data source:", choice="Downloaded latest copy"))
  } else{
    load(data.path)
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Data source:", choice=paste("Local copy from '",data.path,"'.", sep="")))
  }

  tryCatch({
      # gta.evaluation
      if(is.null(gta.evaluation)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="GTA evaluations included:", choice="red, amber, green"))

      } else {
        check=gta_parameter_check(tolower(gta.evaluation), c("red", "amber", "green"))
        if(check!="OK"){
          stop.print <- paste("Unknown GTA evaluation(s): ", check, ".", sep="")
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else {
          eval=tolower(gta.evaluation)
          master=subset(master, tolower(gta.evaluation) %in% eval)

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="GTA evaluations included:", choice=paste(eval, collapse=", ")))

        }

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for gta.evaluation"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }


      # affected.flows

      if(is.null(affected.flows)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Affected flows included:", choice="inward, outward, outward subsidy"))

      } else {
        check=gta_parameter_check(tolower(affected.flows), c("inward", "outward", "outward subsidy"))
        if(check!="OK"){
          stop.print <- paste("Unknown GTA evaluation(s): ", check, ".", sep="")
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else {
          flow=tolower(affected.flows)
          master=subset(master, tolower(affected.flow) %in% flow)

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Affected flows included:", choice=paste(flow, collapse=", ")))

        }

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for affected.flow"
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }


      # implementing.country
      # keep.implementer
      if(is.null(implementing.country)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Implementing countries included:", choice="All"))

      } else {

        if(is.null(keep.implementer)){
          stop.print <- "Please specify whether you want to focus on the specified implementing countries or exclude them (keep.implementer=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          implementers=gta_un_code_vector(implementing.country, role="implementing")

          if(keep.implementer==T){
            master=subset(master, i.un %in% implementers)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Implementing countries included:", choice=paste(implementing.country, collapse = ", ")))

          } else {
            master=subset(master, ! i.un %in% implementers)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Implementing countries included:", choice=paste("All except ", paste(implementing.country, collapse = ", "), sep="")))

          }
        }
      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for implementing.country"
        error.message <<- c(T, stop.print)
        stop(stop.print)
    }


      # affected.country
      # keep.affected
      # additional targets
      # due to the interdependece of these filters, we will only generate vectors of intervention IDs before subetting the result at the very end of this section.

      if(is.null(affected.country)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Affected countries included:", choice="All"))
        affected.interventions=master$intervention.id

      } else {

        if(is.null(keep.affected)){
          stop.print <- "Please specify whether you want to focus on the specified affected countries or exclude them (keep.affected=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else {

          affected=gta_un_code_vector(affected.country, role="affected")

          if(keep.affected==T){
            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Affected countries included:", choice=paste(affected.country, collapse = ", ")))

          } else {
            affected=setdiff(gtalibrary::country.correspondence$un_code, affected)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Affected countries included:", choice=paste("All except ", paste(affected.country, collapse = ", "), sep="")))

          }

          affected.interventions=subset(master, a.un %in% affected)$intervention.id


        }
      }

      # Check # of rows
      if(length(affected.interventions)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for affected.country"
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }


    ## restrict data to the combination of selected affected jurisdictions
    if(! incl.affected.strictness %in% c("ALL","ONE","ONEPLUS")){

      stop.print <- "Please choose how to include the chosen affected jurisdictions (ONE/ALL/ONEPLUS)."
      error.message <<- c(T, stop.print)
      stop(stop.print)

    } else {

      ## this is the 'ONEPLUS' case.
      affected.combinations=unique(master$intervention.id)

      if(incl.affected.strictness=="ALL"){

        for(cty in affected){

          a.c.temp=unique(subset(master, a.un==cty)$intervention.id)

          affected.combinations=intersect(affected.combinations,
                                          a.c.temp)
          rm(a.c.temp)
        }

      }


      if(incl.affected.strictness=="ONE"){

        one.aj=subset(master, a.un %in% affected)

        one.aj=aggregate(a.un ~intervention.id, one.aj, function(x) length(unique(x)))

        affected.combinations=subset(one.aj, a.un==1)$intervention.id

        rm(one.aj)

      }


      if(length(affected.combinations)==0){

        stop.print <- "No rows left for the selected affected jurisdiction combintion (parameter incl.affected.strictness)."
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }

    }
    affected.interventions=intersect(affected.interventions, affected.combinations)

  ### applying the restrictions on the number of affected trading partners per intervention

    ## min/max nr of affected jurisdictions
    nr.affected.min=nr.affected[1]
    nr.affected.max=nr.affected[2]
    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Nr. of affected jurisdictions: ", choice=paste(nr.affected.min, nr.affected.max, sep=" - ")))


    ## Calculation form
    if(! nr.affected.incl %in% c("ALL","SELECTED","UNSELECTED")){

      stop.print <- "Please choose which imports to include into the calculation for the number of affected jurisdictions (ALL/SELECTED/UNSELECTED)."
      error.message <<- c(T, stop.print)
      stop(stop.print)

    }else {

      # ensuring zero-AJ interventions are counted
      master$a.un[is.na(master$a.un)]=1234

      if(nr.affected.incl=="ALL"){

        imp.count=aggregate(a.un ~ intervention.id, master ,function(x) length(unique(x)))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected jurisdictions calculated based on: ", choice="All affected jurisdictions"))


      }

      if(nr.affected.incl=="SELECTED"){

        imp.count=aggregate(a.un ~ intervention.id,subset(master, a.un %in% affected),function(x) length(unique(x)))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected jurisdictions calculated based on: ", choice="Selected affected jurisdictions"))

      }

      if(nr.affected.incl=="UNSELECTED"){

        imp.count=aggregate(a.un ~ intervention.id,subset(master, ! a.un %in% affected),function(x) length(unique(x)))


        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Nr. of affected jurisdictions calculated based on: ", choice="Unselected affected jurisdictions"))

      }


      affected.interventions=intersect(affected.interventions,
                                       subset(imp.count, a.un>=nr.affected.min &
                                                a.un<=nr.affected.max)$intervention.id)

      master$a.un[master$a.un==1234]=NA

    }



    if(length(affected.interventions)==0){
      stop.print <- "There are no interventions that satisfy your choices for nr.affected, nr.affected.incl and incl.affected.strictness simultaneously."
      error.message <<- c(T, stop.print)
      stop(stop.print)}


    ## applying all these conditions
      master=subset(master, intervention.id %in% affected.interventions)


    ## keep.others
      if(keep.others==FALSE & is.null(affected.country) == F){
          master=subset(master, a.un %in% affected)
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Keep data for other jurisdictions affected alongside those specified:",
                                           choice="No"))


      } else {
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Keep data for other jurisdictions affected alongside those specified:",
                                           choice="Yes"))
      }


      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for affected.also.nr and keep.others"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }


      # announcement.period
      date.period=announcement.period

      if(is.null(date.period)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Announcement period:", choice="Full GTA monitoring period"))

      } else {

        if(length(date.period)!=2){
          stop.print <- "Please specify the date pair (after.date, before.date) for the announcement period. 'NA' is permissible, but has to be specified in case you only want one of the two."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{
          dates=sum(as.numeric(is.na(date.period))==F)

          if(dates>0){
            if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
              error.message <<- c(T, "At least one of the announcement dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
              stop("At least one of the announcement dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
            }

            if(dates==2){
              master=subset(master, date.announced>=as.Date(date.period[1], "%Y-%m-%d") & date.announced<=as.Date(date.period[2], "%Y-%m-%d"))
              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Announcement period:", choice=paste(date.period[1]," - ",date.period[2], sep="")))

            }

            if(dates==1){

              if(is.na(as.Date(date.period[1], "%Y-%m-%d"))==F){
                master=subset(master, date.announced>=as.Date(date.period[1], "%Y-%m-%d"))
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Announcement period:", choice=paste(date.period[1]," or more recent", sep="")))
              }

              if(is.na(as.Date(date.period[2], "%Y-%m-%d"))==F){
                master=subset(master, date.announced<=as.Date(date.period[2], "%Y-%m-%d"))
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Announcement period:", choice=paste(date.period[2]," or earlier", sep="")))
              }

            }

          } else {
            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Announcement period:", choice="Full GTA monitoring period"))
          }


        }

        remove(date.period)

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for announcement.period"
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }


      # implementation.period
      date.period=implementation.period

      if(is.null(date.period)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Implementation period:", choice="Full GTA monitoring period"))

      } else {


        if(length(date.period)!=2){
          stop.print <- "Please specify the date pair (after.date, before.date) for the implementation period. 'NA' is permissible, but has to be specified in case you only want one of the two."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          dates=sum(as.numeric(is.na(date.period))==F)

          if(dates>0){
            if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
              stop("At least one of the implementation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
            }

            if(is.null(keep.implementation.na)){
              stop.print <- "Please specify whether you want to keep interventions with missing implementation date or exclude them (keep.implementation.na=T/F)."
              error.message <<- c(T, stop.print)
              stop(stop.print)
            }

            if(dates==2){

              if(keep.implementation.na==T) {
                master=subset(master, (date.implemented>=as.Date(date.period[1], "%Y-%m-%d") & date.implemented<=as.Date(date.period[2], "%Y-%m-%d")) | is.na(date.implemented)==T )
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="implementation period:", choice=paste(date.period[1]," - ",date.period[2],", as well as interventions without implementation date", sep="")))
              }

              if(keep.implementation.na==F) {
                master=subset(master, date.implemented>=as.Date(date.period[1], "%Y-%m-%d") & date.implemented<=as.Date(date.period[2], "%Y-%m-%d"))
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="implementation period:", choice=paste(date.period[1]," - ",date.period[2]," and excluding interventions without implementation date", sep="")))

              }

            }

            if(dates==1){

              if(is.na(as.Date(date.period[1], "%Y-%m-%d"))==F){
                if(keep.implementation.na==T) {master=subset(master, date.implemented>=as.Date(date.period[1], "%Y-%m-%d") | is.na(date.implemented)==T)}
                if(keep.implementation.na==F) {master=subset(master, date.implemented>=as.Date(date.period[1], "%Y-%m-%d"))}
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Implementation period:", choice=paste(date.period[1]," or more recent", sep="")))
              }

              if(is.na(as.Date(date.period[2], "%Y-%m-%d"))==F){
                if(keep.implementation.na==T){master=subset(master, date.implemented<=as.Date(date.period[2], "%Y-%m-%d") | is.na(date.implemented)==T)}
                if(keep.implementation.na==F){master=subset(master, date.implemented<=as.Date(date.period[2], "%Y-%m-%d"))}
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Implementation period:", choice=paste(date.period[2]," or earlier", sep="")))
              }

            }

          } else {
            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Implementation period:", choice="Full GTA monitoring period"))
          }


        }

        remove(date.period)

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for implementation.period"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }


      # revocation.period
      date.period=revocation.period

      if(is.null(date.period)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Revocation period:", choice="None specified"))

      } else {

        if(length(date.period)!=2){
          stop.print <- "Please specify the date pair (after.date, before.date) for the revocation period. 'NA' is permissible, but has to be specified in case you only want one of the two."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{
          dates=sum(as.numeric(is.na(date.period))==F)

          if(dates>0){
            if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
              error.message <<- c(T, "At least one of the revocation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
              stop("At least one of the revocation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
            }

            if(is.null(keep.revocation.na)){
              stop.print <- "Please specify whether you want to keep interventions with missing revocation date or exclude them (keep.revocation.na=T/F)."
              error.message <<- c(T, stop.print)
              stop(stop.print)
            }

            if(dates==2){

              if(keep.revocation.na==T) {
                master=subset(master, (date.removed>=as.Date(date.period[1], "%Y-%m-%d") & date.removed<=as.Date(date.period[2], "%Y-%m-%d")) | is.na(date.removed)==T )
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Revocation period:", choice=paste(date.period[1]," - ",date.period[2],", as well as interventions without removal date", sep="")))
              }

              if(keep.revocation.na==F) {
                master=subset(master, date.removed>=as.Date(date.period[1], "%Y-%m-%d") & date.removed<=as.Date(date.period[2], "%Y-%m-%d"))
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Revocation period:", choice=paste(date.period[1]," - ",date.period[2]," and excluding interventions without removal date", sep="")))

              }

            }

            if(dates==1){



              if(is.na(as.Date(date.period[1], "%Y-%m-%d"))==F){

                if(keep.revocation.na==T) {master=subset(master, date.removed>=as.Date(date.period[1], "%Y-%m-%d") | is.na(date.removed)==T)}
                if(keep.revocation.na==F) {master=subset(master, date.removed>=as.Date(date.period[1], "%Y-%m-%d"))}
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Revocation period:", choice=paste(date.period[1]," or more recent", sep="")))
              }

              if(is.na(as.Date(date.period[2], "%Y-%m-%d"))==F){

                if(keep.revocation.na==T){master=subset(master, date.removed<=as.Date(date.period[2], "%Y-%m-%d") | is.na(date.removed)==T)}
                if(keep.revocation.na==F){master=subset(master, date.removed<=as.Date(date.period[2], "%Y-%m-%d"))}
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Revocation period:", choice=paste(date.period[2]," or earlier", sep="")))
              }

            }

          } else {
            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Revocation period:", choice="None specified"))
          }


        }

        remove(date.period)

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for revocation.period"
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }


      # submission.period
      date.period=submission.period

      if(is.null(date.period)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Submission period:", choice="Full GTA monitoring period"))

      } else {

        if(length(date.period)!=2){
          stop.print <- "Please specify the date pair (after.date, before.date) for the submission period. 'NA' is permissible, but has to be specified in case you only want one of the two."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{
          dates=sum(as.numeric(is.na(date.period))==F)

          if(dates>0){
            if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
              stop.print <- "At least one of the submission dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'."
              error.message <<- c(T, stop.print)
              stop(stop.print)
            }

            if(dates==2){
              master=subset(master, date.published>=as.Date(date.period[1], "%Y-%m-%d") & date.published<=as.Date(date.period[2], "%Y-%m-%d"))
              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Submission period:", choice=paste(date.period[1]," - ",date.period[2], sep="")))

            }

            if(dates==1){

              if(is.na(as.Date(date.period[1], "%Y-%m-%d"))==F){
                master=subset(master, date.published>=as.Date(date.period[1], "%Y-%m-%d"))
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Submission period:", choice=paste(date.period[1]," or more recent", sep="")))
              }

              if(is.na(as.Date(date.period[2], "%Y-%m-%d"))==F){
                master=subset(master, date.published<=as.Date(date.period[2], "%Y-%m-%d"))
                parameter.choices=rbind(parameter.choices,
                                        data.frame(parameter="Submission period:", choice=paste(date.period[2]," or earlier", sep="")))
              }

            }

          } else {
            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Submission period:", choice="Full GTA monitoring period"))
          }


        }

        remove(date.period)

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for submission.period"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }


      # in.force.today
      if(is.null(in.force.today)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Currently in force:", choice="Regardless"))
      } else {

      if(is.null(in.force.today)==F){

        if(tolower(in.force.today)=="any"){

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Currently in force:", choice="Regardless"))
        }

        if (tolower(in.force.today)=="yes"){

          master=subset(master, date.implemented<=Sys.Date() & (is.na(date.removed)==T|date.removed>=Sys.Date()))

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Currently in force:", choice="Yes"))

        }

        if(tolower(in.force.today)=='no'){

          master=subset(master, (date.implemented<Sys.Date() & is.na(date.removed)==F & date.removed<Sys.Date()) | is.na(date.implemented))

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Currently in force:", choice="No"))

        }

      } else {
        stop.print <- "Please specify in.force.today as either 'yes', 'no' or 'any'."
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }

    }


      # Check # of rows
      if(nrow(master)==0) {
        error.message <<- c(T, "Unfortunately no rows remaining after filtering for in.force.today")
        stop("Unfortunately no rows remaining after filtering for in.force.today")
      }

      # intervention.type
      # keep.type
      if(is.null(intervention.types)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Intervention types included:", choice="All"))

      } else {

        if(is.null(keep.type)){
          stop.print <- "Please specify whether you want to focus on the specified intervention types or exclude them (keep.type=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)
        } else{

          int.mast.types <- gtalibrary::int.mast.types
          check=gta_parameter_check(tolower(intervention.types), tolower(int.mast.types$intervention.type))

          if(check!="OK"){
            stop(paste("Unknown intervention type(s): ", check, ".", sep=""))

          } else {
            if(keep.type==T){
              master=subset(master, tolower(intervention.type) %in% tolower(intervention.types))

              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Intervention types included:", choice=paste(intervention.types, collapse = ", ")))

            } else {
              master=subset(master, ! tolower(intervention.type) %in% tolower(intervention.types))

              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Intervention types included:", choice=paste("All except ", paste(intervention.types, collapse = ", "), sep="")))

            }

          }


        }
      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for intervention.types"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }


      parameter.choices$parameter=as.character(parameter.choices$parameter)
      parameter.choices$choice=as.character(parameter.choices$choice)

      # mast.chapter
      # keep.mast
      if(is.null(mast.chapters)){

        if(is.null(intervention.types)){

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Mast chapters included:", choice="All"))
        } else {

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Mast chapters included:", choice="Those implied by intervention type choice."))

        }



      } else {

        if(is.null(keep.mast)){
          stop.print <- "Please specify whether you want to focus on the specified mast chapters or exclude them (keep.mast=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          #Remove integers from string
          mast.letter <- gsub("[0-9]+","", mast.chapters)
          mast.letter <- mast.letter[mast.letter != ""]

          int.mast.types <- gtalibrary::int.mast.types
          check=gta_parameter_check(tolower(mast.letter), tolower(int.mast.types$mast.chapter.id))

          if(check!="OK"){
            stop.print <- paste("Unknown mast chapter(s): ", check, ".", sep="")
            error.message <<- c(T, stop.print)
            stop(stop.print)

          } else {

            if(keep.mast==T){
              master=subset(master, tolower(mast.chapter) %in% tolower(mast.letter))


              parameter.choices$choice[parameter.choices$parameter=="Intervention types included:" & parameter.choices$choice=="All"]="As implied by MAST choice"
              parameter.choices$choice[parameter.choices$parameter=="Intervention types included:" & parameter.choices$choice!="All"]=paste(parameter.choices$choice[parameter.choices$parameter=="Intervention types included:" & parameter.choices$choice!="All"], " and those implied by MAST choice.", sep="")
              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Mast chapters included:", choice=paste(mast.letter, collapse = ", ")))

            } else {
              master=subset(master, ! tolower(mast.chapter) %in% tolower(mast.letter))

              parameter.choices$choice[parameter.choices$parameter=="Intervention types included:" & parameter.choices$choice=="All"]="As implied by MAST choice"
              parameter.choices$choice[parameter.choices$parameter=="Intervention types included:" & parameter.choices$choice!="All"]=paste(parameter.choices$choice[parameter.choices$parameter=="Intervention types included:" & parameter.choices$choice!="All"], " and those implied by MAST choice.", sep="")
              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Mast chapters included:", choice=paste("All except ", paste(mast.letter, collapse = ", "), sep="")))

            }

          }


        }
      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for mast.chapters"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }



      # implementation.level
      # keep.level
      if(is.null(implementation.level)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Implementation levels included:", choice="All"))

      } else {

        if(is.null(keep.level)){
          stop.print <- "Please specify whether you want to focus on the specified implementation levels or exclude them (keep.level=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          imp.levels <- gtalibrary::imp.levels

          check=gta_parameter_check(tolower(implementation.level), tolower(imp.levels$implementation.level))

          if(check!="OK"){
            stop(paste("Unknown implementation level(s): ", check, ".", sep=""))

          } else {

            implementation.level.choice <- implementation.level

            if(keep.level==T){
              master=subset(master, tolower(implementation.level) %in% tolower(implementation.level.choice))

              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Implementation levels included:", choice=paste(implementation.level, collapse = ", ")))

            } else {
              master=subset(master, ! tolower(implementation.level) %in% tolower(implementation.level.choice))

              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Implementation levels included:", choice=paste("All except ", paste(implementation.level, collapse = ", "), sep="")))

            }

          }


        }
      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for implementation.level"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }


      # eligible.firms
      # keep.firms
      if(is.null(eligible.firms)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Eligible firms categories included:", choice="Any"))

      } else {

        if(is.null(keep.firms)){
          stop.print <- "Please specify whether you want to focus on the specified eligibe firms categories or exclude them (keep.level=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          elig.firms <- gtalibrary::elig.firms
          eligible.firms.choice <- eligible.firms
          check=gta_parameter_check(tolower(eligible.firms), tolower(elig.firms$eligible.firms))

          if(check!="OK"){
            stop.print <- paste("Unknown eligible firms categorie(s): ", check, ".", sep="")
            error.message <<- c(T, stop.print)
            stop(stop.print)

          } else {

            if(keep.firms==T){
              master=subset(master, tolower(eligible.firms) %in% tolower(eligible.firms.choice))

              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Eligible firms categories included:", choice=paste(eligible.firms, collapse = ", ")))

            } else {
              master=subset(master, ! tolower(eligible.firms) %in% tolower(eligible.firms.choice))

              parameter.choices=rbind(parameter.choices,
                                      data.frame(parameter="Eligible firms categories included:", choice=paste("All except ", paste(eligible.firms, collapse = ", "), sep="")))

            }

          }


        }
      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for eligible.firms"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }


      # cpc.sectors
      # keep.cpc
      if(is.null(cpc.sectors)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="CPC sectors included:", choice="All"))

      } else {

        if(is.null(keep.cpc)){
          stop.print <- "Please specify whether you want to focus on the specified CPC sectors or exclude them (keep.cpc=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          # CPC code check
          cpc.sectors <- gta_cpc_code_check(codes = cpc.sectors)


          # Create new specific id and master.temp
          master$new.id <- seq(1,nrow(master))
          master.temp <- unique(master[,c("new.id", "affected.sector")])
          master.temp <- cSplit(master.temp, which(colnames(master.temp)=="affected.sector"), direction="long", sep=",")

          # Filter sectors
          if(keep.cpc==T){
            master.temp=subset(master.temp, as.numeric(affected.sector) %in% cpc.sectors)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="CPC codes included:", choice=paste(cpc.sectors, collapse = ", ")))

          } else {
            master.temp=subset(master.temp, ! as.numeric(affected.sector) %in% cpc.sectors)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="CPC codes included:", choice=paste("All except ", paste(cpc.sectors, collapse = ", "), sep="")))

          }

          # clear affected.sector column
          master$affected.sector <- NULL

          # Check # of rows
          if(nrow(master.temp)==0) {
            stop.print <- "Unfortunately no rows remaining while filtering for cpc.sectors"
            error.message <<- c(T, stop.print)
            stop(stop.print)
          }

          # Collapse cpc codes by id
          master.temp <- aggregate( .~ new.id, master.temp, function(x) toString(x))

          # Merge and remove new.id
          master <- merge(master, master.temp, by="new.id")
          master$new.id <- NULL

          ## Correcting the affected product column to only include HS codes belong to the cpc.sectors, if any.
          if(min(cpc.sectors)<500){

            if(keep.cpc==T){
              products=gta_cpc_to_hs(cpc.sectors[cpc.sectors<500])
            } else{
              not.mentioned.cpc=unique(gtalibrary::cpc.to.hs$cpc)[! unique(gtalibrary::cpc.to.hs$cpc) %in% cpc.sectors]
              products=gta_cpc_to_hs(not.mentioned.cpc)
            }

            # Create new specific id and master.temp
            master$new.id <- seq(1,nrow(master))
            master.temp <- unique(master[,c("new.id", "affected.product")])
            master.temp <- cSplit(master.temp, which(colnames(master.temp)=="affected.product"), direction="long", sep=",")

            master.temp=subset(master.temp, affected.product %in% products)


            # clear affected.product/affected.sector column
            master$affected.product <- NULL

            # Check # of rows
            if(nrow(master.temp)==0) {
              stop.print <- "Unfortunately no rows remaining while filtering for hs.codes"
              error.message <<- c(T, stop.print)
              stop(stop.print)
            }
            # Collapse hs codes by id
            master.hs <- aggregate( .~ new.id, master.temp, function(x) toString(x))


            # Merge and remove new.id
            master <- merge(master, master.hs, by="new.id", all.x=T) # all.x=T is vital here since there may also be service sectors in cpc.sectors
            master$new.id <- NULL





          } else {
            ## If the stated sectors only include services, then remove all HS codes that may also have been affected by the same intervention
            master$affected.product=NA
          }

        }


      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for cpc.sectors"
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }


      # hs.codes
      # keep.hs
      if(is.null(hs.codes)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="HS codes included:", choice="All"))

      } else {

        if(is.null(keep.hs)){
          stop.print <- "Please specify whether you want to focus on the specified HS codes or exclude them (keep.hs=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          # HS code check
          hs.codes <- gta_hs_code_check(codes = hs.codes)

          # Create new specific id and master.temp
          master$new.id <- seq(1,nrow(master))
          master.temp <- unique(master[,c("new.id", "affected.product")])
          master.temp <- cSplit(master.temp, which(colnames(master.temp)=="affected.product"), direction="long", sep=",")

          # Filter products
          if(keep.hs==T){
            master.temp=subset(master.temp, affected.product %in% hs.codes)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="HS codes included:", choice=paste(hs.codes, collapse = ", ")))

          } else {
            master.temp=subset(master.temp, ! affected.product %in% hs.codes)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="HS codes included:", choice=paste("All except ", paste(hs.codes, collapse = ", "), sep="")))

          }

          # clear affected.product/affected.sector column
          master$affected.product <- NULL
          master$affected.sector <- NULL

          # Check # of rows
          if(nrow(master.temp)==0) {
            stop.print <- "Unfortunately no rows remaining while filtering for hs.codes"
            error.message <<- c(T, stop.print)
            stop(stop.print)
          }
          # Collapse hs codes by id
          master.hs <- aggregate( .~ new.id, master.temp, function(x) toString(x))

          # Add and collapse corresponding CPC codes
          cpc=gtalibrary::cpc.to.hs
          cpc$affected.product <- cpc$hs
          cpc$affected.sector <- cpc$cpc
          master.temp=merge(master.temp, cpc, by="affected.product", all.x=T)

          # Check # of rows
          if(nrow(master.temp)==0) {
            stop.print <- "Unfortunately no rows remaining while filtering for hs.codes"
            error.message <<- c(T, stop.print)
            stop(stop.print)
          }
          master.cpc <- aggregate(affected.sector ~ new.id, master.temp, function(x) toString(unique(x)))
          cpc$affected.product <- NULL
          cpc$affected.sector <- NULL

          # Merge and remove new.id
          master <- merge(master, master.hs, by="new.id")
          master <- merge(master, master.cpc, by="new.id")
          master$new.id <- NULL

        }


      }

      # Check # of rows
      if(nrow(master)==0) {
        error.message <<- c(T, "Unfortunately no rows remaining after filtering for hs.codes")
        stop("Unfortunately no rows remaining after filtering for hs.codes")
      }


      # reporting lag adjustment
      if(is.null(lag.adjustment)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Lag adjustment:", choice="Unadjusted"))

      } else {

        if (is.na(as.Date(lag.adjustment, "%m-%d"))==T) {
          stop.print <- "Please specifiy a valid lag date ('mm-dd')."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else {

          # Remove interventions without implementation date
          master<-subset(master, is.na(date.implemented)==F)

          # set lag date
          master$date.lag=as.Date(paste(data.table::year(master$date.implemented),lag.adjustment, sep="-"),"%Y-%m-%d")
          master=subset(master, date.published<=date.lag)
          master$date.lag=NULL

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Lag-adjustment date:", choice=paste(lag.adjustment)))

        }

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for lag.adjustment"
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }


      # reporting period
      if(is.null(reporting.period)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Reporting period:", choice="Complete GTA monitoring period"))

      } else {

        report.start=as.Date(reporting.period[1], "%Y-%m-%d")
        report.end=as.Date(reporting.period[2], "%Y-%m-%d")

        if(is.na(report.start)|is.na(report.end)){
          stop.print <- "Reporting period does not correspond to format YYY-MM-DD"
          error.message <<- c(T, stop.print)
          stop(stop.print)

        }

        master=subset(master, date.published>=report.start & date.published<=report.end)


        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Reporting period:", choice="Complete GTA monitoring period"))

      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for reporting.period"
        error.message <<- c(T, stop.print)
        stop(stop.print)

      }


      ######## This needs to be the last check (else we won't know whether other parameters accidently removed the sought IDs.)
      # intervention.id
      # keep.intervention
      if(is.null(intervention.ids)){

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Intervention IDs included:", choice="All"))

      } else {

        if(is.null(keep.interventions)){
          stop.print <- "Please specify whether you want to focus on the specified intervetion IDs or exclude them (keep.interventions=T/F)."
          error.message <<- c(T, stop.print)
          stop(stop.print)

        } else{

          gta.interventions = unique(master$intervention.id)

          if(keep.interventions==T){

            check=gta_parameter_check(intervention.ids, gta.interventions)

            if(check!="OK"){
              stop.print <- paste("Unknown intervention IDs: ", check, ". You may have removed them with another parameter choice.", sep="")
              error.message <<- c(T, stop.print)
              stop(stop.print)

            }

            master=subset(master, intervention.id %in% intervention.ids)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Intervention IDs included:", choice=paste(intervention.ids, collapse = ", ")))

          } else {
            master=subset(master, ! intervention.id %in% intervention.ids)

            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Intervention IDs included:", choice=paste("All except ", paste(intervention.ids, collapse = ", "), sep="")))

          }





        }
      }

      # Check # of rows
      if(nrow(master)==0) {
        stop.print <- "Unfortunately no rows remaining after filtering for intervention.ids"
        error.message <<- c(T, stop.print)
        stop(stop.print)
      }

      ## writing to disk
      if (xlsx==T) {
        print("Saving XLSX ...")
        if(is.null(output.path)){
          write.xlsx(master, file=paste("GTA data slicer output ", Sys.Date(),".xlsx", sep=""), sheetName = "Interventions", row.names = F)
          write.xlsx(parameter.choices, file=paste("GTA data slicer output ", Sys.Date(),".xlsx", sep=""), sheetName = "Parameter choices", row.names = F, append=T)
          print("Saving XLSX ... completed in working directory")
        } else {
          write.xlsx(trade.coverage.estimates, file=output.path, sheetName = "Estimates", row.names = F)
          write.xlsx(parameter.choices, file=output.path, sheetName = "Parameter choices", row.names = F, append=T)
          print("Saving XLSX ... completed in output path")
        }
      }


      ## Returning the result
      error.message <<- FALSE
      eval(parse(text=paste(df.name, "<<-master", sep="")))
      eval(parse(text=paste(pc.name, "<<-parameter.choices", sep="")))

  },
  error = function(error.msg) {
    if(exists("stop.print")){
      error.message <<- c(T, stop.print)
      print(paste("[ERROR DATA SLICER]: ",stop.print, sep=""))
    } else {
      error.message <<- c(T,error.msg$message)
      print(paste("[ERROR DATA SLICER]: ",error.msg$message, sep=""))
      }
    master.sliced<<- master[0,]
  })
}
