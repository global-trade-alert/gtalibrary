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
#' @param target Specify the countries targeted uniquely or together by interventions. Default is 'any'. Permissible values are country names or UN codes.
#' @param additional.targets Specify the number of countries targeted additionally to the specified target country. Default is any. Provide value as integer.
#' @param targets.combined Specify whether interventions shall affect all targets combined ('TRUE') or combined as well as uniquely ('FALSE'). Default is 'TRUE'.
#' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
#' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
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
#' @param df.name Set the name of the generated result data frame. Default is master.sliced.
#' @param pc.name Set the name of the generated parameter choice data frame. Default is parameter.choice.slicer.

#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_data_slicer=function(data.path="data/master_plus.Rdata",
                        gta.evaluation= NULL,
                        affected.flows = NULL,
                        implementing.country = NULL,
                        keep.implementer = NULL,
                        affected.country = NULL,
                        keep.affected = NULL,
                        target = NULL,
                        additional.targets = NULL,
                        targets.combined = TRUE,
                        announcement.period = NULL,
                        implementation.period = NULL,
                        revocation.period = NULL,
                        keep.revocation.na = NULL,
                        submission.period = NULL,
                        in.force.today = 'Any',
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
                        df.name="master.sliced",
                        pc.name="parameter.choice.slicer"
                        ){
  library("httr")
  library("splitstackshape")
  library("lubridate")
  library("data.table")

  ## Collecting parameter values
  parameter.choices=data.frame(parameter=character(), choice=character())

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

  # gta.evaluation
  if(is.null(gta.evaluation)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="GTA evaluations included:", choice="red, amber, green"))

  } else {
    check=gta_parameter_check(tolower(gta.evaluation), c("red", "amber", "green"))
    if(check!="OK"){
      stop(paste("Unknown GTA evaluation(s): ", check, ".", sep=""))
    } else {
        eval=tolower(gta.evaluation)
        master=subset(master, tolower(gta.evaluation) %in% eval)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="GTA evaluations included:", choice=paste(eval, collapse=", ")))

    }
    rm(check, eval)
  }


  # affected.flows

  if(is.null(affected.flows)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Affected flows included:", choice="inward, outward, outward subsidy"))

  } else {
    check=gta_parameter_check(tolower(affected.flows), c("inward", "outward", "outward subsidy"))
    if(check!="OK"){
      stop(paste("Unknown GTA evaluation(s): ", check, ".", sep=""))
    } else {
      flow=tolower(affected.flows)
      master=subset(master, tolower(affected.flow) %in% flow)

      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Affected flows included:", choice=paste(flow, collapse=", ")))

    }
    rm(check, flow)
  }


  # implementing.country
  # keep.implementer
  if(is.null(implementing.country)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Implementing countries included:", choice="All"))

  } else {

    if(is.null(keep.implementer)){
      stop("Please specify whether you want to focus on the specified implementing countries or exclude them (keep.implementer=T/F).")
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

      rm(check, implementers)
    }
  }
  # affected.country
  # keep.affected

  if(is.null(affected.country)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Affected countries included:", choice="All"))

  } else {

    if(is.null(keep.affected)){
      stop("Please specify whether you want to focus on the specified affected countries or exclude them (keep.affected=T/F).")
    } else{

      affected=gta_un_code_vector(affected.country, role="affected")

      if(keep.affected==T){
        master=subset(master, a.un %in% affected)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Affected countries included:", choice=paste(affected.country, collapse = ", ")))

      } else {
        master=subset(master, ! a.un %in% affected)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Affected countries included:", choice=paste("All except ", paste(affected.country, collapse = ", "), sep="")))

      }

      rm(check, affected)
    }
  }


  # targeted countries
  # additional targets
  if(is.null(target)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Targeted countries:", choice="Any"))

  } else {
    target <- gta_un_code_vector(target, role="targeted")

    # unique id to a.un combinations
    all.id <- subset(as.data.frame(unique(master[,c("intervention.id","a.un")])), is.na(a.un)==F)

    if (targets.combined==T){
      # ids where all targets are affected
      target.id <- subset(aggregate(Freq~intervention.id, as.data.frame(table(subset(all.id, a.un %in% target))), sum), Freq == length(target))
      # Subset all interventions and create frequency table and count sum of frequencies
      target.id <- aggregate(Freq~intervention.id, as.data.frame(table(subset(all.id, intervention.id %in% target.id$intervention.id))), sum)

    } else if (targets.combined == F){
      # ids where all targets, as well as single targets are affected
      target.id.single <- subset(aggregate(Freq~intervention.id, as.data.frame(table(subset(all.id, a.un %in% target))), sum), Freq == 1)
      target.id.combined <- subset(aggregate(Freq~intervention.id, as.data.frame(table(subset(all.id, a.un %in% target))), sum), Freq == length(target))
      # Subset all interventions and create frequency table and count sum of frequencies
      target.id.single <- aggregate(Freq~intervention.id, as.data.frame(table(subset(all.id, intervention.id %in% target.id.single$intervention.id))), sum)
      target.id.combined <- aggregate(Freq~intervention.id, as.data.frame(table(subset(all.id, intervention.id %in% target.id.combined$intervention.id))), sum)

      } else {
      stop("Please specify whether to combine targets or not. targets.combined = T/F")
    }

    # Filter for value of frequency
    if(is.null(additional.targets)){
      if (targets.combined==T){
        master <- subset(master, intervention.id %in% target.id$intervention.id)

      } else if (targets.combined==F){
        target.id = rbind(target.id.combined, target.id.single)
        master <- subset(master, intervention.id %in% target.id$intervention.id)
      }

    } else if (targets.combined == T) {master <- subset(master, intervention.id %in% subset(target.id, Freq==(length(target)+additional.targets))$intervention.id)
    } else if (targets.combined == F) {master <- subset(master, intervention.id %in% subset(target.id.single, Freq==(1 + additional.targets))$intervention.id | intervention.id %in% subset(target.id.combined, Freq==(length(target)+additional.targets))$intervention.id)

    }

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Targeted countries:", choice=paste(paste(target, collapse = ", "), " and ", additional.targets, " additional, targets combined = ",targets.combined, sep="")))

    rm(all.id, target.id)

    }

  # announcement.period
  date.period=announcement.period

  if(is.null(date.period)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Announcement period:", choice="Full GTA monitoring period"))

  } else {

    if(length(date.period)!=2){
      stop("Please specify the date pair (after.date, before.date) for the announcement period. 'NA' is permissible, but has to be specified in case you only want one of the two.")
    } else{
      dates=sum(as.numeric(is.na(date.period))==F)

      if(dates>0){
        if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
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


  # implementation.period
  date.period=implementation.period

  if(is.null(date.period)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Implementation period:", choice="Full GTA monitoring period"))

  } else {


    if(length(date.period)!=2){
      stop("Please specify the date pair (after.date, before.date) for the implementation period. 'NA' is permissible, but has to be specified in case you only want one of the two.")
    } else{

      dates=sum(as.numeric(is.na(date.period))==F)

      if(dates>0){
        if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
          stop("At least one of the implementation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
        }

        if(dates==2){
          master=subset(master, date.implemented>=as.Date(date.period[1], "%Y-%m-%d") & date.implemented<=as.Date(date.period[2], "%Y-%m-%d"))
          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Implementation period:", choice=paste(date.period[1]," - ",date.period[2], sep="")))

        }

        if(dates==1){

          if(is.na(as.Date(date.period[1], "%Y-%m-%d"))==F){
            master=subset(master, date.implemented>=as.Date(date.period[1], "%Y-%m-%d"))
            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Implementation period:", choice=paste(date.period[1]," or more recent", sep="")))
          }

          if(is.na(as.Date(date.period[2], "%Y-%m-%d"))==F){
            master=subset(master, date.implemented<=as.Date(date.period[2], "%Y-%m-%d"))
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


  # revocation.period
  date.period=revocation.period

  if(is.null(date.period)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Revocation period:", choice="None specified"))

  } else {

    if(length(date.period)!=2){
      stop("Please specify the date pair (after.date, before.date) for the revocation period. 'NA' is permissible, but has to be specified in case you only want one of the two.")
    } else{
      dates=sum(as.numeric(is.na(date.period))==F)

      if(dates>0){
        if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
          stop("At least one of the revocation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
        }

        if(dates==2){
          master=subset(master, date.removed>=as.Date(date.period[1], "%Y-%m-%d") & date.removed<=as.Date(date.period[2], "%Y-%m-%d"))
          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Revocation period:", choice=paste(date.period[1]," - ",date.period[2], sep="")))

        }

        if(dates==1){

          if(is.null(keep.revocation.na)){
            stop("Please specify whether you want to keep interventions with missing revocation date or exclude them (keep.revocation.na=T/F).")
          } else{

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
        }

      } else {
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Revocation period:", choice="None specified"))
      }


    }

    remove(date.period)

  }


  # submission.period
  date.period=submission.period

  if(is.null(date.period)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Submission period:", choice="Full GTA monitoring period"))

  } else {

    if(length(date.period)!=2){
      stop("Please specify the date pair (after.date, before.date) for the submission period. 'NA' is permissible, but has to be specified in case you only want one of the two.")
    } else{
      dates=sum(as.numeric(is.na(date.period))==F)

      if(dates>0){
        if(sum(is.na(as.Date(date.period[is.na(date.period)==F], "%Y-%m-%d")))>0){
          stop("At least one of the submission dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
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


  # in.force.today
  if(is.null(in.force.today)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Currently in force:", choice="Regardless"))

  } else {

    if(tolower(in.force.today)=="any"){

      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Currently in force:", choice="Regardless"))

    } else if (tolower(in.force.today)=="yes"){

      master=subset(master, date.implemented<=Sys.Date() & (is.na(date.removed)==T|date.removed>=Sys.Date()))

      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Currently in force:", choice="Yes"))

    } else {
      if(tolower(in.force.today)=='no'){

        master=subset(master, (date.implemented<Sys.Date() & is.na(date.removed)==F & date.removed<Sys.Date()) | is.na(date.implemented))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Currently in force:", choice="No"))

      }else{
        stop("Please specify in.force.today as either 'yes', 'no' or 'any'.")
      }
    }

  }

  # intervention.type
  # keep.type
  if(is.null(intervention.types)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Intervention types included:", choice="All"))

  } else {

    if(is.null(keep.type)){
      stop("Please specify whether you want to focus on the specified intervention types or exclude them (keep.type=T/F).")
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

      rm(check, int.mast.types)
    }
  }


  # mast.chapter
  # keep.mast
  if(is.null(mast.chapters)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Mast chapters included:", choice="All"))

  } else {

    if(is.null(keep.mast)){
      stop("Please specify whether you want to focus on the specified mast chapters or exclude them (keep.mast=T/F).")

    } else{

      #Remove integers from string
      mast.letter <- gsub("[0-9]+","", mast.chapters)
      mast.letter <- mast.letter[mast.letter != ""]

      int.mast.types <- gtalibrary::int.mast.types
      check=gta_parameter_check(tolower(mast.letter), tolower(int.mast.types$mast.chapter.id))

      if(check!="OK"){
        stop(paste("Unknown mast chapter(s): ", check, ".", sep=""))

      } else {

      if(keep.mast==T){
        master=subset(master, tolower(mast.chapter) %in% tolower(mast.letter))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Mast chapters included:", choice=paste(mast.letter, collapse = ", ")))

      } else {
        master=subset(master, ! tolower(mast.chapter) %in% tolower(mast.letter))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Mast chapters included:", choice=paste("All except ", paste(mast.letter, collapse = ", "), sep="")))

      }

      }

      rm(check, int.mast.types, mast.letter)
    }
  }



  # implementation.level
  # keep.level
  if(is.null(implementation.level)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Implementation levels included:", choice="All"))

  } else {

    if(is.null(keep.level)){
      stop("Please specify whether you want to focus on the specified implementation levels or exclude them (keep.level=T/F).")

    } else{

      imp.levels <- gtalibrary::imp.levels

      check=gta_parameter_check(tolower(implementation.level), tolower(imp.levels$implementation.level))

      if(check!="OK"){
        stop(paste("Unknown implementation level(s): ", check, ".", sep=""))

      } else {

        if(keep.level==T){
          master=subset(master, tolower(implementation.level) %in% tolower(implementation.level))

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Implementation levels included:", choice=paste(implementation.level, collapse = ", ")))

        } else {
          master=subset(master, ! tolower(implementation.level) %in% tolower(implementation.level))

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Implementation levels included:", choice=paste("All except ", paste(implementation.level, collapse = ", "), sep="")))

        }

      }

      rm(check, imp.levels)
    }
  }


  # eligible.firms
  # keep.firms
  if(is.null(eligible.firms)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Eligible firms categories included:", choice="Any"))

  } else {

    if(is.null(keep.firms)){
      stop("Please specify whether you want to focus on the specified eligibe firms categories or exclude them (keep.level=T/F).")

    } else{

      elig.firms <- gtalibrary::elig.firms

      check=gta_parameter_check(tolower(eligible.firms), tolower(elig.firms$eligible.firms))

      if(check!="OK"){
        stop(paste("Unknown eligible firms categorie(s): ", check, ".", sep=""))

      } else {

        if(keep.firms==T){
          master=subset(master, tolower(eligible.firms) %in% tolower(eligible.firms))

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Eligible firms categories included:", choice=paste(eligible.firms, collapse = ", ")))

        } else {
          master=subset(master, ! tolower(eligible.firms) %in% tolower(eligible.firms))

          parameter.choices=rbind(parameter.choices,
                                  data.frame(parameter="Eligible firms categories included:", choice=paste("All except ", paste(eligible.firms, collapse = ", "), sep="")))

        }

      }

      rm(check, elig.firms)
    }
  }


  # cpc.sectors
  # keep.cpc
  if(is.null(cpc.sectors)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="CPC sectors included:", choice="All"))

  } else {

    if(is.null(keep.cpc)){
      stop("Please specify whether you want to focus on the specified CPC sectors or exclude them (keep.cpc=T/F).")
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
        master.temp=subset(master.temp, ! as.numeric(affected.product) %in% cpc.sectors)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="CPC codes included:", choice=paste("All except ", paste(cpc.sectors, collapse = ", "), sep="")))

      }

      # clear affected.sector column
      master$affected.sector <- NULL

      # Collapse cpc codes by id
      master.temp <- aggregate( .~ new.id, master.temp, function(x) toString(x))

      # Merge and remove new.id
      master <- merge(master, master.temp, by="new.id")
      master$new.id <- NULL

      ## Correcting the affected product column to only include HS codes belong to the cpc.sectors, if any.
      if(min(cpc.sectors)<500){

        products=gta_cpc_to_hs(cpc.sectors[cpc.sectors<500])

        # Create new specific id and master.temp
        master$new.id <- seq(1,nrow(master))
        master.temp <- unique(master[,c("new.id", "affected.product")])
        master.temp <- cSplit(master.temp, which(colnames(master.temp)=="affected.product"), direction="long", sep=",")

        master.temp=subset(master.temp, affected.product %in% products)


        # clear affected.product/affected.sector column
        master$affected.product <- NULL


        # Collapse hs codes by id
        master.hs <- aggregate( .~ new.id, master.temp, function(x) toString(x))


        # Merge and remove new.id
        master <- merge(master, master.hs, by="new.id", all.x=T) # all.x=T is vital here since there may also be service sectors in cpc.sectors
        master$new.id <- NULL

        rm(products, master.hs, master.temp)



      } else {
      ## If the stated sectors only include services, then remove all HS codes that may also have been affected by the same intervention
        master$affected.product=NA
      }

    }

    rm(check, cpc.names, master.temp)
  }


  # hs.codes
  # keep.hs
  if(is.null(hs.codes)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="HS codes included:", choice="All"))

  } else {

    if(is.null(keep.hs)){
      stop("Please specify whether you want to focus on the specified HS codes or exclude them (keep.hs=T/F).")
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

      # Collapse hs codes by id
      master.hs <- aggregate( .~ new.id, master.temp, function(x) toString(x))

      # Add and collapse corresponding CPC codes
      cpc=gtalibrary::cpc.to.hs
      cpc$affected.product <- cpc$hs
      cpc$affected.sector <- cpc$cpc
      master.temp=merge(master.temp, cpc, by="affected.product", all.x=T)
      master.cpc <- aggregate(affected.sector ~ new.id, master.temp, function(x) toString(unique(x)))
      cpc$affected.product <- NULL
      cpc$affected.sector <- NULL

      # Merge and remove new.id
      master <- merge(master, master.hs, by="new.id")
      master <- merge(master, master.cpc, by="new.id")
      master$new.id <- NULL

    }

      rm(check, hs.names, master.temp, master.cpc, master.hs)
  }



  # reporting lag adjustment
  if(is.null(lag.adjustment)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Lag adjustment:", choice="Unadjusted"))

  } else {

      if (is.na(as.Date(lag.adjustment, "%m-%d"))==T) {
        stop("Please specifiy a valid lag date ('mm-dd').")

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

######## This needs to be the last check (else we won't know whether other parameters accidently removed the sought IDs.)
  # intervention.id
  # keep.intervention
  if(is.null(intervention.ids)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Intervention IDs included:", choice="All"))

  } else {

    if(is.null(keep.interventions)){
      stop("Please specify whether you want to focus on the specified intervetion IDs or exclude them (keep.interventions=T/F).")

    } else{

      gta.interventions = unique(master$intervention.id)

      if(keep.interventions==T){

        check=gta_parameter_check(intervention.ids, gta.interventions)

        if(check!="OK"){
          stop(paste("Unknown intervention IDs: ", check, ". You may have removed them with another parameter choice.", sep=""))

        }

        master=subset(master, intervention.id %in% intervention.ids)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Intervention IDs included:", choice=paste(intervention.ids, collapse = ", ")))

      } else {
        master=subset(master, ! intervention.id %in% intervention.ids)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Intervention IDs included:", choice=paste("All except ", paste(intervention.ids, collapse = ", "), sep="")))

      }




      rm(check, gta.interventions)
    }
  }


  ## Returning the result
  eval(parse(text=paste(df.name, "<<-master", sep="")))
  eval(parse(text=paste(pc.name, "<<-parameter.choices", sep="")))
}
