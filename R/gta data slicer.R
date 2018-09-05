# Roxygen documentation

#' This function allows you to extract a tailored subset of the GTA database.
#'
#' Make use of the many variables captured in the taxonomy of the Global Trade Alert and tailor the data to your needs. This function returns a list of 2 data frames. The first, master, contains the subset of GTA data you requested. The second, parameter.choices, lists all implicit or exlicited choices that led to the subset.
#'
#' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param gta.evaluation Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
#' @param affected.flow Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is 'any'. Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
#' @param implementing.country Specify the implementing countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.implementer Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementing country.
#' @param affected.country Specify the affected countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.affected Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated affected country.
#' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
#' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
#' @param revocation.period Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param in.force.today Specify whether you want to focus on interventions in force today ('TRUE') or no longer in force today ('FALSE'). Default is 'any'.
#' @param intervention.type Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
#' @param keep.type Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
#' @param mast.chapter Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
#' @param keep.mast Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
#' @param implementation.level Specify the government level responsible for the intervention.  Default is 'any'. Permissible values are 'supranational', 'national', 'subnational', 'IFI', 'NFI' or combinations thereof. IFI and NFI refer to government-owned financial institutions that are owned by one ('NFI') or more ('IFI') governments.
#' @param keep.level Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementation levels.
#' @param eligible.firms Concentrate the analysis on interventions that are targeted at certain subsets. Default is 'any'. Permissible values are 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise' or combinations thereof.
#' @param keep.firms Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated firm subsets.
#' @param cpc.sectors Provide a vector of CPC codes that you are interested in (version 2.1, any digit level).
#' @param keep.cpc Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
#' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
#' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
#' @param intervention.id Provide a vector of intervention IDs.
#' @param keep.intervention Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated intervention IDs.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_data_slicer=function(data.path="data/master_plus.Rdata",
                        gta.evaluation= NULL,
                        affected.flow = NULL,
                        implementing.country = NULL,
                        keep.implementer = NULL,
                        affected.country = NULL,
                        keep.affected = NULL,
                        announcement.period = NULL,
                        implementation.period = NULL,
                        revocation.period = NULL,
                        in.force.today = NULL,
                        intervention.type = NULL,
                        keep.type = NULL,
                        mast.chapter = NULL,
                        keep.mast = NULL,
                        implementation.level = NULL,
                        keep.level = NULL,
                        eligible.firms = NULL,
                        keep.firms = NULL,
                        cpc.sectors = NULL,
                        keep.cpc = NULL,
                        hs.codes = NULL,
                        keep.hs = NULL,
                        intervention.id = NULL,
                        keep.intervention = NULL
                        ){
  library("httr")

  ## Collecting
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
      print(paste("Unkown GTA evaluation(s): ", check, ".", sep=""))
    } else {
        eval=tolower(gta.evaluation)
        master=subset(master, tolower(gta.evaluation) %in% eval)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="GTA evaluations included:", choice=paste(eval, collapse=", ")))

    }
    rm(check, eval)
  }


  # affected.flow

  if(is.null(affected.flow)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Affected flows included:", choice="inward, outward, outward subsidy"))

  } else {
    check=gta_parameter_check(tolower(affected.flow), c("inward", "outward", "outward subsidy"))
    if(check!="OK"){
      print(paste("Unkown GTA evaluation(s): ", check, ".", sep=""))
    } else {
      flow=tolower(affected.flow)
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

  # announcement.period
  date.period=announcement.period

  if(is.null(date.period)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Announcement period:", choice="Full GTA monitoring period"))

  } else {

    if(length(date.period)!=2){
      stop("Please specify the date pair (after.date, before.date) for the announcement period. 'NA' is permissible, but has to be specified in case you only want one of the two.")
    } else{
      dates=sum(as.numeric(is.na(date.period)))

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
      dates=sum(as.numeric(is.na(date.period)))

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
      dates=sum(as.numeric(is.na(date.period)))

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

          if(is.na(as.Date(date.period[1], "%Y-%m-%d"))==F){
            master=subset(master, date.removed>=as.Date(date.period[1], "%Y-%m-%d"))
            parameter.choices=rbind(parameter.choices,
                                    data.frame(parameter="Revocation period:", choice=paste(date.period[1]," or more recent", sep="")))
          }

          if(is.na(as.Date(date.period[2], "%Y-%m-%d"))==F){
            master=subset(master, date.removed<=as.Date(date.period[2], "%Y-%m-%d"))
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


  # in.force.today
  if(is.null(in.force.today)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Currently in force:", choice="Regardless"))

  } else {

    if(in.force.today==T){

      master=subset(master, date.implemented<=Sys.Date() & (is.na(date.removed)==T|date.removed>=Sys.Date()))

      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="Currently in force:", choice="Yes"))

    } else {
      if(in.force.today==F){

        master=subset(master, (date.implemented<Sys.Date() & is.na(date.removed)==F & date.removed<Sys.Date()) | is.na(date.implemented))

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Currently in force:", choice="No"))

      }else{
        stop("Please specify in.force.today as either TRUE or FALSE.")
      }
    }

  }

  # intervention.type
  # keep.type


  # mast.chapter
  # keep.mast


  # implementation.level
  # keep.level


  # eligible.firms
  # keep.firms


  # cpc.sectors
  # keep.cpc


  # hs.codes
  # keep.hs


  # intervention.id
  # keep.intervention




  ## Returning the result

  output=list(parameter.choices, master)
  return(output)

  rm(parameter.choices, master)
}
