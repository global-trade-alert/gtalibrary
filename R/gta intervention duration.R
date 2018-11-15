# Roxygen documentation

#' GTA intra-year duration calculator
#'
#' Computes the share of each calendar year that each intervention was in force.
#'
#' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param is.data.frame Specify if the data path is a data frame in the current environment. Default is FALSE. If you supply a data frame, the first three columns need to be (1) intervention ID, (2) implementation date, (3) removal date. In that order.
#' @param years The calendar years for which to calculate the shares. Calculation includes interventions based on enforcement status, not implementation date i.e. if you start in 2010, this function will also work with interventions implemneted in 2009 but still in force in 2010. Specify as c(start.year, end.year). Default is c(2008,CURRENT.YEAR).
#' @param current.year.todate Should the duration for the current year, if included, be calculated as 'share of year to date' (TRUE) or 'share of entire current year' (FALSE). Default is TRUE.
#' @param df.name Set the name of the generated result data frame. Default is intervention.duration.
#' @param pc.name Set the name of the generated parameter choice data frame. Default is parameter.choice.duration.
#'
#' @return Output is two data frames. First data frame includes the share of each year that in intervention was in force. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


# Function infos and parameters  --------------------------------------------

gta_intervention_duration <- function(
  data.path="data/master_plus.Rdata",
  is.data.frame=FALSE,
  years=NULL,
  current.year.todate=TRUE,
  df.name="intervention.duration",
  pc.name="parameter.choice.duration"
) {

  ## initialising
  library(data.table)
  parameter.choices=data.frame(parameter=character(), choice=character(),stringsAsFactors = F)

  ## data file
  if(is.data.frame){
    eval(parse(text=paste("master=", data.path, sep="")))

    if(ncol(master)<3){stop("Please supply a data frame with at least three columns (intervention ID, implementation and removal date).")}

    if(ncol(master)==3){
      names(master)=c("intervention.id", "date.implemented", "date.removed")
    }else{
      names(master)=c("intervention.id", "date.implemented", "date.removed", names(master[,4:ncol(master)]))
    }


  }else{
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
  }

  ## check years
  if(is.null(years)){
    year.start=2008
    year.end=year(Sys.Date())
  } else {
    if(length(years)!=2){stop("Please supply a year vector with two entries e.g. c(2008, 2018)")}
    if((min(years)<2008)|(max(years)>year(Sys.Date())) ){stop(paste("Please only supply years between 2008 and ", year(Sys.Date()), sep=""))}
    if(is.numeric(years)==F){stop("Please supply a year vector with two integer entries e.g. c(2008, 2018)")}

    if(years[1]%%1==0){year.start=years[1]}else{stop("Please supply a year vector with two integer entries e.g. c(2008, 2018)")}
    if(years[2]%%1==0){year.end=years[2]}else{stop("Please supply a year vector with two integer entries e.g. c(2008, 2018)")}
  }
  parameter.choices=rbind(parameter.choices, data.frame(parameter="Enforcement years:", choice=paste(year.start, " to ",year.end, sep="")))



  # calculating intra-year duration
  master=unique(master[,c("intervention.id", "date.implemented", "date.removed")])
  master=subset(master, year(date.implemented)<=year.end )
  master$date.removed[is.na(master$date.removed)]=as.Date(paste(year(Sys.Date())+1,"-01-01",sep=""), "%Y-%m-%d")


  yr.length=data.frame(year=c(year.start:year.end), days=365)
  yr.length$days[yr.length$year %in% seq(2008,year.end,4)]=366

  duration=data.frame(expand.grid(intervention.id=unique(master$intervention.id), year=c(year.start:year.end)), share=NA)
  duration=merge(duration, master, by="intervention.id", all.x=T)

  # duration is zero
  duration$share[year(duration$date.implemented)>duration$year]=0
  duration$share[year(duration$date.removed)<duration$year]=0

  # duration is one
  duration$share[year(duration$date.implemented)<duration$year & year(duration$date.removed)>duration$year]=1

  # durations for cases that start/end within the given year
  intra.year=subset(duration, is.na(share))

  for(i in 1:nrow(intra.year)){

    if(intra.year$year[i]<year(Sys.Date())){
      intra.year$share[i]=sum(as.numeric(c(intra.year$date.implemented[i]:intra.year$date.removed[i]) %in% c(as.Date(paste(intra.year$year[i], "-01-01", sep=""), "%Y-%m-%d"):as.Date(paste(intra.year$year[i], "-12-31", sep=""), "%Y-%m-%d"))))/yr.length$days[yr.length$year==intra.year$year[i]]
    }else{

      ## correcting current year duration (if required)
      if(current.year.todate){
        intra.year$share[i]=sum(as.numeric(c(intra.year$date.implemented[i]:intra.year$date.removed[i]) %in% c(as.Date(paste(intra.year$year[i], "-01-01", sep=""), "%Y-%m-%d"):Sys.Date())))/(as.numeric(Sys.Date()-as.Date(paste(year(Sys.Date()),"-01-01",sep="")))+1)
      } else{
        intra.year$share[i]=sum(as.numeric(c(intra.year$date.implemented[i]:intra.year$date.removed[i]) %in% c(as.Date(paste(intra.year$year[i], "-01-01", sep=""), "%Y-%m-%d"):as.Date(paste(intra.year$year[i], "-12-31", sep=""), "%Y-%m-%d"))))/yr.length$days[yr.length$year==intra.year$year[i]]
      }

    }

  }

  duration=rbind(subset(duration, is.na(share)==F), intra.year)
  duration=unique(duration[,c("intervention.id","year","share")])

  eval(parse(text=paste(df.name, "<<-duration", sep="")))
  eval(parse(text=paste(pc.name, "<<-parameter.choices", sep="")))
}
