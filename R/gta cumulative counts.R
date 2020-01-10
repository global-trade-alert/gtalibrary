# Roxygen documentation

#' GTA in force interventions counter
#'
#' Computes number of interventions in force.
#'
#' @param counts.by Specify whether to count by month, quarter or year.'.
#' @param coverage.period Specify the range of years of interest.'.
#' @details all gta_data_slicer parameters are permissible.'.

#' @return Outputs in force interventions by different given periods.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


# Function infos and parameters  --------------------------------------------

gta_cumulative_counts=function(
  counts.by = 'quarter',
  coverage.period = NULL,
  data.path = "data/master_plus.Rdata",
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
  keep.revocation.na = TRUE,
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
  
  library(lubridate)
  library(zoo)
  library(stringr)
  
  if(length(counts.by)!=1 | !tolower(counts.by)%in%c('qtr','quarter','month','year','yr')){
    stop.print <- "counts.by must be a single value, and only the following are permissible: 'qtr', 'quarter', 'month', 'year', 'yr'"
    error.message <<- c(T, stop.print)
    stop(stop.print)
  }
  
  
  if(is.null(coverage.period)){
    coverage.period=c(NA,NA)
  } else {
    na.provided=length(coverage.period[is.na(coverage.period)])
    coverage.period=as.numeric(coverage.period)
    if(length(coverage.period)!= 2 | !all(as.numeric(str_sub(coverage.period,1,4)) %in% 2008:(year(Sys.Date())+5) | is.na(coverage.period)) | na.provided!=length(coverage.period[is.na(coverage.period)])){
      stop.print <- "coverage.period must be given as a year range. To filter implementation dates use the implementation.date parameter instead. NA values are permissible but must be provided, i.e. c(2008,NA). Minimum coverage year is 2008."
      error.message <<- c(T, stop.print)
      stop(stop.print)
    } else {
      if(is.na(coverage.period[1])){coverage.period[1]=2008}
      if(is.na(coverage.period[2])){coverage.period[2]=year(Sys.Date())}
    }
  }  
  
  gta_data_slicer(data.path = data.path,
                  gta.evaluation = gta.evaluation,
                  affected.flows = affected.flows,
                  implementing.country = implementing.country,
                  keep.implementer = keep.implementer,
                  affected.country = affected.country,
                  keep.affected = keep.affected,
                  incl.affected.strictness = incl.affected.strictness,
                  keep.other = keep.others,
                  nr.affected = nr.affected,
                  nr.affected.incl = nr.affected.incl,
                  announcement.period = announcement.period,
                  implementation.period = implementation.period,
                  keep.implementation.na = keep.implementation.na,
                  revocation.period = revocation.period,
                  keep.revocation.na = keep.revocation.na,
                  submission.period = submission.period,
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
                  lag.adjustment=lag.adjustment,
                  reporting.period=reporting.period,
                  df.name=df.name,
                  pc.name=pc.name,
                  xlsx = xlsx,
                  output.path = output.path)
  
  if(nrow(master.sliced)==0) {
    stop.print <- "No rows remain after subsetting"
    error.message <<- c(T, stop.print)
    stop(stop.print)
  }
  
  base=unique(subset(master.sliced, select=c('date.implemented','date.removed')))
  
  #considered in force if: 
  #announcement<cutoff & (removal>cutoff or no removal)
  #in english: evaluation is done at the end of the year
  
  #counts by month
  if(tolower(counts.by)=='month'){
    base$impl <- as.numeric(gsub('-','',format(as.Date(base$date.implemented), "%Y-%m")))
    base$rem <- as.numeric(gsub('-','',format(as.Date(base$date.removed), "%Y-%m")))
    base=subset(base,select=c('impl','rem'))
    
    for (mth in paste0(rep(seq(coverage.period[1],coverage.period[2],1),each=12),c(paste0('0',1:9),10:12))){
      
      base[[paste0(str_sub(mth,1,4),'-',str_sub(mth,5,6))]]=0
      base[[paste0(str_sub(mth,1,4),'-',str_sub(mth,5,6))]][intersect(which(base$impl<=as.numeric(mth)),(which((base$rem>as.numeric(mth)) | (is.na(base$rem)==T))))] = 1
      
    }
    
  }
  
  #counts by year
  if(tolower(counts.by) %in% c('year','yr')){
    base$impl <- as.numeric(str_sub(base$date.implemented,1,4))
    base$rem <- as.numeric(str_sub(base$date.removed,1,4))
    base=subset(base,select=c('impl','rem'))
    
    for (yr in seq(coverage.period[1],coverage.period[2],1)){
      
      base[[as.character(yr)]]=0
      base[[as.character(yr)]][intersect(which(base$impl<=yr),(which((base$rem>yr) | (is.na(base$rem)==T))))] = 1
      
    }
    
  }
  
  #counts by quarter
  if(tolower(counts.by) %in% c('quarter','qtr')){
    base$impl=as.yearqtr(as.Date(base$date.implemented,"%Y-%m-%d"))
    base$rem=as.yearqtr(as.Date(base$date.removed,"%Y-%m-%d"))
    base=subset(base,select=c('impl','rem'))
    
    for (qtr in paste0(rep(seq(coverage.period[1],coverage.period[2],1),each=4),' Q',1:4)){
      
      base[[qtr]]=0
      base[[qtr]][intersect(which(base$impl<=as.yearqtr(qtr)),(which((base$rem>as.yearqtr(qtr)) | (is.na(base$rem)==T))))] = 1
      
    }
    
  }
  
  
  base=data.frame(period=names(base)[!names(base)%in%c('impl','rem')],
                  in.force.interventions=colSums(base[,!names(base)%in%c('impl','rem')]))
  rownames(base)=1:nrow(base)
  return(base)
  
}

