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
#'
#' @return Output is two data frames. First data frame includes the trade values for the given importer-exporter-product combinations. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


# Function infos and parameters  --------------------------------------------

gta_trade_value_bilateral <- function(
  importing.country = NULL,
  keep.importer = NULL,
  exporting.country = NULL,
  keep.exporter = NULL,
  cpc.sectors=NULL,
  keep.cpc=TRUE,
  hs.codes = NULL,
  keep.hs = TRUE,
  trade.data="base",
  trade.data.path="data/support tables/Goods support table for gtalibrary.Rdata",
  df.name="trade.base.bilateral",
  pc.name="parameter.choice.trade.base"
) {

  ## initialising
  library(data.table)
  parameter.choices=data.frame(parameter=character(), choice=character())

  if(!trade.data %in% c("base","prior year","current year", "before announcement","during announcement", paste(2005:2020))){
    stop("Please specify proper trade data choice (i.e. 'base', a year between 2005 and 2018, 'prior year' or 'current year'.")
  }

  if(trade.data=="base"){
    trade.base=gtalibrary::trade.base
    trade.base$trade.value=trade.base$trade.value/3
  } else{
    load(trade.data.path)

    if(trade.data %in% paste(2005:2020)){
      yr=as.numeric(trade.data)
      trade.base=subset(trade.annual, year==yr)
      trade.base$year=NULL
      rm(trade.annual)
    } else {
      trade.base=trade.annual
      rm(trade.annual)

      if(grepl("prior", trade.data, ignore.case = T)){
        trade.base$year=trade.base$year+1
      }

    }
  }


  ## importer
  if(is.null(importing.country)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Importing countries included in trade data:", choice="All"))

  } else {

    if(is.null(keep.importer)){
      stop("Please specify whether you want to focus on the specified importing countries or exclude them (keep.importer=T/F).")
    } else{

      importers=gta_un_code_vector(importing.country, role="importing")

      if(keep.importer==T){
        trade.base=subset(trade.base, i.un %in% importers)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Importing countries included in trade data:", choice=paste(importing.country, collapse = ", ")))

      } else {
        trade.base=subset(trade.base, ! i.un %in% importers)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Importing countries included in trade data:", choice=paste("All except ", paste(importing.country, collapse = ", "), sep="")))

      }


    }
  }

  ## exporter
  if(is.null(exporting.country)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Exporting countries included in trade data:", choice="All"))

  } else {

    if(is.null(keep.exporter)){
      stop("Please specify whether you want to focus on the specified exporting countries or exclude them (keep.exporter=T/F).")
    } else{

      exporters=gta_un_code_vector(exporting.country, role="exporting")

      if(keep.exporter==T){
        trade.base=subset(trade.base, a.un %in% exporters)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Exporting countries included in trade data:", choice=paste(exporting.country, collapse = ", ")))

      } else {
        trade.base=subset(trade.base, ! a.un %in% exporters)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Exporting countries included in trade data:", choice=paste("All except ", paste(exporting.country, collapse = ", "), sep="")))

      }


    }
  }


  ## hs codes
  if(is.null(hs.codes)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="HS codes included in trade data:", choice="All"))

  } else {

    if(is.null(keep.hs)){
      stop("Please specify whether you want to focus on the specified HS codes or exclude them (keep.hs=T/F).")
    } else{

      # HS code check
      hs.codes <- gta_hs_code_check(codes = hs.codes)

      # Filter products
      if(keep.hs==T){
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="HS codes included in trade data:", choice=paste(hs.codes, collapse = ", ")))

      } else {
        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="HS codes included in trade data:", choice=paste("All except ", paste(hs.codes, collapse = ", "), sep="")))

      }


    }

  }

  ## cpc codes
  if(is.null(cpc.sectors)){

    if(is.null(hs.codes)){
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="CPC sectors included in trade data:", choice="All"))
    } else {

      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="CPC sectors included in trade data:", choice="Those implied by HS code choice."))
    }

  }else{

    if(is.null(hs.codes)){
      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="CPC sectors included in trade data:", choice=paste(sprintf("%03i",cpc.sectors), collapse = ", ")))

      hs.codes=gta_cpc_to_hs(cpc.sectors[cpc.sectors<500])
      keep.hs=keep.cpc

    } else {

      parameter.choices=rbind(parameter.choices,
                              data.frame(parameter="CPC sectors included in trade data:", choice=paste("Those implied by HS code choice and CPC ",paste(sprintf("%03i",cpc.sectors), collapse = ", "),".",sep="")))


      if(keep.cpc==T){
        strs=gtalibrary::cpc.to.hs
        hs.codes=unique(c(hs.codes, strs$hs[strs$cpc %in% cpc.sectors]))
        rm(strs)

      }else {
        strs=gtalibrary::cpc.to.hs
        hs.codes=unique(c(hs.codes, strs$hs[! strs$cpc %in% cpc.sectors]))
        rm(strs)
      }

    }


  }

  # adjusting trade to hs/cpc codes

  if(is.null(hs.codes)==F){
    if(keep.hs==T){
      trade.base=subset(trade.base, hs6 %in% hs.codes)
    } else{
      trade.base=subset(trade.base, ! hs6 %in% hs.codes)
    }
  }


  eval(parse(text=paste(df.name, "<<-trade.base", sep="")))
  eval(parse(text=paste(pc.name, "<<-parameter.choices", sep="")))
}
