# Roxygen documentation

#' GTA bilateral trade value
#'
#' Computes a data frame with the bilateral trade values in the GTA base period for the given importer-exporter-product combinations.
#'
#' @param importing.country Specify the importing countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.importer Specify whether to focus on ('TRUE') or exclude ('FALSE') trade with the stated importing country.
#' @param exporting.country Specify the exporting countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.exporter Specify whether to focus on ('TRUE') or exclude ('FALSE') trade with the stated exporting country.
#' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
#' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
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
  hs.codes = NULL,
  keep.hs = NULL,
  df.name="trade.base.bilateral",
  pc.name="parameter.choice.trade.base"
) {

  ## initialising
  library(data.table)
  parameter.choices=data.frame(parameter=character(), choice=character())
  trade.base=gtalibrary::trade.base

  ## importer
  if(is.null(importing.country)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Importing countries included:", choice="All"))

  } else {

    if(is.null(keep.importer)){
      stop("Please specify whether you want to focus on the specified importing countries or exclude them (keep.importer=T/F).")
    } else{

      importers=gta_un_code_vector(importing.country, role="importing")

      if(keep.importer==T){
        trade.base=subset(trade.base, i.un %in% importers)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Importing countries included:", choice=paste(importing.country, collapse = ", ")))

      } else {
        trade.base=subset(trade.base, ! i.un %in% importers)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Importing countries included:", choice=paste("All except ", paste(importing.country, collapse = ", "), sep="")))

      }

      rm(check, importers)
    }
  }

  ## exporter
  if(is.null(exporting.country)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="Exporting countries included:", choice="All"))

  } else {

    if(is.null(keep.exporter)){
      stop("Please specify whether you want to focus on the specified exporting countries or exclude them (keep.exporter=T/F).")
    } else{

      exporters=gta_un_code_vector(exporting.country, role="exporting")

      if(keep.exporter==T){
        trade.base=subset(trade.base, a.un %in% exporters)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Exporting countries included:", choice=paste(exporting.country, collapse = ", ")))

      } else {
        trade.base=subset(trade.base, ! a.un %in% exporters)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="Exporting countries included:", choice=paste("All except ", paste(exporting.country, collapse = ", "), sep="")))

      }

      rm(check, exporters)
    }
  }

  ## hs codes
  if(is.null(hs.codes)){

    parameter.choices=rbind(parameter.choices,
                            data.frame(parameter="HS codes included:", choice="All"))

  } else {

    if(is.null(keep.hs)){
      stop("Please specify whether you want to focus on the specified HS codes or exclude them (keep.hs=T/F).")
    } else{

      # HS code check
      hs.codes <- gta_hs_code_check(codes = hs.codes)

      # Filter products
      if(keep.hs==T){
        trade.base=subset(trade.base, hs6 %in% hs.codes)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="HS codes included:", choice=paste(hs.codes, collapse = ", ")))

      } else {
        trade.base=subset(trade.base, ! hs6 %in% hs.codes)

        parameter.choices=rbind(parameter.choices,
                                data.frame(parameter="HS codes included:", choice=paste("All except ", paste(hs.codes, collapse = ", "), sep="")))

      }


    }

  }


  eval(parse(text=paste(df.name, "<<-trade.base", sep="")))
  eval(parse(text=paste(pc.name, "<<-parameter.choices", sep="")))
}
