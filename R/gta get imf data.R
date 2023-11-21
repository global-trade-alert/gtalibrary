# Roxygen documentation

#' A wrapper for the IMFData function
#'
#'
#' @param start.date Provide the period start date in R format ('YYYY-mm-dd').
#' @param end.date Provide the period end date in R format ('YYYY-mm-dd').
#' @param fx.frequency Provide time series frequency e.g. ('D','M','Q','A')
#' @param series What time series do you want? Options: 'fx' for USD exchange rates.
#' @param countries What countries do you want? Permissiable options are 'all' plus ISO2 format included inside the "imf.symbol" vector.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#'
gta_get_imf_data <- function(start.date=NULL,
                             end.date=NULL,
                             frequency=NULL,
                             series=NULL,
                             countries=NULL) {

  library(IMFData)

  imf.cur=data.frame(currency=c("GBP", "PLN", "EUR", "SEK", "DKK", "HUF", "BGN", "CZK", "NOK", "CHF", "HRK", "USD", "RON", "SKK", "MKD", "ISK", "JPY", "LTL", "LVL", "MTL", "NZD"),
                     imf.symbol=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT", "NZ"),
                     stringsAsFactors = F)

  checkquery = FALSE

  if(any(is.null(start.date),is.null(end.date),is.null(frequency), is.null(series))){
    stop("Please specify all parameters.")
  }

  if(series=="fx"){
    query.series='ENDA_XDC_USD_RATE'
    database.id <- 'IFS'
  } else {
    query.series <- series
    database.id <- 'IFS'
  }

  if(identical(countries, "all")){
    query.countries=imf.cur$imf.symbol
  } else {
    query.countries=subset(imf.cur, imf.symbol %in% countries)$imf.symbol
  }

  queryfilter <- list(CL_FREQ=frequency,
                      CL_AREA_IFS=query.countries,
                      CL_INDICATOR_IFS =query.series)

  imf.data <- CompactDataMethod(database.id,
                               queryfilter,
                               start.date,
                               end.date,
                               checkquery, tidy = T)

  if(series=="fx"){
    imf.data=imf.data[,c(1,2,4)]
    names(imf.data)=c("date","lcu.per.usd", "imf.symbol")
    imf.data=merge(imf.data, imf.cur, by="imf.symbol",all.x=T)
    imf.data$imf.symbol=NULL
  } else {
    imf.data=imf.data[,c(1,2,4)]
    names(imf.data)=c("date", series, "imf.symbol")
    imf.data=merge(imf.data, imf.cur, by="imf.symbol",all.x=T)
    imf.data$imf.symbol=NULL
  }

  return(imf.data)

}
