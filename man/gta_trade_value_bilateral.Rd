% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gta trade value bilateral.R
\name{gta_trade_value_bilateral}
\alias{gta_trade_value_bilateral}
\title{GTA bilateral trade value}
\usage{
gta_trade_value_bilateral(
  importing.country = NULL,
  keep.importer = NULL,
  exporting.country = NULL,
  keep.exporter = NULL,
  cpc.sectors = NULL,
  keep.cpc = TRUE,
  hs.codes = NULL,
  keep.hs = TRUE,
  trade.data = "base",
  trade.data.path = "data/support tables/Goods support table for gtalibrary.Rdata",
  df.name = "trade.base.bilateral",
  pc.name = "parameter.choice.trade.base"
)
}
\arguments{
\item{importing.country}{Specify the importing countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.}

\item{keep.importer}{Specify whether to focus on ('TRUE') or exclude ('FALSE') trade with the stated importing country.}

\item{exporting.country}{Specify the exporting countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.}

\item{keep.exporter}{Specify whether to focus on ('TRUE') or exclude ('FALSE') trade with the stated exporting country.}

\item{cpc.sectors}{Provide a vector of CPC codes that you are interested in (version 2.1, 3-digit level).}

\item{keep.cpc}{Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.}

\item{hs.codes}{Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).}

\item{keep.hs}{Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.}

\item{trade.data}{Choose the trade data underlying these calulations. Choices are individual years between 2007 and 2020, the GTA base period data ('base', averages for 2005-2007) as well as moving trade data as a function of coverage year ('prior year' and 'current year'). Default is 'base'.}

\item{trade.data.path}{Set path of trade data file (default is 'data/support tables/Goods support table for gtalibrary.Rdata').}

\item{df.name}{Set the name of the generated result data frame. Default is trade.base.}

\item{pc.name}{Set the name of the generated parameter choice data frame. Default is parameter.choice.trade.base.}
}
\value{
Output is two data frames. First data frame includes the trade values for the given importer-exporter-product combinations. Second data frame states parameter choices.
}
\description{
Computes a data frame with the bilateral trade values in the GTA base period for the given importer-exporter-product combinations.
}
\references{
www.globaltradealert.org
}
\author{
Global Trade Alert
}
