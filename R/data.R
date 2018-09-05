# Contains descriptions for internal package dataframes

#' @title CPC Names
#' @description Dataframe containing CPC Names, sector codes and levels
#' @format A data frame with 400 rows and 3 variables:
#' \describe{
#'   \item{cpc}}{CPC sector codes, 2nd and 3rd level mixed, numeric}
#'   \item{cpc.digit.level}{CPC sector level indicator, numeric}
#'   \item{cpc.name}{Name of CPC sector, character}
#'}
#' @source Global Trade Alert
"cpc.names"

#' @title HS Names
#' @description Dataframe containing HS12 Codes and their names
#' @format A data frame with 5205 rows and 2 variables:
#' \describe{
#'   \item{HS12code}}{HS12 codes, level 6, numeric}
#'   \item{hs.name}{HS12 code names, character}
#'}
#' @source Global Trade Alert
"hs.names"

#' @title CPC to HS
#' @description Dataframe containing CPC and HS12 code correspondance
#' @format A data frame with 5205 rows and 2 variables:
#' \describe{
#'   \item{cpc}}{3rd level CPC code, numeric}
#'   \item{hs}{Corresponding HS12 code, numeric}
#'}
#' @source Global Trade Alert
"cpc.to.hs"
