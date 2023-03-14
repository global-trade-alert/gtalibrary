#' hs_vintage conversion matrix
#'
#' hs_vintages contains the conversion matrix for converting from HS 1992 / 1996 / 2007 /2017 and 2022 to HS2012.
#' The underlying conversion matrices are obtained from https://unstats.un.org/unsd/classifications/Econ
#'
#' !Only use this data set to convert to HS 2012 codes! Conversion between other HS years are incomplete as the bilateral
#' conversion matrices donwloaded from unstats were joined with HS2012 as the key value
#'
#' This data set is used in the function \code{gta_hs_vintage_converter()}
"hs_vintages"

#' coordinates for countries
#'
#' world_geo is a list which contains:
#' 1. the coordinates for all countries to plot a worldmap. Additionally, information on the region, sub_region and intermediate_region
#' as retreived from https://unstats.un.org/unsd/methodology/m49/overview/ is provided. This allows for easily plotting only certain regions
#' of the world
#' 2. The latitude and longtitude for the center of the biggest landmass for each country. This data is helpful if eg. labels should be plotted
#' in the center of a country.
"world_geo"
