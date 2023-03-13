#' world_geo
#'
#' world_geo contains the coordinates of the world maps retreived from \code{ggplot2::map_data("world")}.
#' The country names are adjusted to match the country names used by GTA (see \code{gtalibrary::country.correspondence})
#' @format The Dataset further contains the following information as retreived from https://unstats.un.org/unsd/methodology/m49/overview/
#' \describe{
#' \item{region}{main region (Americas, Europe, Asia etc...)}
#' \item{sub_region}{Latin America, Western Europe, Eastern Europe etc...}
#' \item{intermediate_retion}{}
#' \item{Breakdown between region and sub_region (only defined for some countries)}{Southern Africa, Western Africa, etc...}
#' \item{is_detached}{Logical Value which indicates whether the region is detached from the mainland (often for overseas territories) --> If you wish to generate
#' a plot of eg. Europe only, set is_detached to false to remove these regions (Examples include: Madeira/Azores --> Portugal, Guernsey --> UK, Christmas Island --> Australia)}
#' }
"world_geo"
