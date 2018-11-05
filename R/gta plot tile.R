# Roxygen documentation

#' Create ggplot maps.
#'
#' This function facilitates map creation with ggplot.
#'
#' @param data Specify the dataframe with the relevant columns.
#' @param value.x Specify the name of the column containing the x-values.
#' @param value.y Specify the name of the column containing the y-values.
#' @param values Specify the name of the column containing the value for each country.
#' @param colour.low Specifiy a color from the gta colour palette for the low part of the gradient. Default: GTA dark blue
#' @param colour.high Specify a color from the gta colour palette for the high part of the gradient. Default: GTA light blue
#' @param legend.title Specify a title for the colour legend. Default: No title.
#' @param title Specify a title for the map plot. Default: No title.
#' @param x.axis.name Specify the x-axis label. Default: No label.
#' @param y.axis.name Specify the x-axis label. Default: No label.
#'
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_plot_tile <- function(data = NULL,
                         value.x = NULL,
                         value.y = NULL,
                         values = NULL,
                         colour.low = blue[4],
                         colour.high = blue[1],
                         legend.title = NULL,
                         title = NULL,
                         x.axis.name = NULL,
                         y.axis.name = NULL) {

  library("data.table")
  library("ggplot2")
  library(lubridate)

  data = master.0
  value.x = "year(date.implemented)"
  value.y = "month(date.implemented)"
  values = "intervention.id"
  colour.low = blue[4]
  colour.high = blue[1]
  legend.title = NULL
  title = NULL
  x.axis.name = NULL
  y.axis.name = NULL

  load("data/master_plus.Rdata")

  master.0 <- aggregate(intervention.id~year(date.implemented)+month(date.implemented), master, function(x) length(unique(x)))

  # Load map data
  gta_colour_palette()

  setnames(data, paste0(value.x),"value.x")
  setnames(data, paste0(value.y),"value.y")
  setnames(data, paste0(values),"values")


  plot = ggplot()+
    geom_tile(data=data, aes(x=value.x, y=value.y, fill=values), color="#FFFFFF", size=0.5)+
    scale_fill_gradient(low=colour.low, high = colour.high)+
    gta_theme()

  plot

  return(plot)
}
