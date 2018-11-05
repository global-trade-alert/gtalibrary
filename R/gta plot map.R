# Roxygen documentation

#' Create ggplot maps.
#'
#' This function facilitates map creation with ggplot.
#'
#' @param data Specify the dataframe with the relevant columns.
#' @param countries Specify the name of the column containing the countries.
#' @param value Specify the name of the column containing the value for each country.
#' @param colour.low Specifiy a color from the gta colour palette for the low part of the gradient. Default: GTA dark blue
#' @param colour.high Specify a color from the gta colour palette for the high part of the gradient. Default: GTA light blue
#' @param colour.breaks Specify the breaks for the colour gradient. Default: ggplot automatic.
#' @param legend.title Specify a title for the colour legend. Default: No title.
#' @param title Specify a title for the map plot. Default: No title.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_plot_map <- function(data = NULL,
                         countries = NULL,
                         value = NULL,
                         colour.low = blue[4],
                         colour.high = blue[1],
                         colour.breaks = waiver(),
                         legend.title = NULL,
                         title = NULL) {

  library("data.table")
  library("ggplot2")

  # Load map data
  gta_colour_palette()

  world <- gtalibrary::world.geo

  setnames(data, paste0(countries),"UN")
  setnames(data, paste0(value),"value")

  # merge data with map data
  world = merge(world, data[,c("UN","value")], by="UN", all.x=T)

  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  world$value[is.na(world$value) == T] <- 0

  plot = ggplot() +
    geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    ggtitle(title) +
    labs(x="", y="") +
    scale_fill_gradient(low = colour.low, high = colour.high, breaks=colour.breaks, position="bottom") + # Set color gradient
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom") +
    guides(fill=guide_legend(title=legend.title, label.position = "top"),
           ymax=guide_legend(titel="size"))

  return(plot)
}
