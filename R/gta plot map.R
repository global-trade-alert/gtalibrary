# Roxygen documentation

#' Create ggplot maps.
#'
#' This function facilitates map creation with ggplot.
#'
#' @param data Specify the dataframe with the relevant columns.
#' @param countries Specify the name of the column containing the countries.
#' @param value Specify the name of the column containing the value for each country.
#' @param marked.country Specify a single country which shall be coloured differenty.
#' @param marked.colour Specify the colour the marked.country shall be coloured with. Default: gta_colour$green[2]
#' @param colour.low Specifiy a color from the gta colour palette for the low part of the gradient. Default: GTA dark blue
#' @param colour.high Specify a color from the gta colour palette for the high part of the gradient. Default: GTA light blue
#' @param range.split Specify the number of parts the value range should be split to. Either as integer (range will be automatically divided and sequenced) or as sequence (e.g. seq(200,1000,200)). Default: ggplot automatic.
#' @param legend.title Specify a title for the colour legend. Default: No title.
#' @param legend.labels Specify the legend labels as list. Default: ggplot automatic.
#' @param title Specify a title for the map plot. Default: No title.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_plot_map <- function(data = NULL,
                         countries = NULL,
                         value = NULL,
                         marked.country = NULL,
                         marked.colour = gta_colour$green[2],
                         colour.low = gta_colour$blue[4],
                         colour.high = gta_colour$blue[1],
                         range.split = waiver(),
                         legend.title = NULL,
                         legend.labels = waiver(),
                         title = NULL) {

  library("data.table")
  library("ggplot2")

  # Load map data
  gta_colour_palette()

  world <- gtalibrary::world.geo

  data[,c("UN","value")] <- data[,c(countries,value)]
  data$UN <- gta_un_code_vector(data$UN)

  # merge data with map data
  world = merge(world, data[,c("UN","value")], by="UN", all.x=T)

  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  # world$value[is.na(world$value) == T] <- 0

  if (length(range.split)==1){
  eval(parse(text=paste("range.split <- seq(",min(world$value, na.rm=T),",",max(world$value, na.rm=T),",",(max(world$value, na.rm=T)-min(world$value, na.rm=T))/(range.split-1),")",sep="")))
    range.split <- range.split[1:length(range.split)]
    if(length(legend.labels)>0) {
      if(length(range.split)!=length(legend.labels)) {
        stop("Please make sure that range.split and legend.labels have the same number of elements.")
      }
    } else {
      legend.labels <- paste(range.split, sep=",")
    }
  }
  if (length(range.split)>1) {
    range.split <- range.split[1:length(range.split)]
    if(length(legend.labels)>0) {
      if(length(range.split)!=length(legend.labels)) {
        stop("Please make sure that range.split and legend.labels have the same number of elements.")
      }
    } else {
      legend.labels <- paste(range.split, sep=",")
    }
  }

  if (is.null(marked.country)){

  plot = ggplot() +
    geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    ggtitle(title) +
    labs(x="", y="") +
    scale_fill_gradient(low = colour.low, high = colour.high, breaks=range.split, position="bottom", labels=legend.labels, na.value = "#CCCCCC") + # Set color gradient
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10)),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
          legend.text.align = 0

    ) +
    guides(fill=guide_legend(title=legend.title, label.position = "top"),
           ymax=guide_legend(titel="size"))

  plot

  }

  if(is.null(marked.country)==F) {

    marked.country <- gta_un_code_vector(marked.country)

    plot = ggplot() +
      geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white", ) +
      geom_polygon(data=subset(world, UN == marked.country), aes(x=long, y=lat, group = group), fill=marked.colour, size = 0.2, colour = "white") +
      coord_fixed() + # Important to fix world map proportions
      ggtitle(title) +
      labs(x="", y="") +
      scale_fill_gradient(low = colour.low, high = colour.high, breaks=range.split, position="bottom", labels=legend.labels, na.value = "#CCCCCC") + # Set color gradient
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
            legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10)),
            legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
            legend.text.align = 0

      ) +
      guides(fill=guide_legend(title=legend.title, label.position = "top"),
             ymax=guide_legend(titel="size"))


    plot


  }

  return(plot)
}
