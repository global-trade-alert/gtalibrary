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
                         colour.low = gta_colour$blue[4],
                         colour.high = gta_colour$blue[1],
                         legend.title = NULL,
                         title = NULL,
                         x.axis.name = NULL,
                         y.axis.name = NULL) {

  library("data.table")
  library("ggplot2")
  library("lubridate")


  # Load colour palette
  gta_colour_palette()

  data[,c("value.x","value.y","values")] <- data[,c(value.x, value.y, values)]

  data$value.x.labels = data$value.x
  data$value.y.labels = data$value.y
  data$value.x.breaks = data$value.x
  data$value.y.breaks = data$value.y

  if (is.numeric(data$value.x)==F) {
    data$value.x.breaks = as.numeric(data$value.x.breaks)
    i = 1
    for (h in unique(as.numeric(data$value.x))) {
      data$value.x.breaks[data$value.x.breaks==h] <-  i
      i=i+1
    }
  }

  if (is.numeric(data$value.y)==F) {
    data$value.y.breaks = as.numeric(data$value.y.breaks)
    i = 1
    for (h in unique(as.numeric(data$value.y))) {
      data$value.y.breaks[data$value.y.breaks==h] <-  i
      i=i+1
    }
  }


  plot = ggplot()+
    geom_tile(data=data, aes(x=value.x.breaks, y=value.y.breaks, fill=values), color="#FFFFFF", size=0.2)+
    scale_fill_gradient(low=colour.low, high = colour.high) +
    scale_y_continuous(breaks = unique(data$value.y.breaks), labels = unique(data$value.y.labels), sec.axis = sec_axis(~.,breaks = unique(data$value.y.breaks), name = y.axis.name, labels = unique(data$value.y.labels)))+
    scale_x_continuous(breaks = unique(data$value.x.breaks), labels = unique(data$value.x.labels))+
    ggtitle(title)+
    labs(x=x.axis.name, y=y.axis.name)+
    guides(fill = guide_legend(title=legend.title))+
    theme(line = element_line(colour = "#FFFFFF", size= 0.5, linetype = 1, lineend = "square"),
          rect = element_rect(fill = "#FFFFFF", colour="#FFFFFF",size=0, linetype = 1),
          text = element_text(family="", colour = "#333333", size=11),
          title = element_text(family="", colour= "#333333", size=11),
          axis.title.x = element_text(family="", colour = "#333333", size=11*0.8, margin = margin(t = 10, r = 0, b = 10, l = 0)),
          axis.title.y.left = element_text(family="", colour = "#333333", size=11*0.8, margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.y.right = element_text(family="", colour = "#333333", size=11*0.8, margin = margin(t = 0, r = 0, b = 0, l = 10)),
          axis.text.x.bottom = element_text(family = "", colour = "#333333", size=11*0.6, margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 45, hjust=1),
          axis.text.x.top = element_text(family = "", colour = "#333333", size=11*0.6, margin = margin(t = 0, r = 0, b = 5, l = 0), angle = 90),
          axis.text.y.left = element_text(family = "", colour = "#333333", size=11*0.6, margin = margin(t = 0, r = 5, b = 0, l = 0), angle = 0),
          axis.text.y.right = element_text(family = "", colour = "#333333", size=11*0.6, margin = margin(t = 0, r = 0, b = 0, l = 5), angle = 0),
          axis.ticks = element_line(colour=gta_colour$grey[1], size=0.2),
          axis.ticks.length = unit(0.15, "cm"),
          axis.line = element_line(colour= NULL, linetype = 1, size=0.2),
          legend.background = element_rect(fill="#FFFFFF", colour = "#FFFFFF", size=0, linetype=1),
          legend.position = "bottom",
          legend.title = element_text(vjust=0.9, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10)),
          legend.title.align = 0,
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, margin = margin(b=5)),
          legend.text.align = 0,
          legend.key = element_rect(fill="#FFFFFF", colour = "#FFFFFF", size = , linetype = 1),
          legend.key.size = unit(0.5, "cm"),
          legend.box = "horizontal",
          legend.box.just = "top",
          legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
          legend.box.spacing = unit(0.2, "cm"),
          panel.background = element_rect(fill="#f8f8f8", colour=gta_colour$grey[1], size=0.5, linetype=1),
          panel.spacing = unit(0.5, "cm"),
          panel.grid.major = element_line(colour = gta_colour$grey[3], linetype = 1, size = 0.2, lineend = "square"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = NULL, colour="#FFFFFF",size=0, linetype = 1),
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5),
          plot.subtitle = element_text(family = "", colour = "#333333", size = 11*0.8, hjust=0.5, margin=margin(t=0, r=0, b=10, l=0)),
          plot.caption = element_text(family = "", colour = "#333333", size = 11*0.6),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
          strip.background = element_rect(fill="#FFFFFF", colour=NULL, size=0.5, linetype=1),
          strip.text = element_text(family="", colour = "#333333", size=11*0.8, hjust = 0.5),
    )

  return(plot)
}
