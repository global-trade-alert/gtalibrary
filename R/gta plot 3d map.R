# Roxygen documentation

#' Create 3D world map.
#'
#' This function creates a world map with 3-dimensional values.
#'
#' @param data Specify which dataset contains the data. The dataset must have a column with UN codes, and a column for x and y values each.
#' @param countries Specify the name of the country UN codes column as string.
#' @param value.x Specify the name of the x value column as string.
#' @param value.y Specify the name of the y value column as string.
#' @param value.x.breaks Specify in how many tiles the x data should be splitted und numeric form. Default is 4.
#' @param value.y.breaks Specify in how many tiles the y data should be splitted und numeric form. Default is 4.
#' @param x.high.y.low Specify the color of the bottom right matrix corner. Applicable is any colour in the gta colour palette or a HEX colour code. Default: blue[1].
#' @param x.high.y.high Specify the color of the top right matrix corner. Applicable is any colour in the gta colour palette or a HEX colour code. Default: blue[4].
#' @param x.low.y.high Specify the color of the top left matrix corner. Applicable is any colour in the gta colour palette or a HEX colour code. Default: red[4].
#' @param x.low.y.low Specify the color of the bottom left matrix corner. Applicable is any colour in the gta colour palette or a HEX colour code. Default: red[1].
#' @param x.axis.title Specify the name of the colour matrix x-axis. Default: NULL.
#' @param y.axis.title Specify the name of the colour matrix y-axis. Default: NULL.
#' @param x.axis.labels Specify the labels along the colour matrix x-axis. If labels are provided, please specify as many labels as there are breaks, empty labels applicable. Default: NULL.
#' @param y.axis.labels Specify the labels along the colour matrix y-axis. If labels are provided, please specify as many labels as there are breaks, empty labels applicable. Default: NULL.
#' @param legend.title Specify the title of the colour matrix. It is advisable to use the title for axis explanations, as it takes up less space than two additional axis titles. Default: NULL.
#' @param save.eps Specify whether you want to save eps file or not ('TRUE' and 'FALSE'). Plot will be returned as an object and can also be saved manually. Default: FALSE.
#' @param save.png Specify whether you want to save png file or not ('TRUE' and 'FALSE'). Plot will be returned as an object and can also be saved manually. Default: FALSE.
#' @param save.path Specify the path the plots will be saved to. Default: Working directory.
#' @param save.name Specify the name of the files to be saved.
#'
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

#' @export
gta_plot_3d_map <- function(
    data = NULL,
    countries = NULL, value.x = NULL,
    value.y = NULL, value.x.breaks = 4,
    value.y.breaks = 4, x.high.y.low = gta_colour$blue[1],
    x.high.y.high = gta_colour$blue[4],x.low.y.high = gta_colour$red[4],
    x.low.y.low = gta_colour$red[1],x.axis.title = NULL,
    y.axis.title = NULL,x.axis.labels = NULL,
    y.axis.labels = NULL,legend.title = NULL,
    save.eps = FALSE,save.png = FALSE,save.path = "/",save.name = "3D-Map"
) {

  library("data.table")
  library("ggplot2")
  library("tidyverse")

  # Load map data
  gta_colour_palette()

  world <- gtalibrary::world.geo

  data[,c("UN","value.x","value.y")] <- data[,c(countries,value.x,value.y)]

  # Prepare color matrix
  gradient.top <- colorRampPalette(c(x.low.y.low, x.high.y.low))
  gradient.top <- gradient.top(value.x.breaks)
  gradient.bottom <- colorRampPalette(c(x.low.y.high, x.high.y.high))
  gradient.bottom <- gradient.bottom(value.x.breaks)

  colour.frame <- as.data.frame(matrix(nrow=value.y.breaks, ncol = value.x.breaks))

  i=1
  for (i in 1:value.x.breaks){
  eval(parse(text=paste0("temp <- colorRampPalette(c('",gradient.top[i],"','",gradient.bottom[i],"'))")))
  eval(parse(text=paste0("temp <- temp(",value.y.breaks,")")))
  colour.frame[,i] <- temp
  }

  rect.frame <- as.data.frame(matrix(nrow=value.y.breaks*value.x.breaks, ncol = 4))
  rect.frame$V4 <- seq(1,value.x.breaks*value.y.breaks, 1)
  setnames(rect.frame,"V1", "ntile.x")
  setnames(rect.frame, "V2", "ntile.y")
  setnames(rect.frame, "V3", "colour")
  setnames(rect.frame, "V4", "id")

  pos = 1
  for(x in 1:value.x.breaks){
    for(y in 1:value.y.breaks){
      rect.frame[pos,1] <- x
      rect.frame[pos,2] <- y
      rect.frame[pos,3] <- colour.frame[y,x]
      pos = pos+1
      }
  }

  colors.frame= rect.frame$colour

  p = ggplot()+
    geom_tile(data=rect.frame, aes(x=ntile.x, y=ntile.y, fill=factor(id)), color="#FFFFFF", size=0.5)+
    scale_fill_manual(values=colors.frame) +
    ggtitle(legend.title)+
    scale_x_continuous(breaks = seq(1, value.x.breaks, 1), labels = x.axis.labels, sec.axis = sec_axis(~., labels = NULL))+
    scale_y_continuous(breaks = seq(1, value.y.breaks, 1),labels = y.axis.labels, sec.axis = sec_axis(~., labels = NULL))+
    labs(x=x.axis.title,y=y.axis.title)+
    guides(fill=FALSE)+
    theme(plot.background = element_rect(fill="transparent", colour="transparent"),
          panel.background = element_rect(fill="transparent", colour="transparent"),
          axis.ticks = element_blank(),
          axis.title = element_text(size=6),
          axis.text = element_text(size=6, angle = 0),
          axis.text.x.bottom = element_text(margin = margin(t = 2)),
          axis.text.y.left = element_text(margin = margin(r = 0)),
          axis.title.x.bottom = element_text(margin = margin(t = 10)),
          axis.title.y.left = element_text(margin = margin(r = 10)),
          plot.title = element_text(hjust = 0.5, size=6),
          aspect.ratio = 0.66)
  p

  data <- data %>%
    mutate(ntile.x = ntile(value.x, value.x.breaks)) %>%
    mutate(ntile.y = ntile(value.y, value.y.breaks))

  data <- merge(data, rect.frame[,c("ntile.x","ntile.y","colour")], by=c("ntile.x","ntile.y"), all.x=T)

  # merge data with map data
  world = merge(world, data[,c("UN","colour")], by="UN", all.x=T)

  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  world$colour[is.na(world$colour) == T] <- "#eeeeee"

  col <- as.character(world$colour)
  names(col) <- as.character(world$UN)


  m = ggplot() +
    geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = factor(UN)), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    labs(x="", y="") +
    scale_fill_manual(values=col) + # Set color gradient
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank())+
    guides(fill=FALSE)

  map.plot = m + annotation_custom(ggplotGrob(p), xmin=-250, xmax=-20, ymin=-65, ymax=15)
  map.plot

  gta_plot_saver(plot = map.plot, path = save.path, name = save.name, eps = save.eps, png = save.png, aspect.ratio = 0.5)

  return(map.plot)

}
