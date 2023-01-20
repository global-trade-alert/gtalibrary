# Roxygen documentation

#' Define plot settings for ggplot.
#'
#' This function wraps ggplot settings in a wrapper function.
#'
#' @param plot.title Define the plot title as string. Default: emtpy.
#' @param plot.subtitle Define the plot subtitle as string. Default: emtpy.
#' @param x.data.type Define the type of data fed to the x-axis. Applicable values are "continuous" or "discrete". This will affect the use of scale_x_continuous or scale_x_discrete respectively. Default: "continuous".
#' @param x.bottom.name Define the bottom x-axis name. Default: empty.
#' @param x.bottom.breaks Define the bottom x-axis breaks. Default: ggplot automatic.
#' @param x.bottom.limits Define the bottom x-axis limits. Default: ggplot automatic.
#' @param x.bottom.labels Define the bottom x-axis limits. Options are percent or ???. Default: no labels.
#' @param x.bottom.expand Expand the plot area some percentage additional to the data. Takes in a vector of type e.g. c(0.5, 0.5) for top and bottom expansion in percentage.  Default: 5 Percent.
#' @param x.top.enable Enable a secondary x-axis on the top. Default: FALSE.
#' @param x.top.transform Transform top x-axis by multiplying it with a integer. Default: No transformation = 1.
#' @param x.top.name Change name of top x-axis. If undefined, value will be taken from bottom x-axis.
#' @param x.top.breaks Change breaks of top x-axis. If undefined, value will be taken from bottom x-axis.
#' @param x.top.limits Change limits of top x-axis. If undefined, value will be taken from bottom x-axis.
#' @param x.top.labels Change labels of top x-axis. If undefined, value will be taken from bottom x-axis.
#' @param y.data.type Define the type of data fed to the y-axis. Applicable values are "continuous" or "discrete". This will affect the use of scale_y_continuous or scale_y_discrete respectively. Default: "continuous".
#' @param y.left.name Define the left y-axis name. Default: empty.
#' @param y.left.breaks Define the left y-axis breaks. Default: ggplot automatic.
#' @param y.left.limits Define the left y-axis limits. Default: ggplot automatic.
#' @param y.left.labels Define the left y-axis labels. Options are percent or ???. Default: no labels.
#' @param y.left.expand Expand the plot area some percentage additional to the data. Takes in a vector of type e.g. c(0.5, 0.5) for left and right expansion in percentage.  Default: 5 Percent.
#' @param y.right.enable Enable a secondary y-axis on the right. Default = T.
#' @param y.right.transform Transform right y-axis by multiplying it with a integer. Default: No transformation = 1.
#' @param y.right.name Change name of right y-axis. If undefined, value will be taken from left y-axis.
#' @param y.right.breaks Change breaks of right y-axis. If undefined, value will be taken from left y-axis.
#' @param y.right.limits Change limits of right y-axis. If undefined, value will be taken from left y-axis.
#' @param y.right.labels Change labels of right y-axis. If undefined, value will be taken from left y-axis.
#' @param colour.palette Define the colour scheme of colour type. Applicable values are: harmful, liberalising, qualitative, or any colour vector in the gta colour palette.
#' @param colour.labels Define the labels for the colour legend as vector. Default: ggplot automatic.
#' @param colour.legend.title Define the title of the colour legend as string. Default: ggplot automatic.
#' @param colour.legend.col Define the number of columns for the colour legend. Default: 1.
#' @param fill.palette Define the colour scheme of colour type. Applicable values are: harmful, liberalising, qualitative, or any colour vector in the gta colour palette.
#' @param fill.labels Define the labels for the fill legend as vector. Default: ggplot automatic.
#' @param fill.legend.title Define the title for the fill legend as string. Default: ggplot automatic.
#' @param fill.legend.col Define the number of columns for the fill legend. Default: 1.
#' @param flip.plot Flip the x and y axis, takes in a TRUE or FALSE value. Default: FALSE.
#' @param facet.var Takes in the column (e.g. dataframe$col) to make facets of.
#' @param facet.ncol Takes in the number of facet columns as integer. Default: 1.
#' @param facet.nrow Takes in the number of facet rows as integer. Default: 1.
#' @param x.highlight.range Define a range as a vector 'c(start, end)'
#' @param x.highlight.colour Define the color as a HEX code or use the gta_colour_palette
#' @param x.highlight.title Define the title for the highlighted x range.
#' @param x.highlight.alpha Define the transparency for the highlight from 0 to 1. Default is 0.2.
#' @param y.highlight.range Define a range as a vector 'c(start, end)'
#' @param y.highlight.colour Define the color as a HEX code or use the gta_colour_palette
#' @param y.highlight.title Define the title for the highlighted y range.
#' @param y.highlight.alpha Define the transparency for the highlight from 0 to 1. Default is 0.2.

#'
#'
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

#' @export
gta_plot_wrapper <- function(
  data=NULL,
  data.x=NULL,
  data.y=NULL,
  group=NULL,
  plot.title = NULL,
  plot.subtitle = NULL,
  x.data.type = "continuous",
  x.bottom.name = NULL,
  x.bottom.breaks = waiver(),
  x.bottom.limits = NULL,
  x.bottom.labels = waiver(),
  x.bottom.expand = c(0.02,0.02),
  x.top.enable = F,
  x.top.transform = 1,
  x.top.name = x.bottom.name,
  x.top.breaks = x.bottom.breaks,
  x.top.limits = x.bottom.limits,
  x.top.labels = waiver(),
  y.data.type = "continuous",
  y.left.name = NULL,
  y.left.breaks = waiver(),
  y.left.limits = NULL,
  y.left.labels = waiver(),
  y.left.expand = c(0.02,0.02),
  y.right.enable = T,
  y.right.transform = 1,
  y.right.name = y.left.name,
  y.right.breaks = y.left.breaks,
  y.right.limits = y.left.limits,
  y.right.labels = y.left.labels,
  colour.palette = gta_colour$qualitative,
  colour.labels = waiver(),
  colour.legend.title = waiver(),
  colour.legend.col = 1,
  colour.legend.enable = T,
  fill.palette = gta_colour$qualitative,
  fill.labels = waiver(),
  fill.legend.title = waiver(),
  fill.legend.col = 1,
  fill.legend.enable = T,
  flip.plot = F,
  facet.var = NULL,
  facet.ncol = 1,
  facet.nrow = 1,
  x.highlight.range = NULL,
  x.highlight.colour = gta_colour$grey[1],
  x.highlight.title = NULL,
  x.highlight.alpha = 0.2,
  y.highlight.range = NULL,
  y.highlight.colour = gta_colour$grey[1],
  y.highlight.title = NULL,
  y.highlight.alpha = 0.2

){
  library("scales")

  gta_colour_palette()

  if(is.character(data[,data.x])) {
    data[,data.x] <- as.factor(data[,data.x])
  }
  if(is.character(data[,data.y])) {
    data[,data.y] <- as.factor(data[,data.y])
  }

  list(

    ggtitle(plot.title,
            subtitle = plot.subtitle),


    labs(x=x.bottom.name,
         y=y.left.name),


    if (is.numeric(data[,data.y])){
      if (y.right.enable==T){
        scale_y_continuous(limits = y.left.limits,
                           breaks = y.left.breaks,
                           expand = y.left.expand,
                           labels = y.left.labels,
                           sec.axis = sec_axis(trans = eval(parse(text=paste("~.*",y.right.transform))),
                                               name = y.right.name,
                                               labels = y.right.labels,
                                               breaks = y.right.breaks))
      } else {
        scale_y_continuous(limits = y.left.limits,
                           breaks = y.left.breaks,
                           expand = y.left.expand,
                           labels = y.left.labels)}

    } else if (is.numeric(data[,data.y])==F) {
      if (y.right.enable==T){
        scale_y_discrete(limits = y.left.limits,
                         breaks = y.left.breaks,
                         expand = y.left.expand,
                         labels = y.left.labels,
                         sec.axis = sec_axis(trans = eval(parse(text=paste("~.*",y.right.transform))),
                                             name = y.right.name,
                                             labels = y.right.labels,
                                             breaks = y.right.breaks))
      } else {
        scale_y_discrete(limits = y.left.limits,
                         breaks = y.left.breaks,
                         expand = y.left.expand,
                         labels = y.left.labels)}},

    if (is.numeric(data[,data.x])) {
      if (x.top.enable==T){
        scale_x_continuous(limits = x.bottom.limits,
                           breaks = x.bottom.breaks,
                           expand = x.bottom.expand,
                           labels = x.bottom.labels,
                           sec.axis = sec_axis(trans = eval(parse(text=paste("~.*",x.top.transform))),
                                               name = x.top.name,
                                               labels = x.top.labels,
                                               breaks = x.top.breaks))
      } else {
        scale_x_continuous(limits = x.bottom.limits,
                           breaks = x.bottom.breaks,
                           expand = x.bottom.expand,
                           labels = x.bottom.labels)}

    } else if (is.numeric(data[,data.x])==F) {
      if (x.top.enable==T){
        scale_x_discrete(limits = x.bottom.limits,
                         breaks = x.bottom.breaks,
                         expand = x.bottom.expand,
                         labels = x.bottom.labels,
                         sec.axis = sec_axis(trans = eval(parse(text=paste("~.*",x.top.transform))),
                                             name = x.top.name,
                                             labels = x.top.labels,
                                             breaks = x.top.breaks))
      } else {
        scale_x_discrete(limits = x.bottom.limits,
                         breaks = x.bottom.breaks,
                         expand = x.bottom.expand,
                         labels = x.bottom.labels)}},


    if (colour.legend.enable) {
      scale_color_manual(values=colour.palette, labels=colour.labels)
    },
    if (fill.legend.enable) {
      scale_fill_manual(values=fill.palette, labels = fill.labels)
    },
    guides(color = guide_legend(title = colour.legend.title, label.hjust = 0, label.vjust = 0.5, title.position = "top", ncol = colour.legend.col, title.hjust = 0, direction = "horizontal", label.position = "right"),
           fill = guide_legend(title = fill.legend.title, label.hjust = 0, label.vjust = 0.5, title.position = "top", ncol = fill.legend.col, title.hjust = 0, direction = "horizontal", label.position = "right")),

    if(is.null(x.highlight.range)==F) {
      geom_rect(aes(xmin = x.highlight.range[1], xmax = x.highlight.range[2], ymin = -Inf, ymax = Inf), fill=x.highlight.colour, colour="transparent", alpha = x.highlight.alpha)
    },
    if(is.null(x.highlight.title)==F) {
      geom_text(aes(x=x.highlight.range[1], y=Inf, label=x.highlight.title), hjust=-0.1, vjust=1.6, color = x.highlight.colour, lineheight = 1)
    },
    if(is.null(y.highlight.range)==F) {
      geom_rect(aes(ymin = y.highlight.range[1], ymax = y.highlight.range[2], xmin = -Inf, xmax = Inf), fill=y.highlight.colour, colour="transparent", alpha = y.highlight.alpha)
    },
    if(is.null(y.highlight.title)==F) {
      geom_text(aes(y=y.highlight.range[2], x=-Inf, label=y.highlight.title), hjust=-0.1, vjust=1.5, color = y.highlight.colour, lineheight = 1)
    },

    if (flip.plot == T) {coord_flip()},


    if (is.null(facet.var)==F){facet_wrap(facets=vars(eval(facet.var)), ncol = facet.ncol, nrow = facet.nrow)}
  )}
