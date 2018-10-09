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
#'
#'
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_plot_wrapper <- function(plot.title = NULL,
                        plot.subtitle = NULL,
                        x.data.type = "continuous",
                        x.bottom.name = NULL,
                        x.bottom.breaks = waiver(),
                        x.bottom.limits = c(NA,NA),
                        x.bottom.labels = waiver(),
                        x.bottom.expand = c(0.05,0.05),
                        x.top.enable = F,
                        x.top.transform = 1,
                        x.top.name = x.bottom.name,
                        x.top.breaks = x.bottom.breaks,
                        x.top.limits = x.bottom.limits,
                        x.top.labels = waiver(),
                        y.data.type = "continuous",
                        y.left.name = NULL,
                        y.left.breaks = waiver(),
                        y.left.limits = c(NA,NA),
                        y.left.labels = waiver(),
                        y.left.expand = c(0.05,0.05),
                        y.right.enable = T,
                        y.right.transform = 1,
                        y.right.name = y.left.name,
                        y.right.breaks = y.left.breaks,
                        y.right.limits = y.left.limits,
                        y.right.labels = y.left.labels,
                        colour.palette = qualitative,
                        colour.labels = waiver(),
                        colour.legend.title = waiver(),
                        colour.legend.col = 1,
                        fill.palette = qualitative,
                        fill.labels = waiver(),
                        fill.legend.title = waiver(),
                        fill.legend.col = 1,
                        flip.plot = F,
                        facet.var = NULL,
                        facet.ncol = 1,
                        facet.nrow = 1
                        ){
  gta_colour_palette()

  list(

  ggtitle(plot.title,
          subtitle = plot.subtitle),


  labs(x=x.bottom.name,
       y=y.left.name),

if (tolower(y.data.type) == "continuous"){
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

  } else if (tolower(y.data.type) == "discrete") {
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


  if ((tolower(x.data.type) %in% c("continuous", "discrete")) == F){
    stop("Please specify data datatype of the x-axis data, 'continuous' or 'discrete'")

    } else {

    if (tolower(x.data.type) == "continuous") {
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

      } else if (tolower(x.data.type) == "discrete") {
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
                             labels = x.bottom.labels)}}},


  scale_color_manual(values=colour.palette, labels=colour.labels),


  scale_fill_manual(values=fill.palette, labels = fill.labels),

  guides(color = guide_legend(title = colour.legend.title, title.position = "top", ncol = colour.legend.col),
         fill = guide_legend(title = fill.legend.title, title.position = "top", ncol = fill.legend.col)),


  if (flip.plot == T) {coord_flip()},


  if (is.null(facet.var)==F){facet_wrap(facets=vars(eval(facet.var)), ncol = facet.ncol, nrow = facet.nrow)}
)}
