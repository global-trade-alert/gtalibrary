# Roxygen documentation

#' Define plot theme settings for ggplot.
#'
#' This function defines the gta theme for ggplot. Add it at the end of a ggplot object.
#'
#' @param x.bottom.angle Define the labels angle for the bottom x-axis as integer. Default: 0.
#' @param x.top.angle Define the labels angle for the top x-axis as integer. If undefined, value will be taken from bottom x-axis.
#' @param y.left.angle Define the labels angle for the left y-axis as integer. Default: 0.
#' @param y.right.angle Define the labels angle for the right y-axis as integer. If undefined, value will be taken from left y-axis.
#' @param legend.box.align If more than legend existing, define the alignment. Either "horizontal" or "vertical". Default: "horizontal".
#' @param legend.title.align Align legend title from left (0.0) to center (0.5) right (1) as integer. Default: 0.
#' @param base.size Takes in the value for the font base size as integer. All font are scaled relative to this value. Default is optimized for plots saved with default width of 21cm and relative height. Default: 11.
#' @param base.family Takes in the name of the font family. Default: "Open Sans".
#' @param aspect.ratio Takes in the value of the relative plot height as integer. Width = 1, height = aspect.ratio. Do not use for plots being saved to file. Rather define aspect ratio on save process. Default: NULL.
#'
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_theme <- function(base.size=11,
                      aspect.ratio = NULL,
                      font.colour = "#333333",
                      base.family = "",
                      legend.box.align = "horizontal",
                      legend.title.align = 0.5,
                      x.bottom.angle = 0,
                      x.top.angle = x.bottom.angle,
                      y.left.angle = 0,
                      y.right.angle = y.left.angle
                      ){

  theme(line = element_line(colour = "#FFFFFF", size= 0.5, linetype = 1, lineend = "straight"),
        rect = element_rect(fill = "#FFFFFF", colour="#FFFFFF",size=0, linetype = 1),
        text = element_text(family=base.family, colour = font.colour, size=base.size),
        title = element_text(family=base.family, colour= font.colour, size=base.size),
        aspect.ratio = aspect.ratio,
        axis.title.x.top = element_text(family=base.family, colour = font.colour, size=base.size*0.8, margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.x.bottom = element_text(family=base.family, colour = font.colour, size=base.size*0.8, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(family=base.family, colour = font.colour, size=base.size*0.8, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(family=base.family, colour = font.colour, size=base.size*0.8, margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.text.x.bottom = element_text(family = base.family, colour = font.colour, size=base.size*0.6, margin = margin(t = 5, r = 0, b = 0, l = 0), angle = x.bottom.angle),
        axis.text.x.top = element_text(family = base.family, colour = font.colour, size=base.size*0.6, margin = margin(t = 0, r = 0, b = 5, l = 0), angle = x.top.angle),
        axis.text.y.left = element_text(family = base.family, colour = font.colour, size=base.size*0.6, margin = margin(t = 0, r = 5, b = 0, l = 0), angle = y.left.angle),
        axis.text.y.right = element_text(family = base.family, colour = font.colour, size=base.size*0.6, margin = margin(t = 0, r = 0, b = 0, l = 5), angle = y.right.angle),
        axis.ticks = element_line(colour=grey[1], size=0.2),
        axis.ticks.length = unit(0.15, "cm"),
        axis.line = element_line(colour= grey[1], linetype = 1, size=0.5),
        legend.background = element_rect(fill="#FFFFFF", colour = "#FFFFFF", size=0, linetype=1),
        legend.position = "bottom",
        legend.title = element_text(family=base.family, colour = font.colour, size = base.size*0.8, margin = margin(b=10)),
        legend.title.align = legend.title.align,
        legend.text = element_text(family=base.family, colour = font.colour, size = base.size*0.8, margin = margin(b=5)),
        legend.text.align = 0,
        legend.key = element_rect(fill="#FFFFFF", colour = "#FFFFFF", size = , linetype = 1),
        legend.key.size = unit(0.5, "cm"),
        legend.box = legend.box.align,
        legend.box.just = "top",
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.box.spacing = unit(0.2, "cm"),
        panel.background = element_rect(fill=panel.bg, colour=grey[1], size=0.5, linetype=1),
        panel.spacing = unit(0.5, "cm"),
        panel.grid.major = element_line(colour = "#FFFFFF", linetype = 1, size = 0.5, lineend = "square"),
        panel.grid.minor = element_line(colour = "#FFFFFF", linetype = 1, size = 0.2, lineend = "square"),
        plot.background = element_rect(fill = "#FFFFFF", colour="#FFFFFF",size=0, linetype = 1),
        plot.title = element_text(family = base.family, colour = font.colour, size = base.size, hjust = 0.5),
        plot.subtitle = element_text(family = base.family, colour = font.colour, size = base.size*0.8, hjust=0.5, margin=margin(t=0, r=0, b=10, l=0)),
        plot.caption = element_text(family = base.family, colour = font.colour, size = base.size*0.6),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill="#e5e5e5", colour=grey[1], size=0.5, linetype=1),
        strip.text = element_text(family=base.family, colour = font.colour, size=base.size*0.8, hjust = 0.5),

        complete = F)
}
