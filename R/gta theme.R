# Roxygen documentation

#' Define plot theme settings for ggplot.
#'
#' This function defines the gta theme for ggplot. Add it at the end of a ggplot object.
#'
#' @param x.bottom.angle Define the labels angle for the bottom x-axis as integer. Default: 0.
#' @param x.bottom.align Define the alignment of the bottom x-axis labels as integer (0 to 1). Default: 0.5.
#' @param x.top.angle Define the labels angle for the top x-axis as integer. If undefined, value will be taken from bottom x-axis.
#' @param y.left.angle Define the labels angle for the left y-axis as integer. Default: 0.
#' @param y.right.angle Define the labels angle for the right y-axis as integer. If undefined, value will be taken from left y-axis.
#' @param legend.box.align If more than legend existing, define the alignment. Either "horizontal" or "vertical". Default: "horizontal".
#' @param legend.title.align Align legend title from left (0.0) to center (0.5) right (1) as integer. Default: 0.
#' @param base.size Takes in the value for the font base size as integer. All font are scaled relative to this value. Default is optimized for plots saved with default width of 21cm and relative height. Default: 11.
#' @param base.family Takes in the name of the font family. Default: "Open Sans".
#' @param base.family.bold Takes in the name of the font family for the bold face. Default: "Open Sans Bold".
#' @param aspect.ratio Takes in the value of the relative plot height as integer. Width = 1, height = aspect.ratio. Do not use for plots being saved to file. Rather define aspect ratio on save process. Default: NULL.
#' @param legend.position Define the position of the legend. Choices are "top" or "bottom".
#' @param background.color Define the background-color of the plot, default is #EEEEEE (light gray), choose #FFFFFF for white.
#'
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_theme <- function(base.size = 12,
                      aspect.ratio = NULL, background.color = "#F9F9F9",
                      font.colour = "#555555", font.colour.bold = "#555555",
                      base.family = "Open Sans", base.family.bold = "Open Sans Bold",
                      legend.box.align = "horizontal", legend.title.align = 0,
                      x.bottom.angle = 0, x.top.angle = x.bottom.angle,
                      x.bottom.align = 0.5, y.left.angle = 0,
                      y.right.angle = y.left.angle, legend.position = "top") {
    gtalibrary::gta_colour_palette()

    if (base.family != "Open Sans") {
        if (base.family.bold == "Open Sans Bold") {
            base.family.bold <- base.family
            font.color.bold <- font.colour
        } else {
            base.family.bold <- base.family.bold
            font.color.bold <- font.color.bold
        }
    }

    theme(
        line = element_line(colour = "#FFFFFF", size = 2, linetype = 1, lineend = "square"),
        rect = element_rect(fill = "#FFFFFF", colour = "#FFFFFF", size = 0, linetype = 1),
        text = element_text(family = base.family, colour = font.colour, size = base.size),
        title = element_text(family = base.family, colour = font.colour, size = base.size),
        aspect.ratio = aspect.ratio,
        axis.title.x = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x.bottom = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x.top = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.y.left = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.text.x.bottom = element_text(family = base.family, colour = font.colour, size = base.size * 0.7, margin = margin(t = 5, r = 0, b = 0, l = 0), angle = x.bottom.angle, hjust = x.bottom.align),
        axis.text.x.top = element_text(family = base.family, colour = font.colour, size = base.size * 0.7, margin = margin(t = 0, r = 0, b = 5, l = 0), angle = x.top.angle),
        axis.text.y.left = element_text(family = base.family, colour = font.colour, size = base.size * 0.7, margin = margin(t = 0, r = 5, b = 0, l = 0), angle = y.left.angle),
        axis.text.y.right = element_text(family = base.family, colour = font.colour, size = base.size * 0.7, margin = margin(t = 0, r = 0, b = 0, l = 5), angle = y.right.angle),
        axis.ticks = element_line(colour = gta_colour$grey[1], size = 0.2),
        axis.ticks.length = unit(0, "cm"),
        axis.line = element_line(colour = "#eeeeee", linetype = 1, size = 0),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", size = 0, linetype = 1),
        legend.position = legend.position,
        legend.justification = c(0, 1),
        legend.title = element_text(family = base.family, face = "bold", colour = font.colour.bold, hjust = 0, size = base.size * 0.9, margin = margin(b = 5)),
        legend.title.align = legend.title.align,
        legend.text = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, margin = margin(b = 0, r = 10, l = 0), hjust = 0),
        legend.text.align = 0,
        legend.direction = "horizontal",
        legend.key = element_rect(fill = "transparent", colour = "#cccccc", size = 0, linetype = 1),
        legend.margin = margin(l = 0),
        legend.box = legend.box.align,
        legend.box.just = 0,
        legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", size = 0.5, linetype = 1),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linetype = 1, size = 0.5, lineend = "square"),
        panel.grid.major = element_line(colour = "#eeeeee", linetype = 1, size = 0.5, lineend = "square"),
        panel.grid.minor = element_line(colour = "#eeeeee", linetype = 1, size = 0, lineend = "square"),
        plot.background = element_rect(fill = background.color, colour = "#CCCCCC", size = 0, linetype = 1),
        plot.title = element_text(family = base.family, colour = font.colour.bold, face = "bold", size = base.size, hjust = 0),
        plot.subtitle = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, hjust = 0, margin = margin(t = 0, r = 0, b = 10, l = 0)),
        plot.caption = element_text(family = base.family, colour = font.colour, size = base.size * 0.7),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "npc"),
        strip.background = element_rect(fill = "#FFFFFF", colour = gta_colour$grey[1], size = 0.5, linetype = 1),
        strip.text = element_text(family = base.family, colour = font.colour, size = base.size * 0.9, hjust = 0.5),
        complete = F
    )
}
