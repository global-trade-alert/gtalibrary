# Roxygen documentation

#' Complete theme for GTA Plots
#'
#' This function defines the GTA theme for ggplot. Add it at the end of a ggplot object.
#' If you want to overwrite certain elements, simply add \code{theme()} to your plot.
#' @usage
#' gta_theme(
#'    base_size = 12,
#'    base_family = "Open Sans",
#'    base_line_size = base_size / 22,
#'    base_rect_size = base_size / 22,
#'    aspect_ratio = NULL
#' )
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @param aspect_ratio Here, you can define the aspect ratio of the panel if needed (default is NULL)
#' @examples
#' # Create a plot with gta_theme()
#' df |>
#'     ggplot(aes(x = x, y = y, color = color)) +
#'     geom_line() +
#'     gta_theme() + # add the base theme to your plot
#'     theme(axis.text.x = element_text(angle = 45, color = "red", size = 20)) # if you need to deviate from the theme, simply add changes within a new theme() element
#' @import ggplot2
#' @export
gta_theme <- function(base_size = 12, base_family = "sans", base_line_size = base_size / 22, base_rect_size = base_size / 22, aspect_ratio = NULL) {
    # margins are defined relative to the text size (base_size) of the plot
    # define complete theme

    # check font availability on system
    font_installed <- systemfonts::system_fonts() |>
        dplyr::filter(family == base_family)

    # check temporarily loaded fonts (eg. via sysfonts::font_add_google())
    font_temp <- which(sysfonts::font_families() == base_family)

    # if font is not loaded, print warning/info message. ggplot resorts to base font (sans)
    if (nrow(font_installed) == 0 & length(font_temp) == 0) {
        out <- glue::glue("The specified font family {base_family} is not installed on your system")
        info <- glue::glue("If you do not wish to install {base_family}, add the following commands to your script:
        sysfonts::add_font_google('{base_family}')
        showtext::showtext_auto()")
        cli::cli_alert_warning(out)
        cli::cli_alert_info(info)
    }

    theme(
        # set base line, rect and text --> These elements are inherited from all other elements
        line = element_line(colour = "#CCCCCC", linewidth = base_line_size, linetype = 1, lineend = "butt"),
        rect = element_rect(fill = "#EEEEEE", color = "#CCCCCC", linewidth = base_rect_size, linetype = 1),
        text = element_text(family = base_family, color = "#555555", size = base_size),

        # specification of theme elements
        axis.title = element_text(size = rel(.85), margin = margin(b = base_size / 4)),
        axis.title.x = element_text(margin = margin(t = base_size / 4), vjust = 1),
        axis.title.x.top = element_text(margin = margin(b = base_size / 4), vjust = 0),
        axis.title.y = element_text(angle = 90, margin = margin(r = base_size / 4), vjust = 1),
        axis.title.y.right = element_text(angle = -90, margin = margin(l = base_size / 4), vjust = 0),
        axis.line = element_blank(),
        axis.text = element_text(size = rel(0.7)),
        axis.text.x = element_text(margin = margin(t = base_size / 4), vjust = 1),
        axis.text.x.top = element_text(margin = margin(b = base_size / 4), vjust = 0),
        axis.text.y = element_text(margin = margin(r = base_size / 4), hjust = 1),
        axis.text.y.right = element_text(margin = margin(l = base_size / 4), hjust = 0),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(3, "pt"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.spacing = unit(base_size, "pt"),
        legend.margin = margin(0, 0, 0, 0, "cm"),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1.2 * base_size, "pt"),
        legend.text = element_text(size = rel(0.85)),
        legend.title = element_text(size = rel(.85), hjust = 0, vjust = .5),
        legend.position = "bottom",
        legend.justification = "center",
        legend.box.margin = margin(0, 0, 0, 0),
        legend.box.spacing = unit(base_size / 2, "pt"),
        legend.box.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = "#EEEEEE"),
        panel.grid.minor = element_line(color = "#EEEEEE"),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(base_size * .8, "pt"),
        panel.ontop = FALSE,
        strip.background = element_rect(fill = "white", color = "#CCCCCC"),
        strip.clip = "inherit",
        strip.text = element_text(size = rel(0.9), margin = margin(0.4 * base_size, 0.4 * base_size, 0.4 * base_size, 0.4 * base_size)),
        strip.text.y = element_text(angle = -90),
        strip.text.y.left = element_text(angle = 90),
        strip.placement = "inside",
        strip.switch.pad.grid = unit(base_size / 4, "pt"),
        strip.switch.pad.wrap = unit(base_size / 4, "pt"),
        plot.background = element_rect(),
        plot.title = element_text(face = "bold", hjust = 0, vjust = 1, margin = margin(b = base_size / 2)),
        plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(b = base_size / 2)),
        plot.caption = element_text(size = rel(0.7), hjust = 1, vjust = 1, margin = margin(t = base_size / 2)),
        plot.margin = margin(base_size, base_size, base_size, base_size),
        aspect.ratio = aspect_ratio,
        complete = TRUE # specifies that the theme is complete. All non-present arguments inherit from blank
    )
}
