# Roxygen documentation

#' GTA Colour Palette .
#'
#' Access any colour by calling e.g. gta_colour$green[1]. Every colour comes in 4 predefined shades (index 1 to 4 or dark to light).
#'
#' 'qualitative' contains 8 neutral colours. Shade functions can be filled with any numeric value indicating the number of shades needed.
#'
#' Applicable values are: green, liberalising, green.light, red, harmful, red.light, amber, blue, brown, turquoise, desert, grey, blue.complement, qualitative, green.shades(), red.shades(), amber.shades(), blue.shades(), turquoise.shades(), desert.shades(), brown.shades(), grey.shades().
#'
#' @export
gta_colour_palette <- function() {
    ### THE GTA standard colour palette

    # Evaluation colours
    green <- c("#2e913b", "#57ac62", "#7fc888", "#a8e3af")
    liberalising <- green
    green.light <- green[4]
    red <- c("#d42e59", "#e26888", "#f1a1b7", "#ffdbe6")
    harmful <- red
    light.red <- red[4]
    amber <- c("#f59b07", "#f8b23a", "#fcc96e", "#ffe0a1")

    # Neutral colours
    blue <- c("#134d6a", "#417895", "#6ea2c0", "#9ccdeb")
    brown <- c("#826448", "#9b8162", "#b39d7c", "#ccb997")
    turquoise <- c("#3f8c8c", "#5aa09d", "#76b3ae", "#91c7bf")
    desert <- c("#c46e2f", "#d48847", "#e3a25f", "#f2bd77")
    grey <- c("#808080", "#9a9a9a", "#b4b4b4", "#cfcfce")

    # Legacy code (blue complement = brown)
    blue.complement <- c(brown[1], brown[2], brown[3], brown[4])

    # Qualitative colors
    qualitative <- c(blue[1], blue[4], brown[1], brown[4], turquoise[1], turquoise[4], desert[1], desert[4])

    # Automatic Shades
    green.shades <- colorRampPalette(c(green[1], green[4]))
    red.shades <- colorRampPalette(c(red[1], red[4]))
    blue.shades <- colorRampPalette(c(blue[1], blue[4]))
    amber.shades <- colorRampPalette(c(amber[1], amber[4]))
    brown.shades <- colorRampPalette(c(brown[1], brown[4]))
    desert.shades <- colorRampPalette(c(desert[1], desert[4]))
    turquoise.shades <- colorRampPalette(c(turquoise[1], turquoise[4]))
    grey.shades <- colorRampPalette(c(grey[1], grey[4]))

    # colors used in gta_theme()
    text_color <- "#555555"
    line_color <- "#CCCCCC"
    background_color <- "#EEEEEE"

    # store colors in list
    gta_colour <- list(
        "text_color" = text_color,
        "line_color" = line_color,
        "background_color" = background_color,
        "green" = green,
        "liberalising" = liberalising,
        "green.light" = green.light,
        "red" = red,
        "harmful" = harmful,
        "light.red" = light.red,
        "amber" = amber,
        "blue" = blue,
        "brown" = brown,
        "turquoise" = turquoise,
        "desert" = desert,
        "grey" = grey,
        "blue.complement" = blue.complement,
        "qualitative" = qualitative,
        "green.shades" = green.shades,
        "red.shades" = red.shades,
        "blue.shades" = blue.shades,
        "amber.shades" = amber.shades,
        "brown.shades" = brown.shades,
        "desert.shades" = desert.shades,
        "turquoise.shades" = turquoise.shades,
        "grey.shades" = grey.shades
    )
    return(gta_colour)
}
