
gta_colour_palette <- function(){

  ### THE GTA standard colour palette

  # Evaluation colours
  green <- c("#2e913b", "#57ac62", "#7fc888", "#a8e3af")
  liberalising <- green
  green.light <- green[4]

  assign("green", green, envir = .GlobalEnv)
  assign("liberalising", liberalising, envir = .GlobalEnv)
  assign("green.light", green.light, envir = .GlobalEnv)

  red <- c("#d42e59", "#e26888", "#f1a1b7", "#ffdbe6")
  harmful <- red
  light.red <- red[4]

  assign("red", red, envir = .GlobalEnv)
  assign("harmful", harmful, envir = .GlobalEnv)
  assign("light.red", light.red, envir = .GlobalEnv)

  amber <- c("#f59b07", "#f8b23a", "#fcc96e", "#ffe0a1")

  assign("amber", amber, envir = .GlobalEnv)

  # Neutral colours
  blue <- c("#134d6a", "#417895", "#6ea2c0", "#9ccdeb")
  brown <- c("#826448", "#9b8162", "#b39d7c", "#ccb997")
  turquoise <- c("#3f8c8c", "#5aa09d", "#76b3ae", "#91c7bf")
  desert <- c("#c46e2f", "#d48847", "#e3a25f", "#f2bd77")
  gta.grey <- c("#808080", "#9a9a9a", "#b4b4b4", "#cfcfce")

  assign("blue", blue, envir = .GlobalEnv)
  assign("brown", brown, envir = .GlobalEnv)
  assign("turquoise", turquoise, envir = .GlobalEnv)
  assign("desert", desert, envir = .GlobalEnv)
  assign("grey", gta.grey, envir = .GlobalEnv)


  # Legacy code (blue complement = brown)
  blue.complement <<- c(brown[1], brown[2], brown[3], brown[4])

  assign("blue.complement", blue.complement, envir = .GlobalEnv)

  # Qualitative colors

  qualitative <<-  c(blue[1], blue[4], brown[1], brown[4], turquoise[1], turquoise[4], desert[1], desert[4])

  assign("qualitative", qualitative, envir = .GlobalEnv)

  # General plot colours
  panel.bg <<- "#EDF2F6"

  assign("panel.bg", panel.bg, envir = .GlobalEnv)

  # Automatic Shades
  green.shades <- colorRampPalette(c(green[1], green[4]))
  red.shades <- colorRampPalette(c(red[1], red[4]))
  blue.shades <- colorRampPalette(c(blue[1], blue[4]))
  amber.shades <- colorRampPalette(c(amber[1], amber[4]))
  brown.shades <- colorRampPalette(c(brown[1], brown[4]))
  desert.shades <- colorRampPalette(c(desert[1], desert[4]))
  turquoise.shades <- colorRampPalette(c(turquoise[1], turquoise[4]))
  grey.shades <- colorRampPalette(c(gta.grey[1], gta.grey[4]))

  assign("green.shades", green.shades, envir = .GlobalEnv)
  assign("red.shades", red.shades, envir = .GlobalEnv)
  assign("blue.shades", blue.shades, envir = .GlobalEnv)
  assign("amber.shades", amber.shades, envir = .GlobalEnv)
  assign("brown.shades", brown.shades, envir = .GlobalEnv)
  assign("desert.shades", desert.shades, envir = .GlobalEnv)
  assign("turquoise.shades", turquoise.shades, envir = .GlobalEnv)
  assign("grey.shades", grey.shades, envir = .GlobalEnv)
}
