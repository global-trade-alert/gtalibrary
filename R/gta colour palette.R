# Roxygen documentation

#' GTA Colour Palette .
#'
#' Access any colour by calling e.g. gta_colour$green[1]. Every colour comes in 4 predefined shades (index 1 to 4 or dark to light).
#'
#' 'qualitative' contains 8 neutral colours. Shade functions can be filled with any numeric value indicating the number of shades needed.
#'
#' Applicable values are: green, liberalising, green.light, red, harmful, red.light, amber, blue, brown, turquoise, desert, grey, blue.complement, qualitative, green.shades(), red.shades(), amber.shades(), blue.shades(), turquoise.shades(), desert.shades(), brown.shades(), grey.shades().
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_colour_palette <- function(){

  ### THE GTA standard colour palette
  
  # Evaluation colours
  green <- c("#1E752A", "#2E913B", "#45AC52", "#67D274", "#A5EAAE")
  liberalising <- green
  green.light <- green[5]

  red <- c("#B6242E", "#D42E3A", "#EC4A55", "#FF737D", "#FFB4B9")
  harmful <- red
  light.red <- red[5]
  
  amber <- c("#C87F0F", "#EB9D00",  "#FFBF00", "#FFD75E", "#FFE79F")

  # Neutral colours
  blue <- c("#0F599D", "#1874CD", "#3B97ED", "#7DC3F8", "#B6DFFF")
  brown <- c("#8C7448", "#AF935F", "#D1B278", "#EACD9C", "#FCE5BF")
  turquoise <- c("#2B7379", "#379293", "#52B6B7", "#8BD8D9", "#BDEDED")
  desert <- c("#925047", "#AF6055", "#C97063", "#DE9589", "#F4BBB3")
  grey <- c("#606060", "#797979", "#9A9A9A", "#BBBBBB", "#DBDBDB")

  # Legacy code (blue complement = brown)
  blue.complement <- c(brown[1], brown[2], brown[3], brown[4], brown[5])

  # Qualitative colors
  qualitative <- c(blue[2], blue[4], turquoise[2], turquoise[4], brown[2], brown[4], desert[2], desert[4])
  
  # qualitative many
  qualitative <- c(blue[2], blue[4], turquoise[2], turquoise[4], brown[2], brown[4], desert[2], desert[4], grey[2], grey[4], blue[1], blue[5], turquoise[1], turquoise[5], brown[1], brown[5], desert[1], desert[5], grey[1], grey[5], blue[3], turquoise[3], brown[3], desert[3], grey[3])

  # General plot colours
  panel.bg <- "#EBF4F6"

  # Automatic Shades
  green.shades <- colorRampPalette(c(green[1], green[5]))
  red.shades <- colorRampPalette(c(red[1], red[5]))
  blue.shades <- colorRampPalette(c(blue[1], blue[5]))
  amber.shades <- colorRampPalette(c(amber[1], amber[5]))
  brown.shades <- colorRampPalette(c(brown[1], brown[5]))
  desert.shades <- colorRampPalette(c(desert[1], desert[5]))
  turquoise.shades <- colorRampPalette(c(turquoise[1], turquoise[5]))
  grey.shades <- colorRampPalette(c(grey[1], grey[5]))


  gta_colour = list("green" = green,
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
                    "panel.bg" = panel.bg,
                    "green.shades" = green.shades,
                    "red.shades" = red.shades,
                    "blue.shades" = blue.shades,
                    "amber.shades" = amber.shades,
                    "brown.shades" = brown.shades,
                    "desert.shades" = desert.shades,
                    "turquoise.shades" = turquoise.shades,
                    "grey.shades" = grey.shades)

  gta_colour <<- gta_colour
}
