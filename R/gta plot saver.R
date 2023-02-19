# Roxygen documentation

#' Save png and eps plots together more easily.
#'
#' This function saves ggplot objects as png and eps. Optimized for width of A4 paper, but can be changed accordingly.
#'
#' @param plot Specify the plot to save.
#' @param path Specify the folder to save the plot into as string. Default: working directory.
#' @param name Input the name of the plot as string. Default: "plot".
#' @param width Takes in a width of the plot object. Default is 21cm, font sizes are optimized for this size. If you change the plot width, you may have to change the gta_theme(base.size) accordingly.
#' @param width Takes in a specific plot height. If unspecified, height will be calculated through aspect.ratio.
#' @param png Set png output to FALSE. Default: TRUE.
#' @param eps Set eps output to FALSE. Default: TRUE.
#' @param eps Set cairo_ps output to TRUE if you want to generate an eps file with transparency levels. Default: FALSE.
#' @param aspect.ratio Define the aspect ratio of the plot as integer. Width = 1, height = 1*aspect.ratio. If height is defined, aspect.ratio does have no effect.
#'
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


#' @export
gta_plot_saver <- function(plot = NULL,
                           path = "/",
                           name = "plot",
                           width = 21,
                           height = NULL,
                           png = T,
                           eps = F,
                           pdf = F,
                           jpg = F,
                           cairo_ps = F,
                           aspect.ratio = 21 / 29.7) {
  if (is.null(height)) {
    height <- width * aspect.ratio
  } else {
    height <- height
  }

  if (png == T) {
    ggsave(
      filename = paste0(name, ".png"),
      plot = plot,
      device = "png",
      path = path,
      dpi = 600,
      width = width,
      height = height,
      units = "cm"
    )
  }
  if (jpg == T) {
    ggsave(
      filename = paste0(name, ".jpg"),
      plot = plot,
      device = "jpg",
      path = path,
      dpi = 600,
      width = width,
      height = height,
      units = "cm"
    )
  }
  if (eps == T) {
    ggsave(
      filename = paste0(name, ".eps"),
      plot = plot,
      device = "eps",
      path = path,
      width = width,
      height = height,
      units = "cm"
    )
  }
  if (pdf == T) {
    ggsave(
      filename = paste0(name, ".pdf"),
      plot = plot,
      device = grDevices::cairo_pdf,
      path = path,
      width = width,
      height = height,
      units = "cm"
    )
  }
  if (cairo_ps == T) {
    ggsave(
      filename = paste0(name, ".eps"),
      plot = plot,
      device = grDevices::cairo_ps,
      path = path,
      width = width,
      height = height,
      units = "cm"
    )
  }
}
devtools::install_github("global-trade-alert/gtalibrary@hs_functions")


gtalibrary::gta_check
