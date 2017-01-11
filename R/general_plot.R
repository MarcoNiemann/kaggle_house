#' Create Scatterplot of Data (to visualize Correlations)
#'
#' The function creates a scatterplot of the complete dataset given by the
#' variable and saves it in a PDF file.
#' The lower panel makes use of \code{panel.smooth} to create the usual
#' scatterplots for pairs of two variables but also adds some kind of a
#' 'regression line' to the plot to increase the understandability.
#' The upper panel uses a custom panel that shows of the numeric correlation
#' values (highlighted with different font sizes and colors).
#'
#' @param x Dataset to be analysed.
#' @param name Name that will be used in case a PDF will be produced.
#' @param threshold The minimum correlation that two features must have to be
#'                  considered further. If a feature does not have a correlation
#'                  greater than the threshold with at least two other features
#'                  it will be excluded from the correlation plot. Defaults to
#'                  a value of 0.7.
#' @param pdf Switch variable that decides whether the plot will be output to
#'            PDF or not. Defaults to PDF output.
#' @param o \code{Boolean} switch to decide whether the method is allowed to
#'          print progress output to \code{stdout}. Defaults to \code{true}.
#' @param cor.met Methodology used to compute the correlation. Can be "pearson",
#'                "spearman" oder "kendall". Defaults to "pearson".
general_plot <- function(x,
                         name,
                         threshold = 0.7,
                         pdf = T,
                         o = T,
                         cor.met = "pearson") {
  if(o) {
    echoln("Start generation of Scatterplot...")
  }
  if (pdf) {
    file <- paste0(dir_presentation, "/plot/understand_data.scatterplot_", name, ".pdf")
    grDevices::pdf(file, width = 20, height = 20)
  }
#   gen_plot <- pairs(
#     x,
#     panel = function(...) panel.smooth(..., col.smooth = col.ercis.orange),
#     upper.panel = util.panel.cor,
#     col = col.ercis.red, col.axis = col.ercis.grey,
#     col.lab = col.ercis.grey, col.main = col.ercis.grey,
#     col.sub = col.ercis.grey, lwd = 2, pch = 19
#   )
  correlations <- cor(x[,-1], method = cor.met)
  rows = apply(correlations, 1, function(row){sum(abs(row) > threshold) > 1})
  correlations <- correlations[rows ,rows]
  if (pdf) {
    corrplot::corrplot(correlations, method = "square")
    grDevices::dev.off()
  } else {
    corrplot::corrplot(correlations, method = "square")
  }
  if(o) {
    echoln("Generation of Scatterplot finished.")
  }
}

general_plot <- compiler::cmpfun(general_plot)