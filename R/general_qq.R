#' Create a QQ-Plot of Numeric Data (to Analyse the Normality Assumption)
#'
#' This function takes a dataset \code{x} - which by default will consist of all
#' numeric variables of the Tripadvisor hotel dataset - and creates a QQ-plot
#' for each of the contained columns/variables.
#' To improve the ease of understanding whether the given data is rather normal
#' distributed or not (theoretical and practical quantiles are similar)
#' additional QQ-Lines are added.
#'
#' @param x dataset to be analysed
general_qq <- function(x, name, pdf = T) {
  echoln("Start generation of QQ-plots...")
  if (pdf) {
    # Setup filename based on directory and variable name.
    file <- paste0(dir_presentation, "/qq/understand_data.qq_plot_", name, ".pdf")
    # Setup PDF formatting.
    grDevices::pdf(file, width = 25, height = 10)
    graphics::layout(matrix(1:12, nrow = 3, byrow = T))
    op <- par(mar = c(5, 4, 2, 2) + 0.2); on.exit(par(op))
  }

  # Create a QQ-Plot for each numeric variable. A QQ-Line is also added, which
  # passes through the first and third quantile and helps to better understand
  # whether the data is rather or rather not normally distributed.
  gen_qq <- sapply(names(x), function(attr) {
    if (is.numeric(x[[attr]])) {
      qqnorm(x[[attr]], main = attr, xlab = paste("p =", format(shapiro.test(x[[attr]])$p.value, digits = 4)), ylab = "", pch = 19,
             col = col.ercis.red, col.axis = col.ercis.grey, col.lab = col.ercis.grey, col.main = col.ercis.grey, col.sub = col.ercis.grey)
      qqline(x[[attr]], lwd = 2, col = col.ercis.red)
    }
  })

  if (pdf) {
    gen_qq
    graphics::layout(matrix(1))
    grDevices::dev.off()
  } else {
    return(gen_qq)
  }
  echoln("Generation of QQ-plots finished.")
}

general_qq <- compiler::cmpfun(general_qq)