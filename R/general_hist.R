#' Create Histogram of Numeric Data (to visualize Distribution)
#'
#' The function creates histograms for all columns/variables with numeric
#' variables in the given dataset \code{x} (at least this is the default
#' behaviour) and saves them to a PDF file.
#' Creating the histograms helps to get a better understanding of the
#' distribution of the values. As the visualized distributions might ressemble
#' the normal distribution, the p-value is plotted below the histograms as an
#' additional indicator for normality.
#'
#' @param x dataset to be analysed
general_hist <- function(x, name, pdf = T) {
  echoln("Start generation of histograms...")
  if (pdf) {
    file <- paste0(dir_presentation, "/hist/understand_data.hist_", name, ".pdf")
    # Setup PDF formatting.
    grDevices::pdf(file, width = 20, height = 10)
    # For each column/variable in the dataset create a histogram plus an
    # additional p-value as a second indicator/tester for normality.
    graphics::layout(matrix(1:12, nrow = 3, byrow = T))
  }

  gen_hist <- sapply(names(x), function(attr) {
    if (is.numeric(x[[attr]]))
      hist(x[[attr]], main = attr, xlab = paste("p =", format(shapiro.test(x[[attr]])$p.value, digits = 4)),
           col = col.ercis.red, col.axis = col.ercis.grey, col.lab = col.ercis.grey, col.main = col.ercis.grey, col.sub = col.ercis.grey)
  })

  if (pdf) {
    gen_hist
    graphics::layout(matrix(1))
    grDevices::dev.off()
  } else {
    return(gen_hist)
  }
  echoln("Generation of histograms finished.")
}

general_hist <- compiler::cmpfun(general_hist)