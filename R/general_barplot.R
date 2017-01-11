#' Generate Bar Plots for Dataset Analysis
#'
#' This method generates bar plots for the nominal and ordinal variables of a
#' given dataset to visualize their value distributions.
#'
#' This method is used to visualize the distribution of nominal and ordinal
#' features in a given dataset \code{data}. As not automated detection of these
#' features is conducted, the columns containing such data are given by the
#' \code{data_cols} parameter. For each of the features a bar plot is created
#' via the \code{\link{create_barplot}} method.
#' If the \code{pdf} switch is activated (\code{switch == TRUE}) the bar plots
#' will be exported to a PDF file. Here the filename is determined by the
#' \code{name} attribute. Each PDF page will then contain up to two bar plots.
#' Otherwise the bar plots will be shown on the screen.
#'
#' @param data      \code{data.frame} containing the data for which the bar
#'                  plots should be created.
#' @param data_cols \code{vector} of column names containing nominal and ordinal
#'                  data (for which the bar plots should be generated).
#' @param name      \code{character} string that is used as the filename.
#' @param pdf       \code{boolean} switch to either export the graphics to a PDF
#'                  file or print them on screen. Defaults to \code{TRUE}.
#'
#' @examples
#'  # Create vectors containing the column names of columns with oridinal or
#'  # nominal features.
#'  KaggleHouse:::data.generate_data_views()
#'  # Generate PDF bar plots.
#'  KaggleHouse:::general_barplot()
#'
general_barplot <- function(
  data = data_train,
  data_cols = c(iowa.houses.nominal.vars, iowa.houses.ordinal.vars),
  name = 'barplots',
  pdf = T
) {
  echoln("Start generation of Barplots...")

  barplots <- lapply(data_cols, function(col_name) {
    create_barplot(data[[col_name]], col_name)
  })

  if (pdf) {
    pdf_plots <- gridExtra::marrangeGrob(barplots, nrow = 2, ncol = 1)
    ggplot2::ggsave(
      paste0(dir_presentation, "/hist/understand_data.", name, ".pdf"),
      pdf_plots,
      width = 30, height = 20, units = "cm", dpi = 300
    )
  } else {
    return(barplots)
  }

  echoln("Generation of Barplots finished.")
}

#' Create Bar Plot
#'
#' This method acts as a wrapper around the \code{\link{ggplot2::ggplot}}.
#'
#' This method creates a \code{\link{ggplot2::ggplot}} for the provided
#' \code{data}. The plotting function is preconfigured to create a bar plot
#' with \code{\link{ggplot2::theme_light}} and vertically aligned
#' characteristics on the x-axis (so that all can be displayed).
#'
#' @param data      \code{vector} with the data of the column \code{col_name}.
#' @param col_name  \code{character} string with the name of the column/feature
#'   for which the bar plot is created.
#'
#' @return \code{ggplot2} bar plot for the feature \code{col.name} and data
#'         \code{data}.
#'
#' @examples
#'  KaggleHouse:::create_barplot(data_train$MSZoning, "MS Zoning")
create_barplot <- function(data, col_name) {
  data = data.frame(col = data)

  ggplot2::ggplot(data = data, ggplot2::aes(x = factor(col))) +
    ggplot2::stat_count() +
    ggplot2::xlab(col_name) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}