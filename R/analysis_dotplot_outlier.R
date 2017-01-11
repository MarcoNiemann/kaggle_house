#' Generate Outlier Point Plots
#'
#' This function generates outlier point plots (for a description see
#' slide 117 of the DA1 lecture of WT 2015/16).
#'
#' The method generates dotplots as univariate outlier detectors for all columns
#' of a given \code{data.frame} \code{df}. So via the function
#' \code{\link{analysis.dotplot}} it computes the distribution of values in each
#' given column via \code{\link{table}}. Based on the distribution the method
#' \code{\link{cpp_valueOccurrencesToPoints}} will generate visualizable points
#' representing each occurence in the form
#' \eqn{(x_i, y_{i,j})}{(x[i], y[i,j])}.
#'
#' @param df \code{data.frame} with the data that should be analyzed for
#'           outliers.
#' @param name \code{chr} name for the file that can be optionally outputted
#'             via this function. The default name will be \code{dotplot.all}.
#' @param pdf  \code{boolean} indicating whether the functions results should
#'             be send to a PDF-file or whether they are intended for R-
#'             internal use. Default is output to PDF (option \code{true}).
#'
#' @return \code{List} of dotplots (\code{ggplot}s) for \code{pdf = F} or a
#'          PDF file in the filesystem for \code{pdf = T}.
#'
#' @seealso \code{\link{analysis.dotplot}}
#' @seealso \code{\link{cpp_valueOccurrencesToPoints}}
#'
#' @examples
#' KaggleHouse:::analysis.dotplot.all(iris, name = 'iris.dots', pdf = T)
analysis.dotplot.all <- function(df, name = 'dotplot.all', pdf = T) {
  stopifnot(is.data.frame(df))

  dotplots <- lapply(colnames(df), function(col_name) {
    analysis.dotplot(df[[col_name]], paste('Outlier Point Plot for', col_name))
  })

  if (pdf) {
    pdf_plots <- gridExtra::marrangeGrob(dotplots, nrow = 2, ncol = 1)
    ggplot2::ggsave(
      paste0(dir_presentation, "/outlier/understand_data.dotplots", ".pdf"),
      pdf_plots,
      width = 30, height = 20, units = "cm", dpi = 300
    )
  } else {
    return(dotplots)
  }
}

analysis.dotplot.all <- compiler::cmpfun(analysis.dotplot.all)


#' Generate Outlier Point Plot
#'
#' This function generates an outlier point plot (for a description see
#' slide 117 of the DA1 lecture of WT 2015/16) showing the value distribution
#' of data \code{df}, alllowing to identify potential outliers.
#'
#' The method generates a dotplot as an univariate outlier detector for a given
#' set of data \code{df} (must not be a \code{data.frame} or \code{matrix}.
#' So via the function \code{\link{analysis.dotplot}} it computes the
#' distribution of values in each given column via \code{\link{table}}.
#' Based on the distribution the method
#' \code{\link{cpp_valueOccurrencesToPoints}} will generate visualizable points
#' representing each occurence in the form \eqn{(x_i, y_{i,j})}{(x[i], y[i,j])}.
#'
#' @param df \code{data.frame} with the data that should be analyzed for
#'           outliers.
#' @param plot.title  Title for the \code{ggplot} object created via this
#'                    method.
#'
#' @return \code{ggplot} point plot.
#'
#' @seealso \code{\link{analysis.dotplot.all}}
#' @seealso \code{\link{cpp_valueOccurrencesToPoints}}
#'
#' @examples
#' KaggleHouse:::analysis.dotplot(
#'    iris$Sepal.Length,
#'    plot.title = 'Outlier Plot for the Sepal Length of the Iris Dataset'
#' )
analysis.dotplot <- function(df, plot.title = 'Outlier') {
  stopifnot( !is.data.frame(df), !is.matrix(df) )

  tab <- table(df)
  tab.df <- as.data.frame(tab)

  point.list <- cpp_valueOccurrencesToPoints(tab.df)
  point.list <- util.list_to_df(point.list)

  dotplot <- ggplot2::ggplot(point.list, ggplot2::aes(x = V1, y = V2)) +
              ggplot2::geom_point() +
              ggplot2::labs(title = plot.title) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  return(dotplot)
}

analysis.dotplot <- compiler::cmpfun(analysis.dotplot)