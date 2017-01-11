#' 2D plot generator
#'
#' Generates a set of two dimensional plots where one variable is plotted against all
#' other variables.
#'
#' All two dimensional plots are saved in one pdf file named
#' \code{understand_data.data_against} using ggplot. This function is typically used
#' to find correlations to the target variable.
#'
#' @param df Dataframe containing all features to which \code{var should be plotted}
#'
#' @param var Vector which is the variable that is plotted to each variable in \code{df}
#'
#' @param pdf Boolean variable stating whether to create a pdf or not
#'
#'
plot_against_var <- function(df, var, pdf = TRUE) {
  stopifnot(is.data.frame(df) && is.data.frame(var))
  echoln("Start generation of plots of SalePrice against all other features...")
  plot_var <- lapply(colnames(df), function(col_name) {
    dframe <- data.frame(a = df[[col_name]], b = var)
    colnames(dframe)[2] = "b"

    gg_ploty <- ggplot2::ggplot(dframe, ggplot2::aes(x = a, y = b)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = col_name, y = colnames(var))

    gg_ploty
  })

  if (pdf) {
    pdf_plots <- gridExtra::marrangeGrob(plot_var, nrow = 3, ncol = 2)
    ggplot2::ggsave(
      paste0(dir_presentation, "/understand_data.data_against",
             colnames(var), ".pdf"),
      pdf_plots,
      width = 30, height = 20, units = "cm",
      dpi = 300)
    grDevices::dev.off()
  } else {
    return(plot_var)
  }
  echoln("Generation of plots of SalePrice against all other features finished.")
}

## Precompile the function to increase the performance
plot_against_var <- compiler::cmpfun(plot_against_var)