% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_dotplot_outlier.R
\name{analysis.dotplot}
\alias{analysis.dotplot}
\title{Generate Outlier Point Plot}
\usage{
analysis.dotplot(df, plot.title = "Outlier")
}
\arguments{
\item{df}{\code{data.frame} with the data that should be analyzed for
outliers.}

\item{plot.title}{Title for the \code{ggplot} object created via this
method.}
}
\value{
\code{ggplot} point plot.
}
\description{
This function generates an outlier point plot (for a description see
slide 117 of the DA1 lecture of WT 2015/16) showing the value distribution
of data \code{df}, alllowing to identify potential outliers.
}
\details{
The method generates a dotplot as an univariate outlier detector for a given
set of data \code{df} (must not be a \code{data.frame} or \code{matrix}.
So via the function \code{\link{analysis.dotplot}} it computes the
distribution of values in each given column via \code{\link{table}}.
Based on the distribution the method
\code{\link{cpp_valueOccurrencesToPoints}} will generate visualizable points
representing each occurence in the form \eqn{(x_i, y_{i,j})}{(x[i], y[i,j])}.
}
\examples{
KaggleHouse:::analysis.dotplot(
   iris$Sepal.Length,
   plot.title = 'Outlier Plot for the Sepal Length of the Iris Dataset'
)
}
\seealso{
\code{\link{analysis.dotplot.all}}

\code{\link{cpp_valueOccurrencesToPoints}}
}

