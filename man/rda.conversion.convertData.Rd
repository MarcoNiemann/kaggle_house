% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rda_conversion.R
\name{rda.conversion.convertData}
\alias{rda.conversion.convertData}
\title{Convert RAW data to RDA data.}
\usage{
rda.conversion.convertData(directory = "inst/extdata")
}
\arguments{
\item{directory}{The directory in which the RAW files should be searched.}
}
\description{
This function converts RAW data into the package-typical RDA format.
}
\details{
This function is a wrapper around all other functions with the
\code{rda.conversion} prefix. It starts by calling
\code{\link{rda.conversion.ensureDataDir}} to ensure that the target
directory for the RDA files exists. Once that is assured the specified
\code{directory} will be checked via
\code{\link{rda.conversion.checkRawData}} for its existance. In case it
exists the list of contained files will be returned and be iteratively
loaded via \code{\link{rda.conversion.loadDataFile}} and saved as a RDA
via \code{\link{rda.conversion.saveAsRDA}}.
}
\seealso{
\code{\link{rda.conversion.ensureDataDir}}

\code{\link{rda.conversion.checkRawData}}

\code{\link{rda.conversion.loadDataFile}}

\code{\link{rda.conversion.saveAsRDA}}
}

