% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{util.number_na}
\alias{util.number_na}
\title{Count Number of \code{NA} Values in a \code{data.frame}}
\usage{
util.number_na(data)
}
\arguments{
\item{data}{\code{data.frame} in which the number of \code{NA} values is
counted for each column.}
}
\value{
\code{Named num} (being a \code{numeric} vector also containing the
        column name for each \code{numeric} value) indicating the number of
        \code{NA} values for each column in \code{data}.
}
\description{
This method counts the number of \code{NA} values for each column of a given
\code{data.frame}.
}
\details{
This method takes a \code{data.frame} as an input and then counts the number
of \code{NA} values in each column. The results get stored in a
\code{numeric} vector (always together with the column name).
}
\examples{
 KaggleHouse:::util.number_na(data_train)

}

