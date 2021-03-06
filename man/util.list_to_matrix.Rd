% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{util.list_to_matrix}
\alias{util.list_to_matrix}
\title{Convert list of matrices to matrix}
\usage{
util.list_to_matrix(lst)
}
\arguments{
\item{lst}{\code{list} with matrices.}
}
\value{
\code{matrix} being the concatenated version of all matrices in
        \code{list} \code{lst}.
}
\description{
Transforms a list of matrices (with the same columns ['objects', 'counts'])
into a concatenated matrix.
}

