% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{not.row.names}
\alias{not.row.names}
\title{Identify Elements That are Not Row.names}
\usage{
not.row.names(x, nm)
}
\arguments{
\item{x}{object with a row.names attribute}

\item{nm}{character vector specifying the elements to test as row.names of \code{x}.}
}
\value{
character vector containing the elements of \code{nm} that are not row.names
of \code{x}.
}
\description{
\code{not.row.names} identifies which elements from \code{nm} are not row.names of \code{x}.
If all elements are row.names, then a character vector of length 0 is returned.
}
\examples{
not.row.names(x = mtcars, nm = c("Mazda RX4","mazda RX4"))
}
