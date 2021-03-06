% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{is.colnames}
\alias{is.colnames}
\title{Test for `colnames`}
\usage{
is.colnames(nm, x)
}
\arguments{
\item{nm}{character vector.}

\item{x}{object whose colnames are desired to be tested.}
}
\value{
TRUE for every element of `nm` that is a colname of x and FALSE otherwise.
The structure is a logical vector with length = length(`nm`) and names = `nm`.
See details for special cases.
}
\description{
\code{is.colnames} returns whether elements of a character vector are colnames of an object.
}
\details{
If the object does not have any colnames, then the function will return `FALSE`
for each element of the character vector.
}
\examples{
data("mtcars")
is.colnames(x = as.matrix(mtcars), nm = c("MPG","mpg"))
}
