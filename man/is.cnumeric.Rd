% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{is.cnumeric}
\alias{is.cnumeric}
\title{Test for Character Numbers}
\usage{
is.cnumeric(x, warn = FALSE)
}
\arguments{
\item{x}{object whose structure is desired to be tested.}

\item{warn}{logical vector with length 1 specifying whether warnings should be printed
due to coercing a character vector that is not all number strings (i.e., one reason
the return object could be `FALSE`).}
}
\value{
logical vector with length 1 specifying whether `x` is a character vector
with all number strings.
}
\description{
\code{is.cnumeric} returns whether an object is a character vector with all
number strings.
}
\details{
\code{is.cnumeric} is useful for ensuring that converting a character vector to
a numeric vector is safe (i.e., won't introduce NAs).
}
\examples{
is.cnumeric(x = c("1","2","3")) # returns TRUE
is.cnumeric(x = c("1","number","3")) # returns FALSE
is.cnumeric(x = c("1","number","3"), warn = TRUE) # includes the warning
is.cnumeric(x = c(1,2,3)) # returns false because not a character vector
}
