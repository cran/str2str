% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{v2d}
\alias{v2d}
\title{(Atomic) Vector to Data-Frame}
\usage{
v2d(v, along = 2, rtn.dim.nm = NULL, stringsAsFactors = FALSE, check = TRUE)
}
\arguments{
\item{v}{(atomic) vector.}

\item{along}{numeric vector of length 1 that is equal to either 1 or 2 specifying
which dimension to bind \code{v} along. 1 means that \code{v} is binded along
rows (i.e., dimension 1) into a one row data.frame. 2 means that \code{v} is binded
along columns (i.e., dimension 2) into a one column data.frame.}

\item{rtn.dim.nm}{character vector of length 1 specifying what dimname to use
for the dimension of length 1 in the returned data.frame. If \code{along} = 1,
then \code{rtn.dim.nm} will be the single rowname. If \code{along} = 2, then
\code{rtn.dim.nm} will be the single colname. If NULL, then the dimension of
length 1 will be created by default with \code{data.frame} internally, which
will have the rowname be "1" and the colname "V1".}

\item{stringsAsFactors}{logical vector of length 1 specifying if \code{v}
should be converted to a factor in the case that typeof is character.}

\item{check}{logical vector of length 1 specifying whether to check the structure
of the input arguments. For example, check whether \code{v} is an atomic vector.
This argument is available to allow flexibility in whether the user values
informative error messages (TRUE) vs. computational efficiency (FALSE).}
}
\value{
data.frame with typeof = \code{typeof(v)}. If \code{along} = 1, then the
dimensions = \code{c(1L, length(v))} and dimnames = \code{list(rtn.dim.nm, names(v))}.
If \code{along} = 2, then the dimensions = \code{c(length(v), 1L)} and dimnames =
\code{list(names(v), rtn.dim.nm)}.
}
\description{
\code{v2m} converts an (atomic) vector to a single row or single column data.frame.
The benefit of \code{v2m} over \code{as.data.frame.vector} is that the dimension
along which the vector is binded can be either rows or columns, whereas in
\code{as.data.frame.vector} it can only be binded along a column, and that
\code{v2m} will keep the names of \code{v} in the dimnames of the returned
data.frame.
}
\examples{
x <- setNames(mtcars[, "mpg"], nm = row.names(mtcars))
v2d(x)
v2d(v = x, along = 1)
v2d(v = x, rtn.dim.nm = "mpg")
}
