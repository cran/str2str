% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{d2lv}
\alias{d2lv}
\title{Data-Frame to List of (Atomic) Vectors}
\usage{
d2lv(d, along, check = TRUE)
}
\arguments{
\item{d}{data.frame.}

\item{along}{numeric vector of length 1 specifying which dimension to slice
the data.frame along. If 1, then the data.frame is sliced by rows. If 2, then the
data.frame is sliced by columns.}

\item{check}{logical vector of length 1 specifying whether to check the structure
of the input arguments. For example, check whether \code{d} is a data.frame.
This argument is available to allow flexibility in whether the user values
informative error messages (TRUE) vs. computational efficiency (FALSE).}
}
\value{
list of (atomic) vectors. If \code{along} = 1, then the names are the
rownames of \code{d} and the vectors are rows from \code{d}. If \code{along} = 2,
then the names are the colnames of \code{d} and the vector are columns from \code{d}.
Note, the vectors always have the same length as \code{nrow(d)}.
}
\description{
\code{d2lv} converts a data.frame to a list of (atomic) vectors. This function
is really only worthwhile when \code{along} = 1 since when \code{along} = 2,
the function is essentially \code{as.list.data.frame(d)}.
}
\examples{
d2lv(mtcars, along = 1)
d2lv(mtcars, along = 2)
d2lv(CO2, along = 1) # all vectors converted to typeof character
d2lv(CO2, along = 2) # each column stays its own typeof (or class for factors)
# check = FALSE
try_expr(d2lv(mtcars, along = 3, check = FALSE)) # less informative error message
try_expr(d2lv(mtcars, along = 3, check = TRUE)) # more informative error message
}
