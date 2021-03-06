% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{m2lv}
\alias{m2lv}
\title{Matrix to List of (Atomic) Vectors}
\usage{
m2lv(m, along, check = TRUE)
}
\arguments{
\item{m}{matrix (i.e., array with 2 dimensions).}

\item{along}{numeric vector of length 1 specifying which dimension to slice
the matrix along. If 1, then the matrix is sliced by rows. If 2, then the
matrix is sliced by columns.}

\item{check}{logical vector of length 1 specifying whether to check the structure
of the input arguments. For example, check whether \code{m} is a matrix.
This argument is available to allow flexibility in whether the user values
informative error messages (TRUE) vs. computational efficiency (FALSE).}
}
\value{
list of (atomic) vectors. If \code{along} = 1, then the names are the
rownames of \code{m} and the vectors are rows from \code{m}. If \code{along} = 2,
then the names are the colnames of \code{m} and the vector are columns from \code{m}.
Note, the vectors always have the same length as \code{nrow(m)}.
}
\description{
\code{m2lv} converts a matrix to a list of (atomic) vectors. This is useful
since there is no \code{as.list.matrix} method. When rownames and/or colnames
are NULL, they are replaced by their position numerals so that the dimension
information is retained.
}
\examples{
m2lv(VADeaths, along = 1)
m2lv(VADeaths, along = 2)
m2lv(m = as.matrix(x = attitude, rownames.force = TRUE), along = 1)
m2lv(m = as.matrix(x = attitude, rownames.force = TRUE), along = 2)
m2lv(m = as.matrix(x = unname(attitude), rownames.force = FALSE),
   along = 1) # dimnames created as position numerals
m2lv(m = as.matrix(x = unname(attitude), rownames.force = FALSE),
   along = 2) # dimnames created as position numerals
# check = FALSE
try_expr(m2lv(VADeaths, along = 3, check = FALSE)) # less informative error message
try_expr(m2lv(VADeaths, along = 3, check = TRUE)) # more informative error message
}
