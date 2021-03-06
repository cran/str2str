% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{la2a}
\alias{la2a}
\title{List of (3D+) Arrays to (3D+) Array}
\usage{
la2a(la, dim.order = 1:(ndim(la[[1]]) + 1L), dimlab.list = NULL, check = TRUE)
}
\arguments{
\item{la}{list of 3D+ arrays which each have the same dimensions.}

\item{dim.order}{integer vector of length = \code{ndim(la[[1]]) + 1L} specifying
the order of dimensions for the returned array. The default is \code{1:(ndim(la[[1]]) + 1L)}
which means the arrays within \code{la} maintain their dimensions and the list
dimension is appended as the last dimension.}

\item{dimlab.list}{character vector of length 1 specifying the dimlabel for
the list dimension.}

\item{check}{logical vector of length 1 specifying whether to check the structure
of the input arguments. For example, check whether \code{la} is a list of 3D+ arrays.
This argument is available to allow flexibility in whether the user values
informative error messages (TRUE) vs. computational efficiency (FALSE).}
}
\value{
3D+ array where the list elements of \code{la} is now a dimension. The
order of the dimensions is determined by the argument \code{dim.order}. The
dimnames of the returned array is determined by the dimnames in \code{la[[1]]}
and \code{names(la)}.
}
\description{
\code{la2a} converts a list of (3D+) arrays to a one dimension larger (3D+)
array where the list dimension becomes the additional dimension of the array.
\code{la2a} is a simple wrapper function for \code{abind::abind}. If you have
a list of matrices, then use \code{lm2a}.
}
\examples{
la <- list("one" = HairEyeColor, "two" = HairEyeColor*2, "three" = HairEyeColor*3)
la2a(la) # default
la2a(la, dimlab.list = "Multiple")
la2a(la, dim.order = c(4,3,1,2))
la2a(la, dim.order = c(4,3,1,2), dimlab.list = "Multiple")
}
