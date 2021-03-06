% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\name{cbind_fill_matrix}
\alias{cbind_fill_matrix}
\title{Bind Matrices Along Columns - Filling in Missing Rows with NA}
\usage{
cbind_fill_matrix(...)
}
\arguments{
\item{...}{any combination of matrices, data.frames, or atomic vectors input
as separate arguments or a list.}
}
\value{
matrix created by combining all the objects input together. It will always
have rownames. It will only have colnames if \code{...} is a list of vectors,
in which the colnames will be the names of the list.
}
\description{
\code{cbind_fill_matrix} binds together matrix-like objects by columns. The input
objects will all internally be converted to matrices by the generic function
\code{as.matrix}. When some objects do not contain rows that are present in other
objects, NAs will be added to fill up the returned combined matrix. If a matrix
doesn't have rownames, the row number is used. Note that this means that a row
with name "1" is merged with the first row of a matrix without name and so on.
The returned matrix will always have row names. Colnames are ignored.
}
\details{
\code{cbind_fill_matrix} is \code{t.default} + \code{plyr::rbind.fill.matrix}
+ \code{t.default} and is based on the code within \code{plyr::rbind.fill.matrix}.
}
\examples{

# standard use
A <- matrix(1:4, 2)
B <- matrix(6:11, 3)
print(A)
print(B)
cbind_fill_matrix(A, B)

# help with unstack()
row_keep <- sample(1:nrow(InsectSprays), size = 36)
InsectSprays2 <- InsectSprays[row_keep, ]
unstacked <- unstack(InsectSprays2)
cbind_fill_matrix(unstacked)

# using rownames for binding
rownames(A) <- c("one", "two")
rownames(B) <- c("three","two","one")
print(A)
print(B)
cbind_fill_matrix(A, B)

# data.frame input
C <- data.frame("V1" = c(12,13,14,15), row.names = c("one","two","three","four"))
print(C)
cbind_fill_matrix(A, B, C)

# atomic vector input
A <- matrix(1:4, 2)
B <- matrix(6:11, 3)
C <- c(12,13,14,15)
cbind_fill_matrix(A, B, C)

# same as plyr::rbind.fill.matrix, cbind_fill_matrix doesn't like some input
# with dimnames and others without...
rownames(A) <- c("one", "two")
print(A)
print(B)
cbind_fill_matrix(A, B)

}
\seealso{
\code{\link{cbind_fill}}
   \code{\link[plyr]{rbind.fill.matrix}}
}
