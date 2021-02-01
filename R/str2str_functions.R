# STR2STR ####

#' str2str: Convert R Objects from One Structure to Another.
#'
#' @description \code{str2str} is a package for converting R objects to different structures.
#' It focuses on four primary R objects: (atomic) vectors, matrices, data.frames, and
#' arrays as well as lists of these objects. For example, converting a (atomic) vector to
#' a data.frame (i.e., \code{v2d()}) or a list of (atomic) vectors to a matrix
#' (i.e., \code{lv2m()}. The current version of the package does not have a
#' function for every convertion (e.g., \code{a2m()}), but some additional
#' convertion functions may be included in future versions if I find a use for them.
#' The package was motivated by limitations of the base R \code{as.<str>.<method>}
#' suite of functions and the \code{plyr} R package \code{**ply(.fun = NULL)}
#' suite of functions for converting R objects to different structures. While those
#' functions are often useful, there are times different conversions are desired
#' or different naming schemes are desired. That is what this package offers R users.
#' It also contains various utility functions for working with common R objects.
#' For example, \code{is.colnames} and \code{ndim}.
#'
#' @section Limitations:
#' This packages does NOT handle the nuances of R objects. It is not for comprehensive
#' restructuring of any version of R objects, but rather for restructuring commonly
#' used versions of R objects. For example, the functions are not tested with the
#' raw and complex typeof atomic vectors, list arrays, or data.frames containing non-atomic
#' vector columns (e.g., matrix or list columns). The base R \code{as.<str>.<method>}
#' functions allow for comprehensive restructuring of R objects; however, at the cost
#' of less convenient convertions for commonly used versions of R objects. The
#' \code{str2str} package seeks to fill that gap in useability.
#'
#' @section Abbreviations:
#' v = (atomic) vector
#' m = matrix
#' d = data.frame
#' a = (3D+) array
#' l = list
#' el = elements
#' nm = names
#' uv = unique values
#' lgl = logical
#' int = integer
#' dbl = double
#' chr = character
#' fct = factor
#' lvl = levels
#' vrb = variable
#' frm = formula
#' fun = function
#'
#' @import datasets stats utils
#'
#' @docType package
#'
#' @name str2str
NULL

# IS.OBJECT ####

# is.whole #

#' Test for Whole Numbers
#'
#' \code{is.whole} returns whether elements of a numeric vector are whole numbers.
#'
#' @param x numeric vector.
#'
#' @param tol tolerance allowed for double floating point numbers. This is always
#' a positive number. The default is based on the numerical characteristics of
#' the machine that R is running on. See \code{.Machine}.
#'
#' @return TRUE for every element of `x` that is a whole number and FALSE otherwise.
#' The structure is a logical vector with length = length(`x`) and names = names(`x`).
#'
#' @examples
#' v <- c(1.0, 1L, 1.1)
#' is.whole(v)
#' @export
is.whole <- function(x, tol = .Machine[["double.eps"]]) {
   abs(x - round(x = x, digits = 0)) < tol
}

# is.empty #

#' Test for Empty Characters
#'
#' \code{is.empty} returns whether elements of a character vector are empty (i.e., "").
#'
#' @param x character vector.
#'
#' @param trim logical vector with a single element specifying whether white spaces
#' should be trimmed from the character vector. See \code{trimws}.
#'
#' @return TRUE for every element of `x` that is empty (i.e., "") and FALSE otherwise.
#' The structure is a logical vector with length = length(`x`) and names = names(`x`).
#'
#' @examples
#' v <- c("1", " ", "")
#' is.empty(v)
#' @export
is.empty <- function(x, trim = FALSE) {
   if (trim) x <- trimws(x); x == ""
}

# is.cnumeric #

#' Test for Character Numbers
#'
#' \code{is.cnumeric} returns whether an object is a character vector with all
#' number strings.
#'
#' \code{is.cnumeric} is useful for ensuring that converting a character vector to
#' a numeric vector is safe (i.e., won't introduce NAs).
#'
#' @param x object whose structure is desired to be tested.
#'
#' @param warn logical vector with length 1 specifying whether warnings should be printed
#' due to coercing a character vector that is not all number strings (i.e., one reason
#' the return object could be `FALSE`).
#'
#' @return logical vector with length 1 specifying whether `x` is a character vector
#' with all number strings.
#'
#' @examples
#' is.cnumeric(x = c("1","2","3")) # returns TRUE
#' is.cnumeric(x = c("1","number","3")) # returns FALSE
#' is.cnumeric(x = c("1","number","3"), warn = TRUE) # includes the warning
#' is.cnumeric(x = c(1,2,3)) # returns false because not a character vector
#' @export
is.cnumeric <- function(x, warn = FALSE) {
   if (!warn) fun <- match.fun(suppressWarnings)
   if (warn) fun <- match.fun(identity)
   is.character(x) && fun(all(!(is.na(as.numeric(x)))))
}

# is.avector #

#' Test for an Atomic Vector
#'
#' \code{is.avector} returns whether an object is an atomic vector with typeof
#' character, logical, integer, or double.
#'
#' \code{is.avector} is simply a logical "and" of \code{is.atomic} and \code{is.vector}.

#' @param x = object whose structure is desired to be tested.
#'
#' @return logical vector with length 1 specifying whether `x` is an atomic vector.
#'
#' @examples
#' is.avector(x = c(1,2,3))
#' is.avector(x = array(c(1,2,3))) # returns false for arrays
#' is.avector(x = list(1,2,3)) # returns false for lists
#' @export
is.avector <- function(x) {
   is.atomic(x) && is.vector(x)
}

# is.names #

#' Test for `names`
#'
#' \code{is.names} returns whether elements of a character vector are names of an object.
#'
#' If the object does not have any names, then the function will return `FALSE`
#' for each element `nm`.
#'
#' @param x object whose names are desired to be tested.
#'
#' @param nm character vector.
#'
#' @return TRUE for every element of `nm` that is a name of `x` and FALSE otherwise.
#' The structure is a logical vector with length = length(`nm`) and names = `nm`.
#' See details for special cases.
#'
#' @examples
#' v <- setNames(object = letters, nm = LETTERS)
#' is.names(x = v, nm = c("A","a"))
#' data("mtcars")
#' is.names(x = mtcars, nm = c("MPG","mpg"))
#' @export
is.names <- function(nm, x) {
   setNames(object = `%in%`(x = nm, table = names(x)), nm = nm)
}

# not.names #

#' Identify Elements That are Not Names
#'
#' \code{not.names} identifies which elements from \code{nm} are not names of \code{x}.
#' If all elements are names, then a character vector of length 0 is returned.
#'
#' @param x object with a names attribute
#'
#' @param nm character vector specifying the elements to test as names of \code{x}.
#'
#' @return character vector containing the elements of \code{nm} that are not names
#' of \code{x}.
#'
#' @examples
#' v <- setNames(object = letters, nm = LETTERS)
#' not.names(x = v, nm = c("A","a"))
#' data("mtcars")
#' not.names(x = mtcars, nm = c("MPG","mpg"))
#' not.names(x = mtcars, names(mtcars)) # returns a character vector of length 0
#' @export
not.names <- function(x, nm) {
   is_names <- is.names(x = x, nm = nm)
   output <- names(is_names[!(is_names)])
   return(output)
}

# is.row.names #

#' Test for `row.names`
#'
#' \code{is.row.names} returns whether elements of a character vector are row.names of an object.
#'
#' If the object does not have any row.names, then the function will return `FALSE`
#' for each element of the character vector. As a reminder, \code{row.names} does not
#' respond to a manually added "row.names" attribute (e.g., to a vector). If this is
#' tried, then \code{is.row.names} will return `FALSE` for each element `nm`.
#'
#' @param x object whose row.names are desired to be tested.
#'
#' @param nm character vector.
#'
#' @return TRUE for every element of `nm` that is a row.name of x and FALSE otherwise.
#' The structure is a logical vector with length = length(`nm`) and names = `nm`.
#' See details for special cases.
#'
#' @examples
#' data("mtcars")
#' is.row.names(x = mtcars, nm = c("Mazda RX4","mazda RX4"))
#' @export
is.row.names <- function(nm, x) {
   setNames(object = `%in%`(x = nm, table = row.names(x)), nm = nm)
}

# not.row.names #

#' Identify Elements That are Not Row.names
#'
#' \code{not.row.names} identifies which elements from \code{nm} are not row.names of \code{x}.
#' If all elements are row.names, then a character vector of length 0 is returned.
#'
#' @param x object with a row.names attribute
#'
#' @param nm character vector specifying the elements to test as row.names of \code{x}.
#'
#' @return character vector containing the elements of \code{nm} that are not row.names
#' of \code{x}.
#'
#' @examples
#' not.row.names(x = mtcars, nm = c("Mazda RX4","mazda RX4"))
#' @export
not.row.names <- function(x, nm) {
   is_row.names <- is.row.names(x = x, nm = nm)
   output <- names(is_row.names[!(is_row.names)])
   return(output)
}

# is.colnames #

#' Test for `colnames`
#'
#' \code{is.colnames} returns whether elements of a character vector are colnames of an object.
#'
#' If the object does not have any colnames, then the function will return `FALSE`
#' for each element of the character vector.
#'
#' @param x object whose colnames are desired to be tested.
#'
#' @param nm character vector.
#'
#' @return TRUE for every element of `nm` that is a colname of x and FALSE otherwise.
#' The structure is a logical vector with length = length(`nm`) and names = `nm`.
#' See details for special cases.
#'
#' @examples
#' data("mtcars")
#' is.colnames(x = as.matrix(mtcars), nm = c("MPG","mpg"))
#' @export
is.colnames <- function(nm, x) {
   setNames(object = `%in%`(x = nm, table = colnames(x)), nm = nm)
}

# not.colnames #

#' Identify Elements That are Not Colnames
#'
#' \code{not.colnames} identifies which elements from \code{nm} are not colnames of \code{x}.
#' If all elements are colnames, then a character vector of length 0 is returned.
#'
#' @param x object with a colnames attribute
#'
#' @param nm character vector specifying the elements to test as colnames of \code{x}.
#'
#' @return character vector containing the elements of \code{nm} that are not colnames
#' of \code{x}.
#'
#' @examples
#' not.colnames(x = as.matrix(mtcars), nm = c("MPG","mpg"))
#' @export
not.colnames <- function(x, nm) {
   is_colnames <- is.colnames(x = x, nm = nm)
   output <- names(is_colnames[!(is_colnames)])
   return(output)
}

# is.rownames #

#' Test for `rownames`
#'
#' \code{is.rownames} returns whether elements of a character vector are rownames of an object.
#'
#' If the object does not have any rownames, then the function will return `FALSE`
#' for each element of the character vector.
#'
#' @param x object whose rownames are desired to be tested.
#'
#' @param nm character vector.
#'
#' @return TRUE for every element of `nm` that is a rowname of x and FALSE otherwise.
#' The structure is a logical vector with length = length(`nm`) and names = `nm`.
#' See details for special cases.
#'
#' @examples
#' data("mtcars")
#' is.rownames(x = as.matrix(mtcars), nm = c("Mazda RX4","mazda RX4"))
#' @export
is.rownames <- function(nm, x) {
   setNames(object = `%in%`(x = nm, table = rownames(x)), nm = nm)
}

# not.rownames #

#' Identify Elements That are Not Rownames
#'
#' \code{not.rownames} identifies which elements from \code{nm} are not rownames of \code{x}.
#' If all elements are rownames, then a character vector of length 0 is returned.
#'
#' @param x object with a rownames attribute
#'
#' @param nm character vector specifying the elements to test as rownames of \code{x}.
#'
#' @return character vector containing the elements of \code{nm} that are not rownames
#' of \code{x}.
#'
#' @examples
#' not.rownames(x = as.matrix(mtcars), nm = c("Mazda RX4","mazda RX4"))
#' @export
not.rownames <- function(x, nm) {
   is_rownames <- is.rownames(x = x, nm = nm)
   output <- names(is_rownames[!(is_rownames)])
   return(output)
}

# all_same #

#' Test if All Elements are the Same
#'
#' \code{all_same} tests if all elements are the same. The elements could be either
#' from an atomic vector, list vector, or list. If \code{x} does not have any unique
#' values (e.g., NULL), then FALSE is returned.
#'
#' The machine precision of \code{all_same} for numeric vectors is the same as
#' \code{unique}. This can causes a problem for some floating-point numbers.
#'
#' @param x atomic vector, list vector, or list.
#'
#' @return logical vector of length 1 specifying whether all the elements in \code{x}
#' are the same (TRUE) or not (FALSE).
#'
#' @examples
#' all_same(rep.int("a", times = 10))
#' all_same(rep.int(1, times = 10))
#' all_same(c(1.0000000, 1.0000001, 0.9999999)) # machine precision good for most cases
#' @export
all_same <- function(x) {
   1L == length(unique(x))
}

# all_diff #

#' Test if All Elements are Different
#'
#' \code{all_diff} tests if all elements are different. The elements could be either
#' from an atomic vector, list vector, or list. If \code{x} does not have any unique
#' values (e.g., NULL), then FALSE is returned.
#'
#' The machine precision of \code{all_diff} for numeric vectors is the same as
#' \code{unique}. This can causes a problem for some floating-point numbers.
#'
#' @param x atomic vector, list vector, or list.
#'
#' @return logical vector of length 1 specifying whether all the elements in \code{x}
#' are the same (TRUE) or not (FALSE).
#'
#' @examples
#' all_diff(1:10)
#' all_diff(c(1:10, 10))
#' all_diff(c(1.0000000, 1.0000001, 0.9999999)) # machine precision good for most cases
#' @export
all_diff <- function(x) {
   length(x) == length(unique(x))
}

# ASSIGN ####

# `append<-` #

#' Add Elements to Vectors
#'
#' \code{`append<-`} adds elements to vectors as a side effect. The purpose of
#' the function is to replace the need to use vec2 <- append(vec1, add1);
#' vec3 <- append(vec2, add2); vec4 <- append(vec3, add3), etc. It functions similarly
#' to \code{`[<-.default`}, but allows you to specify the location of the elements
#' similar to \code{append} (vs. \code{c}).
#'
#' Some traditional R folks may find this function uncomfortable. R is famous for limiting
#' side effects, except for a few notable exceptions (e.g., \code{`[<-`} and \code{`names<-`}).
#' Part of the reason is that side effects can be computationally inefficient in R.
#' The entire object often has to be re-constructed and re-saved to memory. For
#' example, a more computationally efficient alternative to append(vec) <- add1;
#' append(vec) <- add2; append(vec) <- add3 is vec1 <- do.call(what = c,
#' args = list(dat, add1, add2, add3)). However, \code{`append<-`} was not created
#' for R programming use when computational efficiency is valued; it was created
#' for R interactive use when user convenience is valued.
#'
#' @param x atomic vector, list vector, or list.
#'
#' @param after either an integer vector with length 1 or a character vector of
#' length 1 specifying where to add \code{value}. If an integer vector, it is the position
#' of an element. If a character vector, it is the element with that name. Similar to
#' \code{append}, use 0L if you want the added elements to be first.
#'
#' @param nm character vector of length equal to the \code{length(value)} that specifies
#' the names of \code{value} once added to \code{x} as elements. This is an optional
#' argument that defaults to NULL where the pre-existing names of \code{value} are used.
#'
#' @param overwrite logical vector of length 1 specifying whether elements from
#' \code{value} or \code{nm} should overwrite elements in \code{x} with the
#' same names. Note, if \code{overwrite} = FALSE, repeat names are possible
#' similar to \code{append}.
#'
#' @param value vector of the same typeof as \code{x} to be added as elements to \code{x}.
#' Note that for atomic vectors, if more complex elements are added, then the return
#' object will be typeof the most complex element in \code{x} and \code{value}.
#'
#' @return Like other similar functions (e.g., \code{`names<-`} and \code{`[<-`}),
#' it does not appear to have a return object. However, it technically does as a
#' side effect. The argument \code{x} will have been changed such that \code{value}
#' has been added as elements. If a traditional return object is desired, and no side
#' effects, then it can be called like a traditional function:
#' vec2 <- `append<-`(vec1, value = add1).
#'
#' @examples
#' x <- letters
#' append(x) <- LETTERS
#' append(x, after = match(x = "z", table = x)) <- "case_switch" # with the position
#'    # of the added value specified
#' y <- setNames(object = letters, nm = LETTERS)
#' append(y) <- c("ONE" = 1, "TWO" = 2, "THREE" = 3) # with names provided by `value`
#' y <- y[1:(length(y) - 3)]
#' append(y, nm = c("ONE","TWO","THREE")) <- c(1,2,3) # with names specified by `nm`
#' # using overwrite
#' append(y, overwrite = TRUE) <- c("ONE" = "one","TWO" = "two", "THREE" = "three")
#' append(y, overwrite = FALSE) <- c("ONE" = "one","TWO" = "two", "THREE" = "three")
#' @export
`append<-` <- function(x, after = length(x), nm = NULL, overwrite = TRUE, value) {
   # the last argument has to be named "value" for *<- functions"

   if (!(is.null(nm))) names(value) <- nm
   if (overwrite == TRUE) {
      same_names <- intersect(x = names(x), y = names(value))
      if (length(same_names) > 0) {
         x[same_names] <- value[same_names]
         value <- value[!(is.element(el = rownames(value), set = same_names))]
         if (length(value) == 0) return(x) # when all elements in `value` have the same elements as in `data`
      }
   }
   output <- append(x = x, values = value, after = after)
   return(output)
}

# `cbind<-` #

#' Add Columns to Data Objects
#'
#' \code{`cbind<-`} adds columns to data objects as a side effect. The purpose of
#' the function is to replace the need to use dat2 <- cbind(dat1, add1);
#' dat3 <- cbind(dat2, add2); dat4 <- cbind(dat3, add3), etc. For data.frames,
#' it functions similarly to \code{`[<-.data.frame`}, but allows you to specify the
#' location of the columns similar to \code{append} (vs. \code{c}) and overwrite
#' columns with the same colnames. For matrices, it offers more novel functionality
#' since \code{`[<-.matrix`} does not exist.
#'
#' Some traditional R folks may find this function uncomfortable. R is famous for limiting
#' side effects, except for a few notable exceptions (e.g., \code{`[<-`} and \code{`names<-`}).
#' Part of the reason is that side effects can be computationally inefficient in R.
#' The entire object often has to be re-constructed and re-saved to memory. For
#' example, a more computationally efficient alternative to cbind(dat) <- add1;
#' cbind(dat) <- add2; cbind(dat) <- add3 is dat1 <- do.call(what = cbind,
#' args = list(dat, add1, add2, add3)). However, \code{`cbind<-`} was not created
#' for R programming use when computational efficiency is valued; it is created
#' for R interactive use when user convenience is valued.
#'
#' Similar to \code{`cbind`}, \code{`cbind<-`} works with both data.frames and matrices.
#' This is because \code{`cbind`} is a generic function with a default method that
#' works with matrices and a data.frame method that works with data.frames. Similar
#' to \code{`cbind`}, if colnames of \code{value} are not given and \code{col.nm}
#' is left NULL, then the colnames of the return object are automatically created
#' and can be dissatisfying.
#'
#' @param data data.frame or matrix of data.
#'
#' @param after either an integer vector with length 1 or a character vector of
#' length 1 specifying where to add \code{value}. If an integer vector, it is the
#' position of a column. If a character vector it is the column with that name.
#' Similar to \code{append}, use 0L if you want the added columns to be first.
#'
#' @param col.nm character vector of length equal to \code{NCOL(value)} that
#' specifies the colnames of \code{value} once added to \code{data} as columns.
#' This is an optional argument that defaults to NULL where the pre-existing colnames
#' of \code{value} are used.
#'
#' @param overwrite logical vector of length 1 specifying whether columns from
#' \code{value} or \code{col.nm} should overwrite columns in \code{data} with the
#' same colnames. Note, if \code{overwrite} = FALSE, repeat colnames are possible
#' similar to \code{cbind}.
#'
#' @param value data.frame, matrix, or atomic vector to be added as columns to
#' \code{data}. If a data.frame or matrix, it must have the same nrow as \code{data}.
#' If an atomic vector, it must have length equal to nrow of \code{data}.
#'
#' @return Like other similar functions (e.g., \code{`names<-`} and \code{`[<-`}),
#' \code{`cbind<-`} does not appear to have a return object. However, it technically
#' does as a side effect. The argument \code{data} will have been changed such that
#' \code{value} has been added as columns. If a traditional return object is desired,
#' and no side effects, then it can be called like a traditional function:
#' dat2 <- `cbind<-`(dat1, value = add1).
#'
#' @examples
#' attitude2 <- attitude
#' cbind(attitude2) <- rowMeans(attitude2) # defaults to colnames = "value"
#' attitude2["value"] <- NULL
#' cbind(attitude2, col.nm = "mean") <- rowMeans(attitude2) # colnames specified by `col.nm`
#' attitude2["mean"] <- NULL
#' cbind(attitude2, after = "privileges", col.nm = c("mean","sum")) <-
#'    cbind(rowMeans(attitude2), rowSums(attitude2)) # `value` can be a matrix
#' attitude2[c("mean","sum")] <- NULL
#' attitude2 <- `cbind<-`(data = attitude2, value = rowMeans(attitude2)) # traditional call
#' attitude2["value"] <- NULL
#' cbind(attitude2, after = "privileges", col.nm = "mean") <-
#'    rowMeans(attitude2) # `data` can be a matrix
#' cbind(attitude2) <- data.frame("mean" = rep.int(x = "mean", times = 30L)) # overwrite = TRUE
#' cbind(attitude2, overwrite = FALSE) <-
#'    data.frame("mean" = rep.int(x = "mean", times = 30L)) # overwrite = FALSE
#' cbind(attitude2) <- data.frame("mean" = rep.int(x = "MEAN", times = 30L),
#'    "sum" = rep.int(x = "SUM", times = 30L)) # will overwrite only the first "mean" column
#'    # then will append the remaining columns
#' @export
`cbind<-` <- function(data, after = ncol(data), col.nm = NULL, overwrite = TRUE,
   value) {

   if (!(is.null(col.nm))) {
      if (is.vector(value) && !(is.data.frame(value))) value <- as.matrix(value)
      colnames(value) <- col.nm
   }
   Ncol <- ncol(data)
   if (overwrite == TRUE) {
      same_names <- intersect(x = colnames(data), y = colnames(value))
      if (length(same_names) > 0) {
         data[, same_names] <- value[, same_names]
         value <- value[, !(is.element(el = colnames(value), set = same_names)), drop = FALSE]
         if (ncol(value) == 0) return(data) # when all columns in `value` have the same colnames as columns in `data`
      }
   }
   if (is.character(after)) after <- match(x = after, table = colnames(data))
   if (after == 0L) output <- cbind(value, data)
   if (after == Ncol) output <- cbind(data, value)
   if (after != 0L && after != Ncol) {
      data_before <- data[, 1L:after]
      data_after <- data[, (after + 1L):(Ncol)]
      output <- cbind(data_before, value, data_after)
   }
   return(output)
}

# `rbind<-` #

#' Add Rows to Data Objects

#' \code{`rbind<-`} adds rows to data objects as a side effect. The purpose of
#' the function is to replace the need to use dat2 <- rbind(dat1, add1);
#' dat3 <- rbind(dat2, add2); dat4 <- rbind(dat3, add3), etc. For data.frames,
#' it functions similarly to \code{`[<-.data.frame`}, but allows you to specify the
#' location of the rows similar to \code{append} (vs. \code{c}) and overwrite
#' rows with the same rownames. For matrices, it offers more novel functionality
#' since \code{`[<-.matrix`} does not exist.
#'
#' Some traditional R folks may find this function uncomfortable. R is famous for limiting
#' side effects, except for a few notable exceptions (e.g., \code{`[<-`} and \code{`names<-`}).
#' Part of the reason is that side effects can be computationally inefficient in R.
#' The entire object often has to be re-constructed and re-saved to memory. For
#' example, a more computationally efficient alternative to rbind(dat) <- add1;
#' rbind(dat) <- add2; rbind(dat) <- add3 is dat1 <- do.call(what = rbind,
#' args = list(dat, add1, add2, add3)). However, \code{`rbind<-`} was not created
#' for R programming use when computational efficiency is valued; it is created
#' for R interactive use when user convenience is valued.
#'
#' Similar to \code{`rbind`}, \code{`rbind<-`} works with both data.frames and matrices.
#' This is because \code{`rbind`} is a generic function with a default method that
#' works with matrices and a data.frame method that works with data.frames. Similar
#' to \code{`rbind`}, if rownames of \code{value} are not given and \code{row.nm}
#' is left NULL, then the rownames of the return object are automatically created
#' and can be dissatisfying.
#'
#' @param data data.frame or matrix of data.
#'
#' @param after either an integer vector with length 1 or a character vector of
#' length 1 specifying where to add \code{value}. If an integer vector, it is the
#' position of a row. If a character vector it is the row with that name.
#' Similar to \code{append}, use 0L if you want the added rows to be first.
#'
#' @param row.nm character vector of length equal to \code{NROW(value)} that
#' specifies the rownames of \code{value} once added to \code{data} as columns.
#' This is an optional argument that defaults to NULL where the pre-existing rownames
#' of \code{value} are used.
#'
#' @param overwrite logical vector of length 1 specifying whether rows from
#' \code{value} or \code{row.nm} should overwrite rows in \code{data} with the
#' same rownames. Note, if \code{overwrite} = FALSE, R will prevent repeat rownames
#' by adding "1" to the end of the repeat rownames similar to \code{rbind}.
#'
#' @param value data.frame, matrix, or atomic vector to be added as rows to
#' \code{data}. If a data.frame or matrix, it must have the same ncol as \code{data}.
#' If an atomic vector, it must have length equal to ncol of \code{data}.
#'
#' @return Like other similar functions (e.g., \code{`names<-`} and \code{`[<-`}),
#' \code{`rbind<-`} does not appear to have a return object. However, it technically
#' does as a side effect. The argument \code{data} will have been changed such that
#' \code{value} has been added as rows. If a traditional return object is desired,
#' and no side effects, then it can be called like a traditional function:
#' dat2 <- `rbind<-`(dat1, value = add1).
#'
#' @examples
#' attitude2 <- attitude
#' rbind(attitude2) <- colMeans(attitude2) # defaults to rownames = as.character(nrow(`data`) + 1)
#' attitude2 <- attitude2[!(`%in%`(x = row.names(attitude2), table = "31")), ] # logical subset
#' rbind(attitude2, row.nm = "mean") <- colMeans(attitude2)
#' attitude2 <- attitude2[-1*(match(x = "mean", table = row.names(attitude2))), ] # position subset
#' rbind(attitude2, after = "10", row.nm = c("mean","sum")) <-
#'    rbind(colMeans(attitude2), colSums(attitude2)) # `value` as a matrix
#' attitude2 <- attitude2[grep(pattern = "mean|sum", x = row.names(attitude2),
#'    invert = TRUE), ] # rownames subset
#' attitude2 <- `rbind<-`(data = attitude2, value = colMeans(attitude2)) # traditional call
#' attitude2 <- as.matrix(attitude, rownames.force = TRUE) # as.matrix.data.frame
#' rbind(attitude2, after = "10", row.nm = "mean") <- colMeans(attitude2) # `data` as a matrix
#' # using overwrite
#' mtcars2 <- mtcars
#' rownames(mtcars2)
#' add <- mtcars[c("Mazda RX4","Mazda RX4 Wag","Datsun 710"), ]*11
#' rbind(mtcars2, overwrite = TRUE) <- add
#' mtcars2 <- mtcars
#' rbind(mtcars2, overwrite = FALSE) <- add
#' @export
`rbind<-` <- function(data, after = nrow(data), row.nm = NULL, overwrite = TRUE, value) {

   if (!(is.null(row.nm))) {
      if (is.vector(value) && !(is.data.frame(value))) value <- t(as.matrix(value))
      rownames(value) <- row.nm
   }
   Nrow <- nrow(data)
   if (overwrite == TRUE) {
      same_names <- intersect(x = rownames(data), y = rownames(value))
      if (length(same_names) > 0) {
         data[same_names, ] <- value[same_names, ]
         value <- value[!(is.element(el = rownames(value), set = same_names)), , drop = FALSE]
         if (nrow(value) == 0) return(data) # when all rows in `value` have the same rownames as rows in `data`
      }
   }
   if (is.character(after)) after <- match(x = after, table = rownames(data))
   if (after == 0L) output <- rbind(value, data)
   if (after == Nrow) output <- rbind(data, value)
   if (after != 0L && after != Nrow) {
      data_before <- data[1L:after, ]
      data_after <- data[(after + 1L):(Nrow), ]
      output <- rbind(data_before, value, data_after)
   }
   return(output)
}

# TRY ####

# try_fun #

#' Add Try to Function
#'
#' \code{try_fun} creates a version of the function \code{fun} that evaluates the
#' function and then returns a list with three elements: 1) return object, 2) warning
#' message, 3) error message. This can be useful when you want to apply a function
#' and are not sure if it will result in a warning and/or error and don't want R
#' to stop if an error does arise.
#'
#' This function is heavily based on the following StackOverflow post:
#' https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function.
#'
#' @param fun function
#'
#' @param output.class character vector of length 1 specifying the class you want
#' the result from a call to the returned function to be. Note, if \code{fun}
#' is an annonymous function, then the default will probably not work due to the
#' character limitations of deparsing a function. You can always put down NULL for
#' no class, which will always work with annonymous functions.
#'
#' @return function that returns a list object with three elements: "result" = 1)
#' return object of \code{fun}, "warning" = warning message, "error" = error message.
#' When an element is not relevant (e.g., no errors), then that element is NULL.
#'
#' @examples
#' # apply to log()
#' log.try <- try_fun(log)
#' log.try(1)
#' log.try(0)
#' log.try(-1)
#' log.try("a")
#' # return a list where NULL if an error or warning appears
#' lapply(X = list("positive" = 1, "zero" = 0, "negative" = -1,"letter" = "a"),
#'    FUN = function(x) {
#'       log_try <- log.try(x)
#'       result <- log_try[["result"]]
#'       warning <- log_try[["warning"]]
#'       error <- log_try[["error"]]
#'       if (!(is.null(error))) return(NULL)
#'       if (!(is.null(warning))) return(NULL)
#'       return(result)
#' })
#' @export
try_fun <- function(fun, output.class = paste0(deparse(substitute(fun)), ".try")) {

   function(...) {
      err_output <- NULL
      warn_output <- NULL
      err_fun <- function(e) {
         err_output <<- conditionMessage(e)
         NULL
      }
      warn_fun <- function(w) {
         warn_output <<- append(warn_output, conditionMessage(w))
         invokeRestart("muffleWarning")
      }
      res <- withCallingHandlers(tryCatch(
         fun(...),
         error = err_fun),
         warning = warn_fun)
      output <- list("result" = res, "warning" = warn_output, "error" = err_output)
      class(output) <- output.class
      return(output)
   }
}

# try_expr #

#' Add Try to Expression
#'
#' \code{try_expr} evaluates an expression \code{expr} and returns a list with three
#' elements: 1) return object, 2) warning message, 3) error message. This can be
#' useful when you want to evaluate an expression and are not sure if it will result
#' in a warning and/or error and don't want R to stop if an error does arise.
#'
#' This function is heavily based on the following StackOverflow post:
#' https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function.
#'
#' @param expr expression
#'
#' @param output.class character vector of length 1 specifying the class you want
#' the returned object of \code{try_expr} to be. The default is NULL for no class.
#'
#' @return list object with three elements: "result" = 1) return object of \code{expr},
#' "warning" = warning message, "error" = error message. When an element is not
#' relevant (e.g., no errors), then that element is NULL.
#'
#' @examples
#' # apply to log()
#' try_expr(log(1))
#' try_expr(log(0))
#' try_expr(log(-1))
#' try_expr(log("a"))
#' # return a list where NULL if an error or warning appears
#' lapply(X = list("positive" = 1, "zero" = 0, "negative" = -1,"letter" = "a"),
#'    FUN = function(x) {
#'       log_try <- try_expr(log(x))
#'       result <- log_try[["result"]]
#'       warning <- log_try[["warning"]]
#'       error <- log_try[["error"]]
#'       if (!(is.null(error))) return(NULL)
#'       if (!(is.null(warning))) return(NULL)
#'       return(result)
#' })
#' @export
try_expr <- function(expr, output.class = NULL) {

   err_output <- NULL
   warn_output <- NULL
   err_fun <- function(e) {
      err_output <<- conditionMessage(e)
      NULL
   }
   warn_fun <- function(w) {
      warn_output <<- append(warn_output, conditionMessage(w))
      invokeRestart("muffleWarning")
   }
   res <- withCallingHandlers(tryCatch(
      expr,
      error = err_fun),
      warning = warn_fun)
   output <- list("result" = res, "warning" = warn_output, "error" = err_output)
   class(output) <- output.class
   return(output)
}

# MISC ####

# nlay #

#' Number of Layers (the Third Dimension)
#'
#' \code{nlay} returns the number of layers - the third dimension - of an array.
#' If the object does not have a third dimension (e.g., matrix), then the function
#' will return NA with typeof = integer. If the object does not have any dimensions
#' (e.g., atomic vector), then the function will return NULL.
#'
#' R does not have standard terminology for the third dimension. There are several common terms
#' people use including "height" and "page". I personally prefer "layer" as it makes sense
#' whether the user visualizes the third dimension as going into/ontop a desk or into/ontop a wall.
#'
#' @param x array.
#'
#' @return The number of layers (the third dimension) of \code{x}. The structure is
#' an integer vector with length = 1. See details for special cases.
#'
#' @examples
#' nlay(HairEyeColor)
#' a <- array(data = NA, dim = c(6,7,8,9))
#' nlay(a)
#' @export
nlay <- function(x) {
   dim(x)[3L]
}

# ndim #

#' Number of Object Dimensions
#'
#' \code{ndim} returns the number of dimensions an object has. This is most useful
#' for arrays, which can have anywhere from 1 to 1000+ dimensions.
#'
#' \code{ndim} is a very simple function that is simply \code{length(dim(x))}.
#'
#' @param x object that has dimensions (e.g., array).
#'
#' @return integer vector of length 1 specifying the number of dimensions in \code{x}.
#' If \code{x} does not have any dimensions, then O is returned.
#'
#' @examples
#' ndim(state.region)
#' ndim(attitude)
#' ndim(HairEyeColor)
#' @export
ndim <- function(x) {
   length(dim(x))
}

# dimlabels #

#' Dimension labels (i.e., names of dimnames)
#'
#' \code{dimlabels} returns the the dimension labels (i.e., names of dimnames)
#' of an object. This is most useful for arrays, which can have anywhere from 1
#' to 1000+ dimensions.
#'
#' \code{dimlabels} is a very simple function that is simply \code{names(dimnames(x))}.
#'
#' @param x object that has dimensions (e.g., array).
#'
#' @return character vector of length  = \code{ndim(x)} specifying the dimension
#' labels (i.e., names of dimnames) of \code{x}. If \code{x} does not have any
#' dimensions, or has dimensions but no dimension labels, then NULL is returned.
#'
#' @examples
#' dimlabels(state.region)
#' dimlabels(attitude)
#' dimlabels(HairEyeColor)
#' @export
dimlabels <- function(x) {
   names(dimnames(x))
}

# undim #

#' Undimension an Object
#'
#' \code{undim} removes all dimensions from an object. This is particularly useful
#' for simplifying 1D arrays where the dimnames from the array are used for the
#' returned object. Although the function can also be used when dimensions were
#' temporarily (or erroneously) given to an object.
#'
#' @param x object with dimensions (usually an array of some kind)
#'
#' @return \code{x} without any dimensions. If \code{x} is an array, then the return
#' object will be an atomic vector. If \code{x} is a 1D array, then the returned
#' vector will have names = the 1D dimnames.
#'
#' @examples
#' a <- array(NA, dim = 1, dimnames = list("A"))
#' v <- undim(a)
#' str(a); str(v)
#' @export
undim <- function(x) {
   rtn <- x
   dim(rtn) <- NULL
   x_dimnames <- dimnames(x)
   if (1L == length(x_dimnames)) names(rtn) <- x_dimnames[[1]]
   return(rtn)
}

# cat0

#' Concatenate and Print with No Separator
#'
#' \code{cat0} concatenates and prints objects without any separator. \code{cat0}
#' is to \code{cat} as \code{paste0} is to \code{paste}. It also allows you to
#' specify line breaks before (\code{n.before}) and after (\code{n.after}) the
#' the printing of the concatenated R objects. \code{cat0} function can be useful
#' in conjunction with \code{sink} for quick and dirty exporting of results.
#'
#' @param ... one or more R objects. See details of \code{cat} for types of objects
#' allowed.
#'
#' @param n.before integer vector of length 1 specifying how many line breaks to have
#' before printing the concatenated R objects.
#'
#' @param n.after integer vector of length 1 specifying how many line breaks to have
#' after printing the concatenated R objects.
#'
#' @param file A connection or a character string naming the file to print to.
#' If "" (default), \code{cat0} prints to the standard output connection - the
#' console - unless redirected by \code{sink}.
#'
#' @param fill A logical or (positive) numeric vector of length 1 controlling how
#' the output is broken into successive lines. If FALSE (default), new line breaks are
#' only created explicity by \code{"\\n"} bring called. If TRUE, the output is broken into
#' lines with print width equal to the option "width" (\code{options()[["width"]]}).
#' If a (positive) number, then the output is broken after width of that length.
#'
#' @param labels A character vector of labels for the lines printed. Ignored if
#' \code{fill} = FALSE.
#'
#' @param append A logical vector of length 1. Only used if the argument \code{file}
#' is the name of a file (and not a connection). If TRUE, output will be appended
#' to the existing file. If FALSE, output will overwrite the contents of the file.
#'
#' @return nothing as the function only prints and does not return an R object.
#'
#' @examples
#' cat0(names(attitude))
#' cat0("MODEL COEFFICIENTS:", coef(lm(rating ~ critical + advance, data = attitude)),
#'    n.before = 0, n.after = 2)
#' @export
cat0 <- function(..., n.before = 1L, n.after = 1L, file = "", fill = FALSE,
   labels = NULL, append = FALSE) {

   cat(rep.int("\n", times = n.before), ..., rep.int("\n", times = n.after),
      file = file, fill = fill, labels = labels, append = append)
}

# sn #

#' Set a Vector's Names as its Elements

#' \code{sn} sets a vector's names as its elements. It is a simple utility function
#' equal to \code{setNames(x, nm = as.character(x))}. This is particularly useful
#' when using \code{lapply} and you want the return object to have \code{X} as its names.
#'
#' @param x atomic or list vector.
#'
#' @return \code{x} with the elements of \code{x} as its names.
#'
#' @examples
#' sn(1:10)
#' sn(c("one","two","three"))
#' @export
sn <- function(x) {
   setNames(x, nm = as.character(x))
}

# inbtw #

#' Elements Inbetween Values Within a (Atomic) Vector
#'
#' \code{inbtw} extracts all elements inbetween (by position) two specific elements
#' of a (atomic) vector. This can be useful when working with rownames and colnames
#' since \code{seq} does not work with names. Primary for character vectors but
#' can be used with other typeof.
#'
#' An error is returned if either \code{from} or \code{to} don't appear in \code{x}
#' or appear more than once in \code{x}.
#'
#' @param x atomic vector.
#'
#' @param from vector of length 1 specifying the element to start with on the left.
#'
#' @param to vector of length 1 specifying the element to end with on the right.
#'
#' @param left logical vector of length 1 specifying whether the leftmost element,
#' \code{from}, should be included in the return object.
#'
#' @param right logical vector of length 1 specifying whether the rightmost element,
#' \code{to}, should be included in the return object.
#'
#' @return vector of the same type as \code{x} that only includes elements in \code{x}
#' inbetween (by position) \code{from} and \code{to}, which may or may not include
#' \code{from} and \code{to} themselves, depending on \code{left} and \code{right},
#' respectively.
#'
#' @examples
#' # character vector
#' row_names <- inbtw(x = row.names(LifeCycleSavings), from = "China", to = "Peru")
#' LifeCycleSavings[row_names, ] # default use
#' row_names <- inbtw(x = row.names(LifeCycleSavings), from = "China", to = "Peru",
#'    right = FALSE, left = FALSE)
#' LifeCycleSavings[row_names, ] # use with right and left arguments FALSE
#' try_expr(inbtw(x = row.names(LifeCycleSavings), from = "china",
#'    to = "peru")) # error due to `from` and `to` not appearing in `x`
#' try_expr(inbtw(x = rep.int(x = row.names(LifeCycleSavings), times = 2), from = "China",
#'    to = "Peru")) # error due to `from` and `to` appearing more than once in `x`
#' # numeric vector
#' vec <- sample(x = 150:199, size = 50)
#' inbtw(x = vec, from = 150, to = 199)
#' # list vector (error)
#' lst <- list(FALSE, 3L, 9.87, "abc", factor("lvl"))
#' try_expr(inbtw(x = lst, from = 3L, to = "abc")) # error because `lst` is a
#'    # list vector and not an atomic vector
#' @export
inbtw <- function(x, from, to, left = TRUE, right = TRUE) {

   if (!checkmate::testAtomicVector(x))
      stop("`x` must be an atomic vector")
   table_x <- table(x)
   from_chr <- as.character(from) # to allow for numeric vectors
   to_chr <- as.character(to)
   if (is.na(table_x[from_chr]) || is.na(table_x[to_chr]))
      stop("either `from` and/or `to` are not present in `x`")
   if (table_x[from_chr] > 1 || table_x[to_chr] > 1)
      stop("either `from` and/or `to` are present more then once in `x`")
   begin <- match(x = from, table = x)
   finish <- match(x = to, table = x)
   rtn <- x[begin:finish]
   if (!left) rtn <- rtn[-1]
   if (!right) rtn <- rtn[-length(rtn)]
   return(rtn)
}

# pick #

#' Extract Elements From a (Atomic) Vector
#'
#' \code{pick} extracts the elements from a (atomic) vector that meet certain criteria:
#' 1) using exact values or regular expressions (\code{pat}), 2) inclusion vs.
#' exclusion of the value/expression (\code{not}), 3) based on elements or names (\code{nm}).
#' Primarily for character vectors, but can be used with other typeof.
#'
#' \code{pick} allows for 8 different ways to extract elements from a (atomic) vector
#' created by the 2x2x2 combination of logical arguments \code{pat}, \code{not}, and \code{nm}.
#' When \code{pat} = FALSE (default), \code{pick} uses \code{is.element} (essentially
#' \code{match}) and requires exact matching of \code{val} in \code{x}. When \code{pat}
#' = TRUE, \code{pick} uses \code{grepl} and allows for partial matching of \code{val}
#' in \code{x} and/or regular expressions if \code{fixed} = FALSE (default).
#'
#' When dealing with regular expressions via \code{pat} = TRUE and \code{fixed} = FALSE,
#' certain symbols within \code{val} are not interpreted as literal characters and
#' instead have special meanings. Some of the most commonly used symbols are \code{.}
#' = any character, \code{"|"} = logical or, \code{"^"} = starts with, \code{"\\n"} = new line,
#' \code{"\\t"} = tab.
#'
#' @param x atomic vector or an object with names (e.g., data.frame) if \code{nm} = TRUE.
#'
#' @param val atomic vector specifying which elements of \code{x} will be extracted.
#' If \code{pat} = FALSE (default), then \code{val} should be an atomic vector of
#' the same typeof as \code{x}, can have length > 1, and exact matching will be done
#' via \code{is.element} (essentially \code{match}). If \code{pat} = TRUE, then
#' \code{val} has to be a character vector of length 1 and partial matching will be
#' done via \code{grepl} with the option of regular expressions if \code{fixed} = FALSE
#' (default). Note, if \code{nm} = TRUE, then \code{val} should refer to names of
#' \code{x} to determine which elements of \code{x} should be extracted.
#'
#' @param pat logical vector of length 1 specifying whether \code{val} should refer to
#' exact matching (FALSE) via \code{is.element} (essentially \code{match}) or partial
#' matching (TRUE) and/or use of regular expressions via \code{grepl}. See details
#' for a brief description of some common symbols and \code{help(regex)} for more.
#'
#' @param not logical vector of length 1 specifying whether \code{val} indicates
#' values that should be retained (FALSE) or removed (TRUE).
#'
#' @param nm logical vector of length 1 specifying whether \code{val} refers to the
#' names of \code{x} (TRUE) rather than the elements of \code{x} themselves (FALSE).
#'
#' @param fixed logical vector of length 1 specifying whether \code{val} refers to
#' values as is (TRUE) or a regular expression (FALSE). Only used if \code{pat} = TRUE.
#'
#' @return a subset of \code{x} that only includes the elements which meet the criteria
#' specified by the function call.
#'
#' @examples
#' # pedagogical cases
#' chr <- setNames(object = c("one","two","three","four","five"), nm = as.character(1:5))
#' # 1) pat = FALSE, not = FALSE, nm = FALSE
#' pick(x = chr, val = c("one","five"), pat = FALSE, not = FALSE, nm = FALSE)
#' # 2) pat = FALSE, not = FALSE, nm = TRUE
#' pick(x = chr, val = c("1","5"), pat = FALSE, not = FALSE, nm = TRUE)
#' # 3) pat = FALSE, not = TRUE, nm = FALSE
#' pick(x = chr, val = c("two","three","four"), pat = FALSE, not = TRUE, nm = FALSE)
#' # 4) pat = FALSE, not = TRUE, nm = TRUE
#' pick(x = chr, val = c("2","3","4"), pat = FALSE, not = TRUE, nm = TRUE)
#' # 5) pat = TRUE, not = FALSE, nm = FALSE
#' pick(x = chr, val = "n|v", pat = TRUE, not = FALSE, nm = FALSE)
#' # 6) pat = TRUE, not = FALSE, nm = TRUE
#' pick(x = chr, val = "1|5", pat = TRUE, not = FALSE, nm = TRUE)
#' # 7) pat = TRUE, not = TRUE, nm = FALSE
#' pick(x = chr, val = "t|r", pat = TRUE, not = TRUE, nm = FALSE)
#' # 8) pat = TRUE, not = TRUE, nm = TRUE
#' pick(x = chr, val = c("2|3|4"), pat = TRUE, not = TRUE, nm = TRUE)
#' datasets <- data()[["results"]][, "Item"]
#' # actual use cases
#' pick(x = datasets, val = c("attitude","mtcars","airquality"),
#'    not = TRUE) # all but the three most common datasets used in `str2str` package examples
#' pick(x = datasets, val = "state", pat = TRUE) # only datasets that contain "state"
#' pick(x = datasets, val = "state.*state", pat = TRUE) # only datasets that have
#'    # "state" twice in their name
#' pick(x = datasets, val = "US|UK", pat = TRUE) # only datasets that contain
#'    # "US" or "UK"
#' pick(x = datasets, val = "^US|^UK", pat = TRUE) # only datasets that start with
#'    # "US" or "UK"
#' pick(x = datasets, val = "k.*o|o.*k", pat = TRUE) # only datasets containing both
#'    # "k" and "o"
#' @export
pick <- function(x, val, pat = FALSE, not = FALSE, nm = FALSE, fixed = FALSE) {
   # 1) pat = FALSE, not = FALSE, nm = FALSE
   if (!pat && !not && !nm)
      rtn <- x[is.element(el = x, set = val)]
   # 2) pat = FALSE, not = FALSE, nm = TRUE
   if (!pat && !not && nm)
      rtn <- x[is.element(el = names(x), set = val)]
   # 3) pat = FALSE, not = TRUE, nm = FALSE
   if (!pat && not && !nm)
      rtn <- x[!(is.element(el = x, set = val))]
   # 4) pat = FALSE, not = TRUE, nm = TRUE
   if (!pat && not && nm)
      rtn <- x[!(is.element(el = names(x), set = val))]
   # 5) pat = TRUE, not = FALSE, nm = FALSE,
   if (pat && !not && !nm)
      rtn <- x[grepl(pattern = val, x = x, fixed = fixed)]
   # 6) pat = TRUE, not = FALSE, nm = TRUE
   if (pat && !not && nm)
      rtn <- x[grepl(pattern = val, x = names(x), fixed = fixed)]
   # 7) pat = TRUE, not = TRUE, nm = FALSE
   if (pat && not && !nm)
      rtn <- x[!(grepl(pattern = val, x = x, fixed = fixed))]
   # 8) pat = TRUE, not = TRUE, nm = TRUE
   if (pat && not && nm)
      rtn <- x[!(grepl(pattern = val, x = names(x), fixed = fixed))]
   # return object
   return(rtn)
}

# grab #

#' \code{grab} extracts the contents of objects in an environment based on their
#' object names as a character vector. The object contents are stored to a list
#' where the names are the object names.
#'
#' @param x character vector providing the exact names of objects in the environment
#' \code{envir}.
#'
#' @param envir environment to pull the objects from. Default is the global environment.
#'
#' @return list of objects with names \code{x}.
#'
#' @examples
#' grab(x = c("attitude","mtcars","airquality"))
#' grab(x = c("mean.default","mean.Date","mean.difftime"))
#' @export
grab <- function(x, envir = sys.frame()) {

   rtn <- lapply(X = x, FUN = get, envir = envir)
   names(rtn) <- x
   return(rtn)
}

# t_list #
#'
#' Transpose a List
#'
#' \code{t_list} transposes a list, similar to what \code{t.default} does for matrices.
#' \code{t_list} assumes the structure of each \code{x} element is the same. Tests
#' are done to ensure the lengths and names are the same for each \code{x} element.
#' The returned list has list elements in the same order as in \code{x[[1]]}.
#'
#' If any element within \code{x} has no names (NULL), then the transposition is
#' done based on positions. If all element within \code{x} have the same names,
#' then the transposition is done based on those names.
#'
#' @param x list where each element has the same structure.
#'
#' @param rtn.atomic logical vector of length 1 specifying whether the returned list
#' should be a list of atomic vectors (TRUE) rather than a list of lists (FALSE).
#'
#' @return list where each element is from those in \code{x[[1]]} and each element
#' of the returned object has a subelement for each element in \code{x}.
#'
#' @examples
#' # modeling example
#' iris_bySpecies <- split(x = iris, f = iris$"Species")
#' lmObj_bySpecies <- lapply(X = iris_bySpecies, FUN = function(dat) {
#'    lm(Sepal.Length ~ Petal.Width, data = dat)})
#' lmEl_bySpecies <- t_list(lmObj_bySpecies)
#' summary(lmObj_bySpecies); summary(lmEl_bySpecies)
#' summary.default(lmEl_bySpecies[[1]]); summary.default(lmEl_bySpecies[[2]])
#' # no names
#' lmObj_bySpecies2 <- unname(lapply(X = lmObj_bySpecies, FUN = unname))
#' lmEl_bySpecies2 <- t_list(lmObj_bySpecies2)
#' summary(lmObj_bySpecies2); summary(lmEl_bySpecies2)
#' summary.default(lmEl_bySpecies2[[1]]); summary.default(lmEl_bySpecies2[[2]])
#' all(unlist(Map(name = lmEl_bySpecies, nameless = lmEl_bySpecies2,
#'    f = function(name, nameless) all.equal(unname(name), nameless)))) # is everything
#'    # but the names the same?
#' # atomic vector example
#' x <- list("A" = c("a"=1,"b"=2,"c"=3),"B" = c("a"=1,"b"=2,"c"=3),
#'    "C" = c("a"=1,"b"=2,"c"=3))
#' t_list(x, rtn.atomic = TRUE)
#' # names in different positions
#' x <- list("A" = c("a"=1,"b"=2,"c"=3),"B" = c("b"=2,"a"=1,"c"=3),
#'    "C" = c("c"=3,"b"=2,"a"=1))
#' t_list(x, rtn.atomic = TRUE)
#' # no names
#' x <- list(c(1,2,3), c(1,2,3), c(1,2,3))
#' t_list(x, rtn.atomic = TRUE)
#' @export
t_list <- function(x, rtn.atomic = FALSE) {

   # checks
   if (!(is.list(x))) stop("`x` must be a list")
   len_byel <- unlist(lapply(X = x, FUN = length))
   if (0 != var(len_byel))
      stop("each element of `x` must have the same length")
   nm_sort <- lapply(X = x, FUN = function(obj) sort(names(obj))) # sort so the names in the same order
   if (any(unlist(lapply(X = nm_sort, FUN = is.null))))
      named <- FALSE
   else
      named <- TRUE

   # named = TRUE
   if (named) {
      nm_rep <- lapply(X = 1:length(nm_sort[[1]]), FUN = function(i)
         unlist(lapply(X = nm_sort, FUN = `[[`, i = i)))
      if (!(all(unlist(lapply(X = nm_rep, FUN = all_same)))))
         stop("each element of `x` must have the same names")
      nm_first <- names(x[[1]]) # so that sub-elements are in the order from the first element rather than alphabetical
      t_x <- lapply(X = nm_first, FUN = function(nm) lapply(X = x, FUN = `[[`, i = nm))
      names(t_x) <- nm_first
   }

   # named = FALSE
   if (!named) {
      pos_el <- seq_along(x[[1]])
      t_x <- lapply(X = pos_el, FUN = function(i) lapply(X = x, FUN = `[[`, i = i))
   }

   # return object
   if (rtn.atomic)
      return(lapply(X = t_x, FUN = unlist, use.names = TRUE))
   else
      return(t_x)
}

# order.custom #

#' Custom Order Permutation
#'
#' \code{order.custom} creates the order of the positions in the atomic vectors
#' from \code{X} that would cause the atomic vectors from \code{X} to be sorted
#' according to the atomic vectors from \code{ORD}. This is analogus to the
#' \code{order} function, but instead of doing default sorting (e.g., 1, 2, 3, etc.
#' or "A", "B", "C", etc.), the sorting is customized by \code{ORD}.
#' \code{order.custom} does custom ordering by converting each atomic vector from
#' \code{X} to an ordered factor and then default sorting the ordered factors.
#'
#' Note, that the atomic vectors within \code{X} are always forward sequenced;
#' if backward sequence is desired, then the user should call \code{rev} on both
#' the input to \code{X} and \code{ORD}. This is analogous to reversing the
#' order of the atomic vectors given to \code{...} within \code{order}.
#'
#' @param X list of atomic vectors parellel matched with the atomic vectors in \code{X}
#' specifying the elements to be ordered. Can also be a single atomic vector, which
#' will internally be converted to a list with one element.
#'
#' @param ORD list of atomic vectors that do NOT have to be the same length
#' specifying the order of the unique values for sorting. Can also be a single
#' atomic vector, which will internally be converted to a list with one element.
#'
#' @param na.last logical vector of length 1 specifying whether missing values
#' should be put last (TRUE), first (FALSE), or removed (NA).
#'
#' @param decreasing logical vector of length 1 specifying whether the sorting
#' should start with the first element of the atomic vectors within \code{ORD}
#' and proceed forward (FALSE) or the last element of the atomic vectors within
#' \code{ORD} and proceed backwards (TRUE).
#'
#' @return integer vector of length = \code{X[[1]]} (after converting \code{X} to
#' a list with one element is need be) providing the revised order of the atomic
#' vectors within \code{X} that sorts them according to \code{ORD}.
#'
#' @examples
#' # character vector
#' x <- esoph[["tobgp"]]
#' order.custom(X = x, ORD = c("20-29","10-19","30+","0-9g/day"))
#' x[order.custom(X = x, ORD = c("20-29","10-19","30+","0-9g/day"))] # returns character
#' esoph[order.custom(X = x, ORD = c("20-29","10-19","30+","0-9g/day")), ]
#' # order by position
#' sort(state.region)
#' x <- as.character(state.region)
#' order.custom(X = x, ORD = unique(x))
#' x[order.custom(X = x, ORD = unique(x))]
#' # numeric vector
#' y <- esoph[["ncases"]]
#' order.custom(X = y, ORD = c(6,5,4,3,2,1,0,17,8,9))
#' y[order.custom(X = y, ORD = c(6,5,4,3,2,1,0,17,8,9))] # returns numeric
#' esoph[order.custom(X = y, ORD = c(6,5,4,3,2,1,0,17,8,9)), ]
#'    # some unique values not provided in `ORD` (appended at the end and sorted by
#'    # where they appear in the dataset)
#' y <- esoph[["ncases"]]
#' order.custom(X = y, ORD = c(6,5,4,3,2,1,0))
#' y[order.custom(X = y, ORD = c(6,5,4,3,2,1,0))] # returns numeric
#' esoph[order.custom(X = y, ORD = c(6,5,4,3,2,1,0)), ]
#' # multiple vectors
#' z <- esoph[c("agegp","alcgp","tobgp")]
#' ord <- order.custom(X = z, ORD = list(
#'    "agegp" = c("45-54","55-64","35-44","65-74","25-34","75+"),
#'    "alcgp" = c("40-79","80-119","0-39g/day","120+"),
#'    "tobgp" = c("10-19","20-29","0-9g/day","30+")))
#' esoph[ord, ]
#' @export
order.custom <- function(X, ORD, na.last = FALSE, decreasing = FALSE) {

   if (!(is.list(X))) X <- list(X)
   if (!(is.list(ORD))) ORD <- list(ORD)
   if (length(X) != length(ORD)) stop("`X` and `ORD` must have the same number of list elements.")
   X_fct <- Map(vec = X, ord = ORD, f = function(vec, ord) {
      vec_unique <- unique(vec, fromLast = FALSE) # unique.default
      set_diff <- setdiff(x = vec_unique, y = ord)
      if (length(set_diff) > 0L)
         ord <- c(ord, set_diff)
      factor(x = vec, levels = ord, ordered = TRUE)
   })
   X_fct[["na.last"]] <- na.last
   X_fct[["decreasing"]] <- decreasing
   rtn <- do.call(what = `order`, args = X_fct)
   return(rtn)
}

# stack2 #

#' Stack one Set of Variables from Wide to Long
#'
#' \code{stack2} converts one set of variables in a data.frame from wide to long format.
#' (If you want to convert *multiple* sets of variables from wide to long, see
#' \code{reshape}.) It is a modified version of \code{stack} that 1) adds a column for
#' the rownames, 2) returns character vectors rather than factors, 3) can return
#' additional (repeated) columns, and 4) can order by rownames original
#' positions rather than the variable names being stacked call order.
#'
#' \code{stack2} is also very similar to \code{reshape::melt.data.frame}. The
#' differences are that it 1) adds a column for the rownames, 2) returns character
#' vectors rather than factors, and 3) can order by rownames original positions
#' rather than the variable names being stacked call order.
#'
#' @param data data.frame of data.
#'
#' @param select.nm character vector of colnames from \code{data} specifying the
#' variables to be stacked.
#'
#' @param keep.nm optional argument containing a character vector of colnames from
#' \code{data} specifying the additional columns to be included in the return object.
#' These columns are repeated down the data.frame as they are not stacked together.
#' The default is NULL where no additional columns are included in the return object.
#'
#' @param rtn.el.nm character vector of length 1 specifying the name of the column
#' in the return object that corresponds to the elements of the stacked variables.
#'
#' @param rtn.vrbnames.nm character vector of length 1 specifying the name of the column
#' in the return object that corresponds to the names of the stacked variables.
#'
#' @param rtn.rownames.nm character vector of length 1 specifying the name of the column
#' in the return object that corresponds to the rownames.
#'
#' @param order.by.rownames logical vector of length 1 specifying whether the returned
#' data.frame should be ordered by the positions of the rownames (TRUE) or by the
#' positions of the names of the stacked variables (i.e., \code{select.nm}). Note,
#' the ordering is by the *positions*, not by alphabetical order. If that is desired,
#' convert the rownames to a (id) column and use \code{reshape::melt.data.frame}.
#'
#' @param stringsAsFactors logical vector of length 1 specifying whether the
#' \code{rtn.vrbnames.nm} and \code{rtn.rownames.nm} columns should be converted
#' to factors. Note, the factor levels are ordered by positions and not alphabetically
#' (see \code{v2fct}).
#'
#' @return data.frame with nrow = \code{nrow(data) * length(`select.nm`)} from stacking the
#' elements of \code{data[select.nm]} on top of one another. The first column is
#' the rownames with name \code{rtn.rownames.nm}, the second column is the names
#' of the stacked variables with name \code{rtn.vrbnames.nm}, the third column is
#' the stacked elements with name \code{rtn.el.nm}, and the additional columns
#' are those specified by \code{keep.nm}.
#'
#' @examples
#' stack2(data = mtcars, select.nm = c("disp","hp","drat","wt","qsec"),
#'    keep.nm = c("vs","am"))
#' stack2(data = mtcars, select.nm = c("disp","hp","drat","wt","qsec"),
#'    keep.nm = c("vs","am"), rtn.el.nm = "rating", rtn.vrbnames.nm = "item",
#'    rtn.rownames.nm = "row_names") # change the return object colnames
#' stack2(data = mtcars, select.nm = c("disp","hp","drat","wt","qsec"),
#'    keep.nm = pick(x = names(mtcars), val = c("disp","hp","drat","wt","qsec"),
#'    not = TRUE)) # include all columns from `data` in the return object
#' # compare to utils::stack.data.frame and reshape::melt.data.frame
#' ChickWeight2 <- ChickWeight
#' ChickWeight2$"Diet" <- as.integer(ChickWeight$"Diet")
#' x <- stack(x = ChickWeight2, select = c("weight","Diet")) # does not allow
#'    # keeping additional columns
#' y <- reshape::melt(data = ChickWeight2, measure.vars = c("weight","Diet"),
#'    id.nm = c("Chick","Time"), variable_name = "vrb_names") # does not include
#'    # rownames and not ordered by rownames
#' z <- stack2(data = ChickWeight2, select.nm = c("weight","Diet"),
#'    keep.nm = c("Chick","Time"))
#' head(x); head(y); head(z)
#' @export
stack2 <- function(data, select.nm, keep.nm = NULL, rtn.el.nm = "el",
   rtn.vrbnames.nm = "vrb_names", rtn.rownames.nm = "row_names",
   order.by.rownames = TRUE, stringsAsFactors = FALSE) {

   # stacked columns
   stacked <- rev(stack(x = data, select = select.nm)) # stack.data.frame
   names(stacked) <- c(rtn.vrbnames.nm, rtn.el.nm)
   stacked[[rtn.vrbnames.nm]] <- fct2v(fct = stacked[[rtn.vrbnames.nm]], simplify = FALSE)

   # keep columns
   n_select <- length(select.nm)
   if (!(is.null(keep.nm))) {
      tmp_keep <- replicate(n = n_select, expr = data[, keep.nm, drop = FALSE],
         simplify = FALSE)
      keep_dfm <- do.call(what = `rbind.data.frame`, args = tmp_keep) # rbind.data.frame
   }

   # rownames
   row_names <- row.names(data)
   rowNames_repVec <- rep.int(x = row_names, times = n_select)
   rowNames_repDfm <- setNames(data.frame(rowNames_repVec, stringsAsFactors = FALSE),
      nm = rtn.rownames.nm)

   # order rows
   rtn <- cbind.data.frame(rowNames_repDfm, stacked, keep_dfm)
   if (order.by.rownames)
      ord <- order.custom(X = rtn[c(rtn.rownames.nm, rtn.vrbnames.nm)], ORD = list(row_names, select.nm))
   else
      ord <- order.custom(X = rtn[c(rtn.vrbnames.nm, rtn.rownames.nm)], ORD = list(select.nm, row_names))
   rtn <- rtn[ord, ]

   # factors
   if (stringsAsFactors) {
      rtn[[rtn.rownames.nm]] <- v2fct(v = rtn[[rtn.rownames.nm]], order.lvl = "position")
      rtn[[rtn.vrbnames.nm]] <- v2fct(v = rtn[[rtn.vrbnames.nm]], order.lvl = "position")
   }

   # return object
   row.names(rtn) <- seq.int(from = 1, to = nrow(rtn), by = 1)
   return(rtn)
}

# unstack2 #

#' Unstack one Set of Variables from Long to Wide
#'
#' \code{unstack2} converts one set of variables in a data.frame from long to wide format.
#' (If you want to convert multiple sets of variables from long to wide, see
#' \code{reshape}.) It is a modified version of \code{unstack} that 1) requires a
#' column for the rownames of the data.frame (or equivalently an id column with
#' unique values for each row in the wide format) before it was stacked, 2) can
#' retain additional columns not being unstacked, and 3) can order by rownames
#' original positions rather than their alphanumerical order.
#'
#' \code{unstack2} is also very similar to \code{reshape::cast.data.frame}. The
#' differences are that it 1) can return the rownames as rownames of the returned
#' data.frame rather than an id column, 2) can retain additional columns not being
#' unstacked, and 3) can order by rownames original positions rather than the variable
#' names being stacked call order.
#'
#' @param data data.frame of data containing stacked variables.
#'
#' @param rownames.nm character vector of length 1 specifying the colname in
#' \code{data} for whom its unique values correspond to the rows in the return object.
#'
#' @param vrbnames.nm character vector of length 1 specifying the colname in
#' \code{`data`} that contains the names of the variables to be unstacked.
#'
#' @param el.nm character vector of length 1 specifying the colname in \code{data}
#' containing the elements from the variable to be unstacked.
#'
#' @param keep.nm optional argument containing a character vector of colnames from
#' \code{data} specifying the additional columns to be included in the return object.
#' The default is all the other columns in the data.frame besides \code{rownames.nm},
#' \code{vrbnames.nm}, and \code{el.nm}. If NULL, then no additional columns are retained.
#' The \code{keep.nm} columns will be the last (aka most right) columns in the return object.
#'
#' @param rownamesAsColumn logical vector of length 1 specifying whether the unique
#' values in \code{rownames.nm} column should be a column in the return object (TRUE)
#' or the rownames of the return object (FALSE).
#'
#' @return data.frame with nrow = \code{length(unique(data[[rownames.nm]]))} from
#' unstacking the elements of \code{el.nm} alongside one another. New columns are
#' created for each unique values in \code{vrbnames.nm} as well as columns for any
#' colnames additional specified by \code{keep.nm}. If \code{rownamesAsColumn} = TRUE,
#' then the first column are the unique values in \code{rownames.nm}; otherwise,
#' they are the rownames of the return object (default).
#'
#' @examples
#' # ordered by rownames
#' stacked <- stack2(data = mtcars, select.nm = c("disp","hp","drat","wt","qsec"),
#'    keep.nm = c("vs","am"), order.by.rownames = TRUE)
#' x <- unstack2(stacked)
#' # ordered by vrbnames
#' stacked <- stack2(data = mtcars, select.nm = c("disp","hp","drat","wt","qsec"),
#'    keep.nm = c("vs","am"), order.by.rownames = FALSE)
#' y <- unstack2(stacked)
#' identical(x, y)
#' # rownames as a column
#' z <- unstack2(data = stacked, rownamesAsColumn = TRUE)
#' # compare to utils::unstack.data.frame and reshape::cast.data.frame
#' stacked <- stack2(data = mtcars, select.nm = c("disp","hp","drat","wt","qsec"),
#'    keep.nm = c("vs","am"))
#' x <- unstack(x = stacked, form = el ~ vrb_names) # not able to keep additional variables
#' y <- reshape::cast(data = stacked, formula = row_names ~ vrb_names,
#'    value = "el") # automatically sorts the rownames and does not include extra columns
#' z <- unstack2(stacked)
#' head(x); head(y); head(z)
#' @export
unstack2 <- function(data, rownames.nm = "row_names", vrbnames.nm = "vrb_names", el.nm = "el",
   keep.nm = pick(x = names(data), val = c(rownames.nm, vrbnames.nm, el.nm), not = TRUE),
   rownamesAsColumn = FALSE) {

   form <- reformulate(termlabels = vrbnames.nm, response = el.nm)
   unstacked <- unstack(x = data, form = form) # unstack.data.frame
   row_vrb <- data[c(vrbnames.nm, rownames.nm)]
   row_vrb_unique <- lapply(X = row_vrb, FUN = unique)
   if (!(is.null(keep.nm))) {
      order_custom <- order.custom(X = row_vrb, ORD = row_vrb_unique)
      data_ordered <- data[order_custom, ]
      data_keep <- data_ordered[1:nrow(unstacked), keep.nm, drop = FALSE]
      unstacked <- cbind(unstacked, data_keep) # cbind.data.frame
   }
   if (rownamesAsColumn) {
      rtn <- cbind(setNames(data.frame("tmp" = row_vrb_unique[[2]]), nm = rownames.nm),
         unstacked) # cbind.data.frame
      row.names(rtn) <- seq.int(from = 1L, to = nrow(rtn), by = 1L)
   } else {
      rtn <- unstacked
      row.names(rtn) <- row_vrb_unique[[2]]
   }
   return(rtn)
}

# FACTORS ####

# v2fct #

#' Character Vector to (Unordered) Factor
#'
#' \code{v2fct} converts a character vector to a (unordered) factor. It goes
#' beyond \code{as.factor} by allowing you to specify how you want the levels ordered
#' and whether you want NA treated as a level.
#'
#' When \code{order.lvl} = "alpanum" the levels are sorted alphabetically if letters
#' or a combination of letters and numbers/numerals are in present in \code{v}.
#' If only numbers/numerals are present in \code{v}, then levels are sorted numerically.
#'
#' @param v character vector. If it is not a character vector (e.g., factor,
#' numeric vector), then it is coerced to a character vector within \code{v2fct}.
#'
#' @param order.lvl character vector of length 1 specifying how you want to order
#' the levels of the factor. The options are "alphanum", which sorts the levels
#' alphanumerically (with NA last); "position", which sorts the levels by the position
#' the level first appears; "frequency", which sorts the levels by their frequency.
#' If any frequencies are tied, then the ties are sorted alphanumerically (with NA last).
#'
#' @param decreasing logical vector of length 1 specifying whether the ordering of the
#' levels should be decreasing (TRUE) rather than increasing (FALSE).
#'
#' @param na.lvl logical vector of length 1 specifying if NA should be considered a level.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{v} is an atomic vector.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return factor of length = \code{length(x)} and names = \code{names(x)}.
#'
#' @examples
#' # no missing values
#' state_region <- as.character(state.region)
#' v2fct(state_region, order.lvl = "position") # in position order
#' v2fct(v = state_region, order.lvl = "frequency",
#'    decreasing = TRUE) # most frequent to least frequent
#' v2fct(v = state_region, order.lvl = "alphanum") # in alphanumerical order
#' v2fct(v = state_region, na.lvl = TRUE) # na.lvl is inert because no NAs in `v`
#' # with missing values
#' state_region <- c(NA_character_, as.character(state.region), NA_character_)
#' v2fct(v = state_region, order.lvl = "position", na.lvl = TRUE)
#' v2fct(v = state_region, order.lvl = "frequency", decreasing = TRUE, na.lvl = TRUE)
#' v2fct(v = state_region, order.lvl = "alphanum", na.lvl = TRUE)
#' identical(x = v2fct(v = state_region, order.lvl = "alphanum"),
#'    y = as.factor(state_region)) # equal to as.factor()
#' # numeric vectors
#' v2fct(v = round(faithful$"eruptions"), order.lvl = "position")
#' v2fct(v = round(faithful$"eruptions"), order.lvl = "frequency", decreasing = TRUE)
#' v2fct(v = round(faithful$"eruptions"), order.lvl = "alphanum")
#' # cnumeric vectors
#' cnum <- c("100","99","10","9","1","0","100","99","10","9","1","0")
#' factor(cnum) # not in numerical order
#' v2fct(v = cnum, order.lvl = "alphanum") # yes in numerical order
#' # ties on frequency
#' v2fct(v = rev(npk$"block"), order.lvl = "alphanum") # ties sorted alphanumerically
#' v2fct(v = rev(npk$"block"), order.lvl = "position") # no possibility of ties
#' @export
v2fct <- function(v, order.lvl = "position", decreasing = FALSE, na.lvl = FALSE,
   check = TRUE) {

   if (check) {
      checkmate::assertAtomicVector(v) # factors are considered atomic vectors
      order.lvl <- match.arg(arg = order.lvl,
         choices = c("alphanum","position","frequency"), several.ok = FALSE)
      checkmate::assertLogical(decreasing, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(na.lvl, any.missing = FALSE, len = 1L)
   }
   if (!(is.character(v))) v <- as.character(v)
   unique_values <- unique(v)
   if (!na.lvl) unique_values <- na.omit(unique_values)
   if (order.lvl == "alphanum") {
      if (is.cnumeric(unique_values))
         lvl <- as.character(sort.int(as.numeric(unique_values), na.last = TRUE))
      else
         lvl <- sort.int(unique_values, na.last = TRUE)
   }
   if (order.lvl == "position") {
      lvl <- unique_values
   }
   if (order.lvl == "frequency") {
      if (na.lvl) i <- addNA(v) else i <- v
      len <- c(tapply(X = v, INDEX = i, FUN = length)) # c() to get rid of the single dimension
      lvl <- names(sort.int(len))
   }
   if (decreasing) lvl <- rev(lvl)
   fct <- factor(x = v, levels = lvl, exclude = NULL, ordered = FALSE) # exclude = NULL is needed to allow NA as a level
   names(fct) <- names(v)
   return(fct)
}

# fct2v

#' Factor to (Atomic) Vector
#'
#' \code{fct2v} converts a factor to an (atomic) vector. It allows the user to specify
#' whether they want the factor to always return a character vector (\code{simplify = TRUE}),
#' simplified if possible (\code{simplify = FALSE}), or just return the integer codes
#' (\code{codes = TRUE}).
#'
#' When \code{simplify = TRUE}, \code{fct2v} uses \code{type.convert} to try to simplify
#' the factor. Note, missing values are assumed to be "NA" and decimals are assumed
#' to be "."; however, "L" after a number is not interpreted as an integer specifier.
#'
#' @param fct factor.
#'
#' @param simplify logical vector of length 1 specifying whether R should attempt to
#' simplify \code{fct} to typeof simplier than character (e.g., logical, integer, double).
#' If FALSE, a character vector is always returned.
#'
#' @param codes logical vector of length 1 specifying whether the integer codes of
#' \code{fct} should be returned. If \code{codes} = TRUE, then \code{simplify} is ignored.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{fct} is a factor.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return (atomic) vector of the same length as \code{fct}. If \code{codes} = TRUE,
#' then the returned vector is typeof integer containing the underlying factor codes.
#' If \code{codes} = FALSE and \code{simplify} = FALSE, then the returned vector is
#' typeof character containing the factor levels. If \code{codes} = FALSE, and
#' \code{simplify} = TRUE, then the returned vector is the simpliest typeof possible
#' without having to coerce any elements to NA. For example, if \code{fct} contains
#' all integer numerals (e.g., "1", "2", "3", etc), then it will be converted to an
#' integer vector. See examples.
#'
#' @examples
#' fct2v(state.region)
#' fct2v(fct = factor(c("7.00001","8.54321","9.99999"))) # double
#' fct2v(fct = factor(c("7","8","9")), simplify = FALSE) # character
#' fct2v(fct = factor(c("7","8","9")), simplify = TRUE) # integer
#' fct2v(fct = factor(c("7","8","9")), codes = TRUE) # integer codes
#' fct2v(fct = factor(c("7L","8L","9L")),
#'    simplify = TRUE) # does not understand "L" for integers
#' @export
fct2v <- function(fct, simplify = TRUE, codes = FALSE, check = TRUE) {
   if (check) {
      checkmate::assertFactor(fct)
      checkmate::assertLogical(simplify, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(codes, any.missing = FALSE, len = 1L)
   }
   if (codes) return(as.integer(fct))
   if (!simplify) return(as.character(fct)) # as.character.factor
   if (simplify) return(type.convert(x = fct, as.is = TRUE)) # type.convert.default
}

# codes #

#' Integer Codes of Factor Levels
#'
#' \code{codes} returns the integer codes for each factor level from a factor.
#'
#' @param fct factor.
#'
#' @return integer vector with length = \code{length(levels(fct)}, elements = integer
#' codes of \code{fct} and names = \code{levels(fct)}.
#'
#' @examples
#' codes(state.region)
#' codes(iris$"Species")
#' @export
codes <- function(fct) {
   lvl <- levels(fct)
   rtn <- setNames(seq_along(lvl), nm = lvl)
   return(rtn)
}

# FORMULAS ####

# v2frm #

#' Character Vector to Formula
#'
#' \code{v2frm} converts a character vector to a formula. The formula has the
#' simple structure of y ~ x1 + x2 + x3 + ... + xn. This function is a simple
#' wrapper for \code{reformulate}.
#'
#' @param v character vector of term(s) and/or response to be included on either
#' side of the returned formula. If it is not a character vector (e.g., factor,
#' numeric vector), then it is coerced to a character vector within \code{v2frm}.
#' Note, if the length of \code{v} is 1, then \code{y.which} must be NULL because
#' at least one term on the right hand side is required, otherwise an error is returned.
#'
#' @param y character vector of length 1 specifying the value of the element
#' within \code{v}, or integer of length 1 specifying the position of the element
#' within \code{v}, that is the response to be placed on the left hand side of the
#' returned formula. If NULL, then no elements of \code{v} are treated as response(s)
#' and the left hand side is empty.
#'
#' @param intercept logical vector of length 1 specifying whether the intercept should
#' be included in the returned formula. The default is TRUE and no change is made
#' to the returned formula. If FALSE, then a -1 is added to the end of the right hand side.
#'
#' @return formula with element \code{v[y]} on the left hand side and \code{v[-y]}
#' elements on the right hand side (rhs) separated by plus signs (+) with a -1
#' if \code{intercept} = FALSE.
#'
#' @examples
#' v2frm(v = names(attitude))
#' v2frm(v = names(attitude), y = 7L)
#' v2frm(v = names(attitude), y = NULL)
#' v2frm(v = "rating", y = NULL)
#' try_expr(v2frm(v = "rating")) # error is returned
#' @export
v2frm <- function(v, y = 1L, intercept = TRUE) {

   if (!(is.character(v))) v <- as.character(v)
   if (is.null(y)) {
      return(reformulate(termlabels = v, response = NULL, intercept = intercept))
   }
   else {
      if (is.character(y)) {
         y_which <- which(v == y)
         if (length(y_which) > 1) stop("If `y` is a value within `v`, then it can only appear once.")
      } else y_which <- y
      return(reformulate(termlabels = v[-y_which], response = v[+y_which], intercept = intercept))
   }
}

# VECTOR2 ####

# v2m #

#' (Atomic) Vector to Matrix
#'
#' \code{v2m} converts an (atomic) vector to a single row or single column matrix.
#' The matrix will be the same typeof as the atomic vector. The benefit of \code{v2m}
#' over \code{as.matrix.default} is that the dimension along which the vector is binded
#' can be either rows or columns, whereas in \code{as.matrix.default} it can only
#' be binded along a column.
#'
#' @param v (atomic) vector.
#'
#' @param along numeric vector of length 1 that is equal to either 1 or 2 specifying
#' which dimension to bind \code{v} along. 1 means that \code{v} is binded along
#' rows (i.e., dimension 1) into a one row matrix. 2 means that \code{v} is binded
#' along columns (i.e., dimension 2) into a one column matrix.
#'
#' @param rtn.dim.nm character vector of length 1 specifying what dimname to use
#' for the dimension of length 1 in the returned matrix. If \code{along} = 1,
#' then \code{rtn.dim.nm} will be the single rowname. If \code{along} = 2, then
#' \code{rtn.dim.nm} will be the single colname. If NULL, then the dimension of
#' length 1 has no dimname.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{v} is an atomic vector.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return matrix with typeof = \code{typeof(v)}. If \code{along} = 1, then the
#' dimensions = \code{c(1L, length(v))} and dimnames = \code{list(rtn.dim.nm, names(v))}.
#' If \code{along} = 2, then the dimensions = \code{c(length(v), 1L)} and dimnames =
#' \code{list(names(v), rtn.dim.nm)}.
#'
#' @examples
#' mtcars2 <- as.matrix(mtcars, rownames.force = TRUE) # to make sure dimnames stay in the example
#' v2m(mtcars2[, "mpg"])
#' identical(x = v2m(mtcars2[, "mpg"]),
#'    y = as.matrix(mtcars2[, "mpg"])) # default = as.matrix.default()
#' v2m(mtcars2[, "mpg"], along = 1)
#' identical(x = v2m(mtcars2[, "mpg"], along = 1),
#'    y = t(as.matrix(mtcars2[, "mpg"]))) # = t(as.matrix.default())
#' v2m(v = mtcars2[, "mpg"], rtn.dim.nm = "mpg")
#' @export
v2m <- function(v, along = 2, rtn.dim.nm = NULL, check = TRUE) {
   if (check) {
      checkmate::assertAtomicVector(v)
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertCharacter(rtn.dim.nm, any.missing = FALSE, len = 1L, null.ok = TRUE)
   }
   length_v <- length(v)
   if (along == 1) {
      m <- matrix(data = v, nrow = 1L, ncol = length_v,
         dimnames = list(rtn.dim.nm, names(v)))
   }
   if (along == 2) {
      m <- matrix(data = v, nrow = length_v, ncol = 1L,
         dimnames = list(names(v), rtn.dim.nm))
   }
   return(m)
}

# v2d #

#' (Atomic) Vector to Data-Frame
#'
#' \code{v2m} converts an (atomic) vector to a single row or single column data.frame.
#' The benefit of \code{v2m} over \code{as.data.frame.vector} is that the dimension
#' along which the vector is binded can be either rows or columns, whereas in
#' \code{as.data.frame.vector} it can only be binded along a column, and that
#' \code{v2m} will keep the names of \code{v} in the dimnames of the returned
#' data.frame.
#'
#' @param v (atomic) vector.
#'
#' @param along numeric vector of length 1 that is equal to either 1 or 2 specifying
#' which dimension to bind \code{v} along. 1 means that \code{v} is binded along
#' rows (i.e., dimension 1) into a one row data.frame. 2 means that \code{v} is binded
#' along columns (i.e., dimension 2) into a one column data.frame.
#'
#' @param rtn.dim.nm character vector of length 1 specifying what dimname to use
#' for the dimension of length 1 in the returned data.frame. If \code{along} = 1,
#' then \code{rtn.dim.nm} will be the single rowname. If \code{along} = 2, then
#' \code{rtn.dim.nm} will be the single colname. If NULL, then the dimension of
#' length 1 will be created by default with \code{data.frame} internally, which
#' will have the rowname be "1" and the colname "V1".
#'
#' @param stringsAsFactors logical vector of length 1 specifying if \code{v}
#' should be converted to a factor in the case that typeof is character.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{v} is an atomic vector.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame with typeof = \code{typeof(v)}. If \code{along} = 1, then the
#' dimensions = \code{c(1L, length(v))} and dimnames = \code{list(rtn.dim.nm, names(v))}.
#' If \code{along} = 2, then the dimensions = \code{c(length(v), 1L)} and dimnames =
#' \code{list(names(v), rtn.dim.nm)}.
#'
#' @examples
#' x <- setNames(mtcars[, "mpg"], nm = row.names(mtcars))
#' v2d(x)
#' v2d(v = x, along = 1)
#' v2d(v = x, rtn.dim.nm = "mpg")
#' @export
v2d <- function(v, along = 2, rtn.dim.nm = NULL, stringsAsFactors = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertAtomicVector(v)
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertCharacter(rtn.dim.nm, any.missing = FALSE, len = 1L, null.ok = TRUE)
      checkmate::assertLogical(stringsAsFactors, any.missing = FALSE, len = 1L)
   }
   m <- v2m(v = v, along = along, rtn.dim.nm = rtn.dim.nm, check = FALSE)
   d <- m2d(m = m, col = 2, stringsAsFactors = stringsAsFactors, FALSE)
   return(d)
}

# MATRIX2 ####

# m2v #

#' Matrix to (Atomic) Vector
#'
#' \code{m2v} converts a matrix to a (atomic) vector. The benefit of \code{m2v}
#' over \code{as.vector} or \code{c} is that 1) the vector can be formed along rows
#' as well as columns and 2) the dimnames from \code{m} can be used for the names of
#' the returned vector.
#'
#' If \code{use.dimnames} = TRUE, then each element's name will be analogous to
#' \code{paste(rownames(m)[i], colnames(m)[j], sep = sep)}. If \code{m} does not
#' have rownames and/or colnames, then they will be replaced by dimension positions.
#' This is also true when \code{m} has only one row *and* one column. The exception
#' is when \code{m} has either a single row *or* single column. In these cases,
#' only the non-single dimension's names will be used. If \code{m} has one row,
#' then the names of the returned vector will be \code{colnames(m)}. If \code{m}
#' has one column, then the names of the returned vector will be \code{rownames(m)}.
#' Again, if \code{m} does not have rownames and/or colnames, then they will be
#' replaced by dimension positions.
#'
#' @param m matrix
#'
#' @param along numeric vector of length one that is equal to either 1 or 2.
#' 1 means that \code{m} is split along rows (i.e., dimension 1) and then concatenated.
#' 2 means that \code{m} is split along columns (i.e., dimension 2) and then concatenated.
#'
#' @param use.dimnames logical vector of length 1 that specifies whether the dimnames
#' of \code{m} should be used to create the names for the returned vector. If FALSE,
#' the returned vector will have NULL names. If TRUE, see details.
#'
#' @param sep character vector of length 1 specifying the string that will separate
#' the rownames and colnames in the naming scheme of the return object. Note, \code{sep}
#' is not used if \code{use.dimnames} = FALSE.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{m} is a matrix.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return (atomic) vector of length = \code{length(m)} where the order of elements
#' from \code{m} has been determined by \code{along} and the names determined by
#' the \code{use.dimnames}, \code{dimnames(m)}, and \code{sep}. See details for when
#' \code{use.dimnames} = TRUE.
#'
#' @examples
#' # general matrix
#' mtcars2 <- as.matrix(mtcars, rownames.force = TRUE) # to make sure dimnames stay in the example
#' m2v(mtcars2) # default
#' m2v(m = mtcars2, along = 1) # concatenate along rows
#' m2v(m = mtcars2, sep = ".") # change the sep of the rownames(m) and colnames(m)
#' m2v(m = `dimnames<-`(mtcars2, list(NULL, NULL))) # use dimension positions as dimnames
#' m2v(m = mtcars2, use.dimnames = FALSE) # return object has no names
#' # one row/column matrix
#' one_row <- mtcars2[1,, drop = FALSE]
#' m2v(one_row)
#' one_col <- mtcars2[, 1, drop = FALSE]
#' m2v(one_col)
#' one_all <- mtcars2[1,1, drop = FALSE]
#' m2v(one_all)
#' m2v(one_all, use.dimnames = FALSE)
#' @export
m2v <- function(m, along = 2, use.dimnames = TRUE, sep = "_", check = TRUE) {

   if (check) {
      checkmate::assertMatrix(m)
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(x = use.dimnames, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(x = sep, any.missing = FALSE, len = 1L)
   }
   if (!use.dimnames) {
      if (along == 1) {
         t_m <- t(m)
         v <- as.vector(t_m)
      }
      if (along == 2) {
         v <- as.vector(m)
      }
      return(v)
   }
   if (use.dimnames) {
      n_row <- nrow(m)
      n_col <- ncol(m)
      row_names <- rownames(m)
      col_names <- colnames(m)
      if (n_row == 1L && n_col > 1L) {
         if (is.null(col_names)) col_names <- seq_len(n_col)
         v <- setNames(as.vector(m), nm = col_names)
      }
      if (n_col == 1L && n_row > 1L) {
         if (is.null(row_names)) row_names <- seq_len(n_row)
         v <- setNames(as.vector(m), nm = row_names)
      }
      if (n_row == 1L && n_col == 1L) {
         if (is.null(col_names)) col_names <- 1L
         if (is.null(row_names)) row_names <- 1L
         v <- setNames(as.vector(m), nm = paste(row_names, col_names, sep = sep))
      }
      if (n_row > 1L && n_col > 1L) {
         d <- reshape::melt.array(m) # to create position dimnames if they NULL
         expand_dim <- d[c(1,2)]
         dimnames_order <- lapply(X = expand_dim, FUN = unique, fromLast = FALSE)
         if (along == 1)
            ord <- order.custom(X = expand_dim, ORD = dimnames_order)
         if (along == 2)
            ord <- order.custom(X = rev(expand_dim), ORD = rev(dimnames_order))
         d_order <- d[ord, ]
         nm <- apply(X = d_order[c(1,2)], MARGIN = 1L, FUN = function(vec) {
            vec <- as.list(vec) # as.list.default
            vec[["sep"]] <- sep
            do.call(what = `paste`, args = vec)
         })
         v <- setNames(d_order[, 3], nm = nm)
      }
   }
   return(v)
}

# m2d #

#' Matrix to Data-Frame
#'
#' \code{m2d} converts a matrix to a data.frame. The benefit of \code{m2d} over
#' \code{as.data.frame.matrix} is that it provides the \code{col} argument, which
#' allows the columns of the data.frame to be the columns of the matrix (i.e.,
#' \code{col = 2}), the rows of the matrix (i.e., \code{col = 1}), or the expanded
#' matrix (i.e., \code{col = 0}).
#'
#' @param m matrix
#'
#' @param col numeric vector of length 1 that is equal to either 0, 1, or 2. \code{col}
#' specifies what dimension from \code{m} should be the columns of the returned data.frame.
#' If \code{col = 2}, then the columns of \code{m} (i.e., dimension 2) are the columns
#' of the returned data.frame. If \code{col = 1}, then the rows of \code{m}
#' (i.e., dimension 1) are the columns of the returned data.frame. If \code{col = 0},
#' neither of the \code{m} dimensions are the columns and instead the matrix is
#' expanded by \code{reshape::melt.array} such that in the returned data.frame
#' the first column is \code{rownames(m)}, the second column is \code{colnames(m},
#' and the third column is the elements of \code{m}. If any \code{dimnames(m)} are
#' NULL, then they are replaced with the positions of the dimensions.
#'
#' @param stringsAsFactors logical vector of length 1 specifying whether any resulting
#' character columns in the return object should be factors. If \code{m} is a
#' character matrix and \code{stringsAsFactors} = TRUE, then all columns in the
#' returned data.frame will be factors. If \code{col} = 0 and \code{stringsAsFactors}
#' = TRUE, then the first two columns in the returned data.frame specifying
#' \code{dimnames(m)} will be factors.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{m} is a matrix.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame with rownames and colnames specified by \code{dimnames(m)}
#' and \code{col}. If \code{col = 0}, then the rownames are default (i.e., "1","2","3", etc.)
#' and the colnames are the following: the first two columns are \code{names(dimnames(m))}
#' (if NULL they are "rownames" and "colnames", respectively) and the third is
#' "element".
#'
#' @examples
#' mtcars2 <- as.matrix(mtcars, rownames.force = TRUE) # to make sure dimnames stay in the example
#' m2d(mtcars2) # default
#' m2d(m = mtcars2, col = 1) # data.frame columns are matrix rownames
#' m2d(m = mtcars2, col = 0) # data.frame columns are the entire matrix
#' mat <- cbind(lower = letters, upper = LETTERS)
#' m2d(mat)
#' m2d(mat, stringsAsFactors = TRUE)
#' m2d(mat, col = 0)
#' m2d(mat, col = 0, stringsAsFactors = TRUE)
#' @export
m2d <- function(m, col = 2, stringsAsFactors = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertMatrix(m)
      if (!(is.element(el = col, set = c(0,1,2))))
         stop("`along` must be equal to 0, 1, or 2")
      checkmate::assertLogical(x = stringsAsFactors, any.missing = FALSE, len = 1L)
   }

   if (col == 1) d <- as.data.frame.matrix(x = t(m), stringsAsFactors = stringsAsFactors) # as.data.frame.matrix
   if (col == 2) d <- as.data.frame.matrix(x = m, stringsAsFactors = stringsAsFactors) # as.data.frame.matrix
   if (col == 0) {
      d <- reshape::melt.array(m)
      if (!stringsAsFactors) {
         d[c(1,2)] <- lapply(X = d[c(1,2)], FUN = as.character)
         if (is.character(d[[3]])) d[[3]] <- as.factor(d[[3]])
      }
      if (stringsAsFactors) {
         if (is.integer(d[[1]])) d[[1]] <- as.factor(d[[1]])
         if (is.integer(d[[2]])) d[[2]] <- as.factor(d[[2]])
      }
      if (is.null(names(dimnames(m)))) names(d)[1:2] <- c("rownames", "colnames")
      names(d)[3] <- "element"
   }
   return(d)
}

# DATA.FRAME2 ####

# d2d #

#' Data-Frame to Data-Frame (e.g., factors to character vectors)
#'
#' \code{d2d} converts a data.frame to a modified version of the data.frame. It is
#' used to convert factors, character vectors, and logical vectors to different
#' classes/types (e.g., factors to character vectors).
#'
#' \code{d2d} internally uses the \code{fct2v} and \code{v2fct} functions. See them
#' or more details about how column conversions work.
#'
#' @param d data.frame.
#'
#' @param fct character vector of length 1 specifying what factors should be converted
#' to. There are three options: 1) "chr" for converting to character vectors (i.e.,
#' factor labels), 2) "int" for converting to integer vectors (i.e., factor codes),
#' or 3) "fct" for keeping the factor as is without any changes.
#'
#' @param chr character vector of length 1 specifying what character vectors should
#' be converted to. There are three options: 1) "fct" for converting to factors (i.e.,
#' elements will be factor labels), 2) "int" for converting to integer vectors (i.e.,
#' factor codes after first converting to a factor), or 3) "chr" for keeping the
#' character vectors as is without any changes.
#'
#' @param lgl character vector of length 1 specifying what logical vectors should
#' be converted to. There are four options: 1) "fct" for converting to factors (i.e.,
#' "TRUE" and "FALSE" will be factor labels), 2) "chr" for converting to character
#' vectors (i.e., elements will be "TRUE" and "FALSE"), 3) "int" for converting to
#' integer vectors (i.e., TRUE = 1; FALSE = 0), and 4) "lgl" for keeping the logical
#' vectors as is without any changes.
#'
#' @param order.lvl character vector of length 1 specifying how you want to order
#' the levels of the factor. The options are "alphanum", which sorts the levels
#' alphanumerically (with NA last); "position", which sorts the levels by the position
#' the level first appears; "frequency", which sorts the levels by their frequency.
#' If any frequencies are tied, then the ties are sorted alphanumerically (with NA last).
#'
#' @param decreasing logical vector of length 1 specifying whether the ordering of the
#' levels should be decreasing (TRUE) rather than increasing (FALSE).
#'
#' @param na.lvl logical vector of length 1 specifying if NA should be considered a level.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{d} is a data.frame.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame with the same dim and dimnames as \code{d}, but with potentially
#' altered columns which were factors, character vectors, and/or integer vectors.
#'
#' @examples
#' dat <- data.frame(
#' "lgl_1" = c(TRUE, FALSE, NA),
#' "lgl_2" = c(FALSE, TRUE, NA),
#' "int_1" = c(1L, NA, 2L),
#' "int_2" = c(2L, NA, 1L),
#' "dbl_1" = c(1.1, NA, 2.2),
#' "dbl_2" = c(2.2, NA, 1.1),
#' "chr_1" = c(NA, "a","b"),
#' "chr_2" = c(NA, "b","a"),
#' "fct_1" = factor(c(NA, "one","two")),
#' "fct_2" = factor(c(NA, "two","one"))
#' )
#' str(dat)
#' x <- d2d(dat); str(x) # default
#' x <- d2d(dat, fct = "fct", chr = "fct", lgl = "fct"); str(x) # all to factors
#' x <- d2d(dat, fct = "int", chr = "int"); str(x) # all to integers
#' @export
d2d <- function(d, fct = "chr", chr = "chr", lgl = "int", order.lvl = "alphanum",
   decreasing = FALSE, na.lvl = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertDataFrame(d)
      fct <- match.arg(arg = fct, choices = c("chr","int","fct"))
      chr <- match.arg(arg = chr, choices = c("fct","int","chr"))
      lgl <- match.arg(arg = lgl, choices = c("fct","chr","int","lgl"))
      order.lvl <- match.arg(arg = order.lvl,
         choices = c("alphanum","position","frequency"), several.ok = FALSE)
      checkmate::assertLogical(decreasing, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(na.lvl, any.missing = FALSE, len = 1L)
   }

   # do first so that not sequencing along vectors twice (e.g., sequence along for factor, convert to character, then sequence along for character)
   i_fct <- vapply(X = d, FUN = is.factor, FUN.VALUE = logical(1))
   i_chr <- vapply(X = d, FUN = is.character, FUN.VALUE = logical(1))
   i_lgl <- vapply(X = d, FUN = is.logical, FUN.VALUE = logical(1))

   # factor
   d[i_fct] <- lapply(X = d[i_fct], FUN = function(vec) {
      if (fct == "chr" | fct == "int") {
         if (fct == "int") codes <- TRUE
         if (fct == "chr") codes <- FALSE
         output <- fct2v(fct = vec, simplify = FALSE, codes = codes)
      }
      if (fct == "fct") output <- vec
      return(output)
   })

   # character
   d[i_chr] <- lapply(X = d[i_chr], FUN = function(vec) {
      if (chr == "fct" | chr == "int") {
         output <- v2fct(v = vec, order.lvl = order.lvl, decreasing = decreasing, na.lvl = na.lvl)
         if (chr == "int") output <- fct2v(fct = output, simplify = FALSE, codes = TRUE)
      }
      if (chr == "chr") output <- vec
      return(output)
   })

   # logical
   d[i_lgl] <- lapply(X = d[i_lgl], FUN = function(vec) {
      if (lgl == "fct") output <- v2fct(v = vec, order.lvl = order.lvl, decreasing = decreasing, na.lvl = na.lvl)
      if (lgl == "int") output <- as.integer(vec)
      if (lgl == "chr") output <- as.character(vec)
      if (lgl == "lgl") output <- vec
      return(output)
   })

   # return object
   return(d)
}

# d2m #

#' Data-Frame to Matrix
#'
#' \code{d2m} converts a data.frame to a matrix. The user can specify how to convert
#' factors, character vectors, and integer vectors in the data.frame through the
#' internal use of the \code{d2d} function. After the call to \code{d2d}, \code{d2m}
#' simply calls \code{as.matrix.data.frame(rownames.force = TRUE)}, which will
#' return a matrix of the most complex typeof of any column in the data.frame
#' (most complex to least complex: character, double, integer, logical). Therefore,
#' if any factors or character vectors are left in the data.frame, it will return
#' a character matrix. On the other side of things, if all columns in the data.frame
#' are logical, then it will return a logical matrix. However, if every column in the
#' data.frame is logical except for one factor or character vector, then it will
#' return a character matrix.
#'
#' @param d data.frame.
#'
#' @param fct character vector of length 1 specifying what factors should be converted
#' to. There are three options: 1) "chr" for converting to character vectors (i.e.,
#' factor labels), 2) "int" for converting to integer vectors (i.e., factor codes),
#' or 3) "fct" for keeping the factor as is without any changes.
#'
#' @param chr character vector of length 1 specifying what character vectors should
#' be converted to. There are three options: 1) "fct" for converting to factors (i.e.,
#' elements will be factor labels), 2) "int" for converting to integer vectors (i.e.,
#' factor codes after first converting to a factor), or 3) "chr" for keeping the
#' character vectors as is without any changes.
#'
#' @param lgl character vector of length 1 specifying what logical vectors should
#' be converted to. There are four options: 1) "fct" for converting to factors (i.e.,
#' "TRUE" and "FALSE" will be factor labels), 2) "chr" for converting to character
#' vectors (i.e., elements will be "TRUE" and "FALSE"), 3) "int" for converting to
#' integer vectors (i.e., TRUE = 1; FALSE = 0), and 4) "lgl" for keeping the logical
#' vectors as is without any changes.
#'
#' @param order.lvl character vector of length 1 specifying how you want to order
#' the levels of the factor. The options are "alphanum", which sorts the levels
#' alphanumerically (with NA last); "position", which sorts the levels by the position
#' the level first appears; "frequency", which sorts the levels by their frequency.
#' If any frequencies are tied, then the ties are sorted alphanumerically (with NA last).
#'
#' @param decreasing logical vector of length 1 specifying whether the ordering of the
#' levels should be decreasing (TRUE) rather than increasing (FALSE).
#'
#' @param na.lvl logical vector of length 1 specifying if NA should be considered a level.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{d} is a data.frame.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return matrix with the same dim and dimnames as \code{d}. After applying the
#' factor, character vector, and/or integer vector conversions through \code{d2d},
#' the matrix will have typeof = most complex typeof of any column in the modified
#' data.frame.
#'
#' @examples
#' x <- d2m(mtcars); str(x)
#' dat <- as.data.frame(CO2)
#' x <- d2m(dat); str(x)
#' x <- d2m(dat, fct = "int"); str(x)
#' @export
d2m <- function(d, fct = "chr", chr = "chr", lgl = "int", order.lvl = "alphanum",
   decreasing = FALSE, na.lvl = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertDataFrame(d)
      fct <- match.arg(arg = fct, choices = c("chr","int","fct"))
      chr <- match.arg(arg = chr, choices = c("fct","int","chr"))
      lgl <- match.arg(arg = lgl, choices = c("fct","chr","int","lgl"))
      order.lvl <- match.arg(arg = order.lvl,
         choices = c("alphanum","position","frequency"), several.ok = FALSE)
      checkmate::assertLogical(decreasing, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(na.lvl, any.missing = FALSE, len = 1L)
   }

   # computational efficiency for specific case
   if (fct == "int" & chr == "int" & lgl == "int") return(data.matrix(d, rownames.force = TRUE))

   # all other cases
   dfm <- d2d(d = d, fct = fct, chr = chr, lgl = lgl, order.lvl = order.lvl,
      decreasing = decreasing, na.lvl = na.lvl, check = FALSE)
   m <- as.matrix.data.frame(x = dfm, rownames.force = TRUE) # as.matrix.data.frame
   return(m)
}

# d2v #

#' Data-Frame to (Atomic) Vector
#'
#' \code{d2v} converts a data.frame to a matrix. The user can specify how to convert
#' factors, character vectors, and integer vectors in the data.frame through the
#' internal use of the \code{d2d} function. After the call to \code{d2d}, the
#' data.frame is simplied to an atomic vector, which will return a vector of the most
#' complex typeof of any column in the data.frame (most complex to least complex:
#' character, double, integer, logical). Therefore, if any factors or character
#' vectors are left in the data.frame, it will return a character vector. On the
#' other side of things, if all columns in the data.frame are logical, then it will
#' return a logical vector. However, if every column in the data.frame is logical
#' except for one factor or character vector, then it will return a character vector.
#'
#' @param d data.frame.
#'
#' @param along numeric vector of length one that is equal to either 1 or 2.
#' 1 means that \code{d} is split along rows (i.e., dimension 1) and then concatenated.
#' 2 means that \code{d} is split along columns (i.e., dimension 2) and then concatenated.
#'
#' @param use.dimnames logical vector of length 1 that specifies whether the dimnames
#' of \code{d} should be used to create the names for the returned vector. If FALSE,
#' the returned vector will have NULL names. If TRUE, see details of \code{m2v}.
#'
#' @param sep character vector of length 1 specifying the string that will separate
#' the rownames and colnames in the naming scheme of the returned vector. Note, \code{sep}
#' is not used if \code{use.dimnames} = FALSE.
#'
#' @param fct character vector of length 1 specifying what factors should be converted
#' to. There are three options: 1) "chr" for converting to character vectors (i.e.,
#' factor labels), 2) "int" for converting to integer vectors (i.e., factor codes),
#' or 3) "fct" for keeping the factor as is without any changes.
#'
#' @param chr character vector of length 1 specifying what character vectors should
#' be converted to. There are three options: 1) "fct" for converting to factors (i.e.,
#' elements will be factor labels), 2) "int" for converting to integer vectors (i.e.,
#' factor codes after first converting to a factor), or 3) "chr" for keeping the
#' character vectors as is without any changes.
#'
#' @param lgl character vector of length 1 specifying what logical vectors should
#' be converted to. There are four options: 1) "fct" for converting to factors (i.e.,
#' "TRUE" and "FALSE" will be factor labels), 2) "chr" for converting to character
#' vectors (i.e., elements will be "TRUE" and "FALSE"), 3) "int" for converting to
#' integer vectors (i.e., TRUE = 1; FALSE = 0), and 4) "lgl" for keeping the logical
#' vectors as is without any changes.
#'
#' @param order.lvl character vector of length 1 specifying how you want to order
#' the levels of the factor. The options are "alphanum", which sorts the levels
#' alphanumerically (with NA last); "position", which sorts the levels by the position
#' the level first appears; "frequency", which sorts the levels by their frequency.
#' If any frequencies are tied, then the ties are sorted alphanumerically (with NA last).
#'
#' @param decreasing logical vector of length 1 specifying whether the ordering of the
#' levels should be decreasing (TRUE) rather than increasing (FALSE).
#'
#' @param na.lvl logical vector of length 1 specifying if NA should be considered a level.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{d} is a data.frame.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return (atomic) vector with elements from \code{d}. If \code{d} had one row,
#' then the names of the return object are \code{names(d)}. If \code{d} has one
#' column, then the names of the return object are \code{row.names(d)}.
#'
#' @examples
#' # general data.frame
#' d2v(mtcars) # default
#' d2v(d = mtcars, along = 1) # concatenate along rows
#' d2v(d = mtcars, sep = ".") # change the sep of the rownames(d) and colnames(d)
#' d2v(d = mtcars, use.dimnames = FALSE) # return object has no names
#' # one row/column data.frame
#' one_row <- mtcars[1,, drop = FALSE]
#' d2v(one_row)
#' one_col <- mtcars[, 1, drop = FALSE]
#' d2v(one_col)
#' one_all <- mtcars[1,1, drop = FALSE]
#' d2v(one_all)
#' d2v(one_all, use.dimnames = FALSE)
#' @export
d2v <- function(d, along = 2, use.dimnames = TRUE, sep = "_",
   fct = "chr", chr = "chr", lgl = "int", order.lvl = "alphanum",
   decreasing = FALSE, na.lvl = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertDataFrame(d)
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(x = use.dimnames, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(x = sep, any.missing = FALSE, len = 1L)
      fct <- match.arg(arg = fct, choices = c("chr","int","fct"))
      chr <- match.arg(arg = chr, choices = c("fct","int","chr"))
      lgl <- match.arg(arg = lgl, choices = c("fct","chr","int","lgl"))
      order.lvl <- match.arg(arg = order.lvl,
         choices = c("alphanum","position","frequency"), several.ok = FALSE)
      checkmate::assertLogical(decreasing, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(na.lvl, any.missing = FALSE, len = 1L)
   }

   m <- d2m(d = d, fct = fct, chr = chr, lgl = lgl, order.lvl = order.lvl,
      decreasing = decreasing, na.lvl = na.lvl, check = FALSE) # let d2m internally call d2d()
   v <- m2v(m = m, along = along, use.dimnames = use.dimnames, sep = sep, check = FALSE)
   return(v)
}

# d2a #

#' Data-Frame to (3D+) Array
#'
#' \code{d2a} converts a data.frame to a (3D+) array. This function assumes the
#' data.frame contains 3+ variable dimensions, which will correspond to the
#' returned arrays dimensions. One variable (and one variable only) must contain
#' the elements which will correspond to the elements of the returned array.
#'
#' \code{d2a} is a wrapper for \code{reshape::cast} with the addition of reordering
#' the dimnames by position, which sorts the dimnames by the position they first
#' appear in the variable dimensions of the data.frame (\code{reshape::cast}) sorts
#' all the dimnames alphabetically).
#'
#' @param d data.frame with at least 4 columns, where 3+ columns are variable
#' dimensions and 1 column contains the to-be array elements. The columns containing
#' the variable dimensions do not need to be factors or character vectors.
#'
#' @param el.nm character vector of length 1 specifying the colname in \code{d}
#' that contains the to-be array elements.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{d} is a data.frame.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return (3D+) array containing the elements in \code{d[[el.nm]]}. The dimlabels
#' are the colnames of \code{d} other than \code{el.nm} and the dimnames are the unique
#' elements in those columns. The dimnames are ordered by position (rather than
#' alphabetically), which allow for conversions back to the original array after
#' a call to \code{a2d()}.
#'
#'
#' @examples
#' print(HairEyeColor)
#' d <- reshape::melt.array(HairEyeColor)
#' a <- reshape::cast(d, Hair ~ Eye ~ Sex)
#' identical(a, unclass(HairEyeColor)) # not the same as HairEyeColor
#' d <- a2d(HairEyeColor)
#' a <- d2a(d)
#' identical(a, unclass(HairEyeColor)) # yes the same as HairEyeColor
#' @export
d2a <- function(d, el.nm = "element", check = TRUE) {

   if (check) {
      checkmate::assertDataFrame(d, min.cols = 4L)
      checkmate::assertCharacter(el.nm, any.missing = FALSE, len = 1L)
   }

   dim_labels <- names(d)[names(d) != el.nm]
   frm <- as.formula(paste(dim_labels, collapse = " ~ "))
   tmp <- reshape::cast(data = d, formula = frm, value = "element")
   dim_names <- lapply(X = d[names(d) != el.nm], FUN = unique)
   a <- do.call(what = `[`, args = c(list(tmp), dim_names))
   return(a)
}

# ARRAY2 ####

# a2d #

#' (3D+) Array to Data-Frame
#'
#' \code{a2d} converts a (3D+ array) to a data.frame. It allows you to specify a
#' dimension of the array to be the columns. All other dimensions are variables
#' in the data.frame. This is different than \code{as.data.frame.array} which
#' converts the (3D+) array to a matrix first; although it is very similar to
#' \code{as.data.frame.table}.
#'
#' \code{a2d} is mostly a wrapper for \code{reshape::melt.array} (+ \code{reshape::cast})
#' that allows for the variable dimensions to be character vectors rather than factors.
#'
#' @param a 3D+ array.
#'
#' @param col integer vector or character vector of length 1 specifing the dimension
#' of \code{a} to have as columns in the return object. If an integer vector,
#' \code{col} refers to the dimension number. If a character vector, \code{col}
#' refers to the name of the dimension. The columns are in order of the dimnames
#' for that dimension (not alphabetical order like \code{reshape::cast}). If 0
#' (default), then no dimension of the array has a column and the function becomes
#' similar to \code{as.data.frame.table}.
#'
#' @param stringsAsFactors logical vector of length 1 specifying whether the variable
#' dimensions should be factors of chracter vectors.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{a} is a (3D+) array.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame of \code{a}'s elements. The colnames of the variable dimensions
#' are the names of the dimensions in \code{a}. If there were no dimension names in
#' \code{a}, then each dimension is named after its number with an X in front.
#' If \code{col} is not 0, then the rest of the colnames are the dimnames of
#' that dimension from \code{a}. If \code{col} is 0, then the names of the single
#' column with \code{a}'s elements is "element".
#'
#' @examples
#' a2d(HairEyeColor)
#' a2d(HairEyeColor, col = 1)
#' a2d(HairEyeColor, col = "Hair", stringsAsFactors = TRUE)
#' a2d(HairEyeColor, col = 2)
#' a2d(HairEyeColor, col = "Sex", stringsAsFactors = TRUE)
#' try_expr(a2d(as.matrix(attitude))) # error due to inputting a matrix. Instead use \code{m2d}.
#' @export
a2d <- function(a, col = 0, stringsAsFactors = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertArray(a, min.d = 3L)
      test_integerish <- checkmate::testIntegerish(col, any.missing = FALSE, len = 1L,
         lower = 0, upper = ndim(a)) # if integerish, must be between 1 and ndim(a)
      pat <- do.call(what = `paste`, args = c(as.list(dimlabels(a)), "sep" = "|"))
      test_character <- checkmate::testCharacter(col, any.missing = FALSE, len = 1L,
         pattern = pat, null.ok = TRUE)
      if (!(test_integerish || test_character))
         stop("`col` must be 1) a numeric vector of length 1 with an integerish value, 2) a character vector of length 1 with a dimlabels(`a`) value, or 3) NULL")
      if (is.character(col)) # if character, must be one of the dimlabels(a)
         col <- match.arg(arg = col, choices = dimlabels(a), several.ok = FALSE)
      checkmate::assertLogical(stringsAsFactors, any.missing = FALSE, len = 1L)
   }

   d_melt <- reshape::melt.array(a) # defaults to using dimlabels as returned data.frame names
   if (col == 0) {
      d <- d_melt
      names(d)[ncol(d)] <- "element"
   } else {
      if (is.numeric(col)) col_dimlabels <- names(d_melt)[col]
      if (is.character(col)) col_dimlabels <- col
      frm <- as.formula(paste("...", "~", col_dimlabels))
      d_cast <- reshape::cast(data = d_melt, formula = frm, fun.aggregate = NULL,
         add.missing = FALSE, value = "element")
      attr(x = d_cast, which = "idvars") <- NULL # cast returns an object of class "cast_df" with extra attributes
      attr(x = d_cast, which = "rdimnames") <- NULL
      d <- as.data.frame.list(unclass(d_cast)) # as.data.frame.list
      col_dimnames <- unique(as.character(d_melt[[col_dimlabels]]), fromLast = FALSE)
      other_dimlabels <- names(d)[1L:(ncol(d) - length(col_dimnames))]
      d <- d[c(other_dimlabels, col_dimnames)] # put columns in order of dimnames
   }
   if (stringsAsFactors) {
      d <- d2d(d = d, fct = "fct", chr = "fct", lgl = "lgl", order.lvl = "alphanum",
         decreasing = FALSE, na.lvl = FALSE)
   }
   if (!stringsAsFactors) {
      d <- d2d(d = d, fct = "chr", chr = "chr", lgl = "lgl", order.lvl = "alphanum",
         decreasing = FALSE, na.lvl = FALSE)
   }
   return(d)
}

# a2v #

#' (3D+) Array to (Atomic) Vector
#'
#' \code{a2v} converts a matrix to a (atomic) vector. The benefit of \code{m2v}
#' over \code{as.vector} or \code{c} is that 1) the vector can be formed along rows
#' any sequence of dimensions and 2) the dimnames from \code{a} can be used for
#' the names of the returned vector.
#'
#' @param a 3D+ array.
#'
#' @param along numeric vector of length = \code{ndim(a)} that contains the integers
#' \code{1:ndim(a)} specifying the order which the array elements should be concatenated.
#' For example, with a 3D array, \code{3:1} (default) specifies to split the array by
#' layers first, then columns, and then rows. See examples.
#'
#' @param use.dimnames logical vector of length 1 that specifies whether the dimnames
#' of \code{a} should be used to create the names for the returned vector. If FALSE,
#' the returned vector will have NULL names. If TRUE, then each element's name will
#' be analogous to \code{paste(dimnames(a)[[1L]][i], dimnames(a)[[2L]][j], dimnames(a)[[3L]][k],
#' ..., sep = sep)}. If \code{a} does not have dimnames, then they will be replaced
#' by dimension positions.
#'
#' @param sep character vector of length 1 specifying the string that will separate
#' the dimnames from each dimension in the naming scheme of the return object. Note,
#' \code{sep} is not used if \code{use.dimnames} = FALSE.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{a} is a 3D+ array.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return (atomic) vector of length = \code{length(a)} where the order of elements
#' from \code{a} has been determined by \code{along} and the names determined by
#' the \code{use.dimnames}, \code{dimnames(a)}, and \code{sep}.
#'
#' @examples
#' a2v(HairEyeColor) # layers, then columns, then rows (default)
#' a2v(HairEyeColor, along = c(3,1,2)) # layers, then rows, then columns
#' a2v(HairEyeColor, along = 1:3) # rows, then columns, then layers
#' a2v(HairEyeColor, along = 1:3, use.dimnames = FALSE)
#' @export
a2v <- function(a, along = ndim(a):1, use.dimnames = TRUE, sep = "_", check = TRUE) {

   if (check) {
      checkmate::assertArray(a, min.d = 3L)
      if (all(!(is.element(el = along, set = seq.int(from = 1L, to = ndim(a), by = 1L)))))
         stop("`along` must be equal to one of the following integers: 1:ndim(`a`)")
      checkmate::assertLogical(x = use.dimnames, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(x = sep, any.missing = FALSE, len = 1L)
   }
   if (!use.dimnames) {
      a_perm <- aperm(a = a, perm = rev(along))
      a <- as.vector(a_perm)
   }
   if (use.dimnames) {
      d <- reshape::melt.array(a) # to create position dimnames if they NULL
      n_dim <- ndim(a)
      expand_dim <- d[1:n_dim]
      dimnames_order <- lapply(X = expand_dim, FUN = unique, fromLast = FALSE)
      ord <- order.custom(X = expand_dim[along], ORD = dimnames_order[along])
      d_order <- d[ord, ]
      nm <- apply(X = d_order[1:n_dim], MARGIN = 1L, FUN = function(vec) {
         vec <- as.list(vec) # as.list.default
         vec[["sep"]] <- sep
         do.call(what = `paste`, args = vec)
      })
      a <- setNames(d_order[, n_dim + 1L], nm = nm)
   }
   return(a)
}

# LV2 ####

# lv2v #

#' List of (atomic) Vectors to (atomic) Vector
#'
#' \code{lv2v} converts a list of (atomic) vectors to an (atomic) vector. \code{lv2v}
#' is simply a wrapper function for \code{unlist} that allows for more control
#' over the names of the returned (atomic) vector.
#'
#' There are four different scenarios. Each scenario is given as well as the structure
#' of the returned object when both \code{use.listnames} and \code{use.vecnames} are
#' TRUE (default): 1) if both \code{lv} and its vectors have names, then the names of
#' the return object will be a pasted combination of the \code{lv} element's name
#' and the vector element's name separated by \code{sep}; 2) if only \code{lv} has
#' names and its vectors do not, then the names of the returned vector will be a
#' pasted combination of the \code{lv} element's name and the vector element's position
#' separated by \code{sep}; 3) if the vectors have names and \code{lv} does not,
#' then the names of the returned vector will be a pasted combination of the \code{lv}
#' positions and vector names separated by \code{sep}; 4) if both \code{lv} and its
#' vectors do not have names, then the names of the returned vector will be the pasted
#' combination of the \code{lv} positions and vector element's positions separated
#' by \code{sep}.
#'
#' If you want to convert a list of vectors where each vector has length = 1 and the
#' list has names, then you probably want to specify \code{use.vecnames} = FALSE.
#' This will replicate the functionality of \code{unlist(lv)}. See the last example.
#'
#' If you want have a list of vectors where each vector has length > 1 and you
#' want to convert it to a list vector (i.e., all vectors with length = 1),
#' then you can combine \code{lv2v} with \code{v2lv} via \code{v2lv(lv2v(v))}.
#'
#' @param lv list of (atomic) vectors.
#'
#' @param use.listnames logical vector of length 1 specifying whether the names of
#' \code{lv} should be used to construct names for the returned vector. If \code{lv}
#' does not have names and \code{use.listnames} = TRUE, then the list positions will
#' be used as names (i.e., "1","2","3", etc.).
#'
#' @param use.vecnames logical vector of length 1 specifying whether the names of
#' each vector within \code{lv} should be used to construct names for the returned vector.
#' If any vectors within \code{lv} do not have names and \code{use.vecnames} = TRUE,
#' then the vector positions will be used as names (i.e., "1","2","3", etc.).
#'
#' @param sep character vector of length 1 specifying what string to use to separate
#' list names from vector element names. Only used if \code{use.listnames} = TRUE.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{lv} is a list of atomic
#' vectors. This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return atomic vector with length = sum of the lengths of the atomic vectors in
#' \code{lv} and typeof = the highest typeof present in the atomic vectors in \code{lv}
#' (i.e., from high to low: character > double > integer > logical). See the argument
#' \code{use.listnames} for how names are created.
#'
#' @examples
#' # 1) both `lv` and its atomic vectors have names
#' lv <- setNames(object = Map(object = 1:26, nm = letters, f = setNames), nm = LETTERS)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = FALSE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = FALSE)
#' # 2) only `lv` has names
#' lv <- list("lower" = letters, "upper" = LETTERS)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = FALSE) # FYI - results in repeat names
#' lv2v(lv, use.listnames = FALSE, use.vecnames = FALSE)
#' # 3) only the atomic vectors have names
#' lv <- list(setNames(object = 1:26, nm = letters), setNames(object = 1:26, nm = LETTERS))
#' lv2v(lv, use.listnames = TRUE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = FALSE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = FALSE)
#' # 4) neither `lv` nor its atomic vectors have names
#' lv <- as.list(1:26)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = TRUE) # FYI - results in repeat names
#' lv2v(lv, use.listnames = TRUE, use.vecnames = FALSE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = FALSE)
#' # common use case for when vectors are all length 1 and list has names
#' lv <- setNames(as.list(letters), nm = LETTERS)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = FALSE, use.vecnames = TRUE)
#' lv2v(lv, use.listnames = TRUE, use.vecnames = FALSE) # FYI - probably what you want
#' lv2v(lv, use.listnames = FALSE, use.vecnames = FALSE)
#' identical(unlist(lv), lv2v(lv, use.vecnames = FALSE)) # identical to unlist()
#' @export
lv2v <- function(lv, use.listnames = TRUE, use.vecnames = TRUE, sep = "_", check = TRUE) {

   if (check) {
      checkmate::assertList(lv)
      if (!all(unlist(sapply(X = lv, FUN = checkmate::testAtomicVector))))
         stop("all elements of `lv` must be (atomic) vectors")
      checkmate::assertLogical(use.listnames, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(use.vecnames, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(sep, any.missing = FALSE, len = 1L)
   }

   if (use.listnames) {
      if (is.null(names(lv))) names(lv) <- seq_along(lv)
   }
   if (use.vecnames) {
      need_names <- unlist(lapply(X = lv, FUN = function(v) is.null(names(v))))
      lv[need_names] <- lapply(X = lv[need_names], FUN = function(v)
         `names<-`(v, value = seq_along(v)))
   }
   v <- unlist(x = unname(lv), recursive = FALSE, use.names = use.vecnames)
   if (use.listnames) {
      list_len <- lengths(lv, use.names = FALSE)
      tmp <- Map(len = list_len, nm = names(lv), f = function(len, nm) {
         rep.int(nm, times = len)
      })
      list_nm_rep <- unlist(tmp, use.names = FALSE)
      if (use.vecnames)
         names(v) <- paste(list_nm_rep, names(v), sep = sep)
      else
         names(v) <- list_nm_rep
   }
   return(v)
}

# lv2m #

#' List of (atomic) Vectors to Matrix
#'
#' \code{lv2m} converts a list of (atomic) vectors to a matrix. This function
#' is similar to a hypothetical \code{as.matrix.list} method if it existed.
#' Note, if the vectors are not all the same typeof, then the matrix will have
#' the most complex typeof any vector in \code{lv}.
#'
#' If fill = FALSE, \code{lv2m} uses a combination of \code{do.call} and \code{rbind}
#' if \code{along} = 1 or \code{do.call} and \code{cbind} if \code{along} = 2.
#' rownames and colnames of the returned data.frame are determined by the names of
#' \code{lv} and the names of the first vector within \code{lv}. If either are NULL,
#' then the positions are used as the dimension names. If fill = FALSE, then an
#' error is returned ff the vectors in \code{lv} do not all have the same length.
#' If fill = FALSE, there is no check to ensure the elements within each \code{lv}
#' vector have the same names in the same order. The names are taken from the first
#' vector in \code{lv}, and it is assumed those names and their order apply to each
#' vector in \code{lv}. Essentially, if fill = FALSE, \code{lv} binds the vectors
#' by positions and not names.
#'
#' If fill = TRUE, \code{lv2m} uses \code{plyr::rbind.fill.matrix} if \code{along} = 1 or
#' \code{plyr::rbind.fill.matrix} and \code{t.default} if \code{along} = 2. If fill = TRUE,
#' \code{lv2d} binds the vectors by by names (and by positions if no names are present).
#' Depending on what the user wants, fill = FALSE or TRUE could be safer. If the user
#' wants an error returned when any vectors within \code{lv} have different lengths,
#' then fill = FALSE should be used. If the user wants to bind by names rather than
#' position, then fill = TRUE should be used.
#'
#' @param lv list of (atomic) vectors.
#'
#' @param along numeric vector of length 1 specifying either 1 for binding along rows
#' (i.e., each list element is a row) and 2 for binding along columns (i.e., each
#' list element in a column).
#'
#' @param fill logical vector of length 1 specifying whether 1) to allow the vectors
#' in \code{lv} to have different lengths, names, or both, 2) to bind by the names
#' of the vectors within \code{lv} rather than by their positions (unless no names
#' are present in which case positions are used), and 3) fill in any missing values
#' in the return object with NA.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{lv} is a list of atomic
#' vectors. This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return matrix with the elements of \code{lv} either as rows or columns and dimnames
#' determined by the names of \code{lv} and \code{lv[[1]]}. The typeof is determined
#' by the highest typeof in the elements of \code{lv} (i.e., highest to lowest: character >
#' double > integer > logical).
#'
#' @examples
#' # 1) `lv` has names; vectors have names
#' lv <- setNames(object = lapply(X = letters, FUN = setNames, nm = "alphabet"), nm = LETTERS)
#' lv2m(lv, along = 1)
#' lv2m(lv, along = 2)
#' # 2) `lv` has names; no vector names
#' lv <- setNames(object = as.list(letters), nm = LETTERS)
#' lv2m(lv, along = 1)
#' lv2m(lv, along = 2)
#' # 3) no `lv` names; vector have names
#' lv <- lapply(X = letters, FUN = setNames, nm = "alphabet")
#' lv2m(lv, along = 1)
#' lv2m(lv, along = 2)
#' # 4) no `lv` names; no vector names
#' lv <- as.list.default(letters)
#' lv2m(lv, along = 1)
#' lv2m(lv, along = 2)
#' # actual use case (sort of)
#' lv <- lapply(X = asplit(x = as.matrix(attitude), MARGIN = 1),
#'    FUN = undim) # need undim since asplit returns 1D arrays
#' cbind(lv) # not what we want
#' do.call(what = cbind, args = lv) # doesn't have useful dimnames
#' lv2m(lv, along = 2) # finally what we want
#' # when vectors have named elements in different positions
#' lv <- list("row_1" = c("col_A" = "col_A1", "col_B" = "col_B1", "col_C" = "col_C1"),
#'    "row_2" = c("col_B" = "col_B2", "col_C" = "col_C2", "col_A" = "col_A2"),
#'    "row_3" = c("col_C" = "col_C3", "col_A" = "col_A3", "col_B" = "col_B3"))
#' lv2m(lv, along = 1, fill = FALSE) # probably not what you want
#' lv2m(lv, along = 1, fill = TRUE) # what you want (See details)
#' @export
lv2m <- function(lv, along, fill = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertList(lv)
      if (!all(unlist(lapply(X = lv, FUN = checkmate::testAtomicVector))))
         stop("all elements of `lv` must be (atomic) vectors")
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(fill, any.missing = FALSE, len = 1L)
   }
   if (!fill) {
      if (check) {
         if (var(lengths(lv) != 0))
            stop("if `fill` = FALSE, all elements of `lv` must have the same length")
      }
      if (along == 1L) {
         m <- do.call(what = `rbind`, args = lv)
         # deal with no names in `lv` object
         if (!(is.null(names(lv)))) row.names(m) <- names(lv)
         else row.names(m) <- seq.int(from = 1L, to = length(lv), by = 1L)
         if (!(is.null(names(lv[[1]])))) colnames(m) <- names(lv[[1]])
         else colnames(m) <- seq.int(from = 1L, to = length(lv[[1]]), by = 1L)
      }
      if (along == 2L) {
         m <- do.call(what = `cbind`, args = lv)
         # deal with no names in `lv` object
         if (!(is.null(names(lv[[1]])))) row.names(m) <- names(lv[[1]])
         else row.names(m) <- seq.int(from = 1L, to = length(lv[[1]]), by = 1L)
         if (!(is.null(names(lv)))) colnames(m) <- names(lv)
         else colnames(m) <- seq.int(from = 1L, to = length(lv), by = 1L)
      }
   }
   if (fill) {
      if (along == 1) {
         lm_1row <- lapply(X = lv, FUN = v2m, along = 1, check = FALSE)
         m <- plyr::rbind.fill.matrix(lm_1row)
         rownames(m) <- names(lv) # even if each element in lm_1row has character row.names, plyr::rbind.fill.matrix replaces them with no rownames
      }
      if (along == 2) { # transpose the lm_1row solution because matrix is atomic
         lm_1row <- lapply(X = lv, FUN = v2m, along = 1, check = FALSE)
         tmp <- plyr::rbind.fill.matrix(lm_1row)
         rownames(tmp) <- names(lv) # even if each element in lm_1row has character row.names, plyr::rbind.fill.matrix replaces them with no rownames
         m <- t(tmp) # t.default
      }
   }
   return(m)
}

# lv2d #

#' List of (atomic) vectors to Data-Frame
#'
#' \code{lv2d} converts a list of (atomic) vectors to a data.frame. This function
#' is similar to \code{as.data.frame.list}, but allows for more flexibility in how
#' the data.frame will be structured (e.g., rowwise), while simplifying the dimension
#' naming process.
#'
#' If fill = FALSE, \code{lv2d} uses a combination of \code{do.call} and \code{rbind}
#' if \code{along} = 1 or \code{do.call} and \code{cbind} if \code{along} = 2.
#' rownames and colnames of the returned data.frame are determined by the names of
#' \code{lv} and the names of the first vector within \code{lv}. If either are NULL,
#' then the positions are used as the dimension names. If fill = FALSE, then an
#' error is returned ff the vectors in \code{lv} do not all have the same length.
#' If fill = FALSE, there is no check to ensure the elements within each \code{lv}
#' vector have the same names in the same order. The names are taken from the first
#' vector in \code{lv}, and it is assumed those names and their order apply to each
#' vector in \code{lv}. Essentially, if fill = FALSE, \code{lv} binds the vectors
#' by positions and not names.
#'
#' If fill = TRUE, \code{lv2d} uses \code{plyr::rbind.fill} if \code{along} = 1 or
#' \code{plyr::join_all} by the vector names if \code{along} = 2. If fill = TRUE,
#' \code{lv2d} binds the vectors by by names (and by positions if no names are present).
#' Depending on what the user wants, fill = FALSE or TRUE could be safer. If the user
#' wants an error returned when any vectors within \code{lv} have different lengths,
#' then fill = FALSE should be used. If the user wants to bind by names rather than
#' position, then fill = TRUE should be used.
#'
#' @param lv list of (atomic) vectors.
#'
#' @param along numeric vector of length 1 specifying either 1 for binding along rows
#' (i.e., each list element is a row) or 2 for binding along columns (i.e., each
#' list element in a column).
#'
#' @param fill logical vector of length 1 specifying whether 1) to allow the vectors
#' in \code{lv} to have different lengths, names, or both, 2) to bind by the names
#' of the vectors within \code{lv} rather than by their positions (unless no names
#' are present in which case positions are used), and 3) fill in any missing values
#' in the return object with NA.
#'
#' @param risky logical vector of length 1 specifying whether to use \code{list2DF}
#' rather than \code{data.frame} when \code{along} = 2 and \code{fill} = TRUE.
#' If either \code{along} = 1 or \code{fill} = FALSE, this argument is not used.
#'
#' @param stringsAsFactors logical vector of length 1 specifying whether character
#' vectors should be coerced to factors. See \code{default.stringsAsFactors}.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{lv} is a list of atomic
#' vectors. This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame with the elements of `lv` either as rows or columns and dimnames
#' determined along the names of `lv` and `lv`[[1]].
#'
#' @examples
#' # 1) `lv` has names; vectors have names
#' lv <- setNames(object = lapply(X = letters, FUN = setNames, nm = "alphabet"), nm = LETTERS)
#' lv2d(lv, along = 1)
#' lv2d(lv, along = 2)
#' lv2d(lv, along = 2, stringsAsFactors = TRUE)
#' # 2) `lv` has names; no vector names
#' lv <- setNames(object = as.list(letters), nm = LETTERS)
#' lv2d(lv, along = 1)
#' lv2d(lv, along = 2)
#' # 3) no `lv` names; vector have names
#' lv <- lapply(X = letters, FUN = setNames, nm = "alphabet")
#' lv2d(lv, along = 1)
#' lv2d(lv, along = 2)
#' # 4) no `lv` names; no vector names
#' lv <- as.list.default(letters)
#' lv2d(lv, along = 1)
#' lv2d(lv, along = 2)
#' # we want vectors combined along rows
#' lv <- lapply(X = unclass(mtcars), FUN = `names<-`, value = row.names(mtcars))
#' rbind(lv) # not what we want (array list)
#' rbind.data.frame(lv) # also not what we want (combined along cols)
#' do.call(what = rbind.data.frame, args = lv) # doesn't have useful dimnames
#' lv2d(lv, along = 1) # finally what we want
#' # fill = TRUE
#' tmp <- lapply(X = unclass(mtcars), FUN = `names<-`, value = row.names(mtcars))
#' lv <- lapply(X = tmp, FUN = function(v) v[-(sample(x = seq_along(v), size = 9))])
#' lv2d(lv, along = 1L, fill = TRUE) # NA for missing values in any given row
#' tmp <- lapply(X = unclass(as.data.frame(t(mtcars))), FUN = `names<-`, value = names(mtcars))
#' lv <- lapply(X = tmp, FUN = function(v) v[-(sample(x = seq_along(v), size = 3))])
#' lv2d(lv, along = 2L, fill = TRUE) # NA for missing values in any given column
#' # actual use case
#' lv <- lapply(X = sn(1:30), FUN = function(i)
#'    coef(lm(v2frm(names(attitude)), data = attitude[-i, ])))
#' lv2d(lv, along = 2) # coefs in a data.frame
#' # when vectors have named elements in different positions use fill = TRUE
#' lv <- list("row_1" = c("col_A" = "col_A1", "col_B" = "col_B1", "col_C" = "col_C1"),
#' "row_2" = c("col_B" = "col_B2", "col_C" = "col_C2", "col_A" = "col_A2"),
#' "row_3" = c("col_C" = "col_C3", "col_A" = "col_A3", "col_B" = "col_B3"))
#' lv2d(lv, along = 1, fill = FALSE) # probably not what you want (See details)
#' lv2d(lv, along = 1, fill = TRUE) # what we want
#' @export
lv2d <- function(lv, along, fill = FALSE, risky = FALSE, stringsAsFactors = FALSE,
   check = TRUE) {

   if (check) {
      checkmate::assertList(lv)
      if (!all(unlist(lapply(X = lv, FUN = checkmate::testAtomicVector))))
         stop("all elements of `lv` must be (atomic) vectors")
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(fill, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(stringsAsFactors, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(risky, any.missing = FALSE, len = 1L)
   }
   if (!fill) {
      if (check) {
         if (var(lengths(lv) != 0))
            stop("if `fill` = FALSE, all elements of `lv` must have the same length")
      }
      if (risky && along == 2) {
         tmp <- list2DF(lv)
         if (stringsAsFactors)
            d <- d2d(d = tmp, fct = "fct", chr = "fct", check = FALSE)
         else
            d <- tmp
         return(d)
      }
      m <- lv2m(lv = lv, along = along, check = FALSE)
      d <- m2d(m = m, col = 2, stringsAsFactors = stringsAsFactors, check = FALSE)
   }
   if (fill) {
      if (along == 1) {
         ld_1row <- lapply(X = lv, FUN = v2d, along = 1, check = FALSE)
         d <- plyr::rbind.fill(ld_1row)
         row.names(d) <- names(lv) # even if each element in ld_1row has character row.names, plyr::rbind.fill replaces them with default row.names
      }
      if (along == 2) { # merge by row.names
         ld_1col <- lapply(X = lv, FUN = v2d, along = 2, check = FALSE)
         lv_names <- names(lv)
         if (is.null(lv_names)) lv_names <- as.character(1:length(lv))
         ld_1col_rownm <- Map(d = ld_1col, nm = lv_names, f = function(d, nm) {
            names(d) <- nm
            d[[".Row.names."]] <- row.names(d)
            return(d)
         })
         d <- plyr::join_all(dfs = ld_1col_rownm, by = ".Row.names.", type = "full", match = "first")
         row.names(d) <- d[[".Row.names."]]
         # there is no way to order the returned data.frame since `lv` doesn't know what order the rownames should be in
         d[[".Row.names."]] <- NULL
      }
   }
   return(d)
}

# 2LV ####

# v2lv #

#' (Atomic) Vector to List of (Atomic) Vectors
#'
#' \code{v2lv} converts a (atomic) vector to a list of atomic vectors. The default is
#' conversion to a list vector where each element of the list has only one element.
#' The \code{n.break} argument allows for the input vector to be broken up into
#' larger sections with each section being a list element in the return object.
#'
#' Future versions of this function plan to allow for use similar to the \code{utils::relist}
#' function to allow reconstruction after flattening a matrix-like object to a single vector.
#'
#' @param v (atomic) vector.
#'
#' @param use.names logical vector of length 1 specifying whether the names from
#' \code{v} should be retained in the return object.
#'
#' @param n.break integer vector of length 1 specifying how \code{v} should be broken
#' up. Every {n.break} elements while seq_along \code{v}, a new element of the list
#' is created and subsequent elements of \code{v} are stored there. If \code{n.break}
#' is not a multiple of \code{length(v)}, then NAs are appended to the end of \code{v}
#' to ensure that each list element has (atomic) vectors of the same length. Note, the
#' default is 1L resulting in a list vector.
#'
#' @param warn.break logical vector of length one specifying whether a warning
#' should be printed if \code{length(v) / n.break} is not a whole number, which
#' would then result in NAs being appended to the end of the vector before converting
#' to a list.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{v} is an atomic vector.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list of (atomic) vectors that are the elements of \code{v} broken up
#' according to \code{n.break}. The list only has names if \code{v} has names and
#' \code{n.break} = 1L.
#'
#' @examples
#' vec <- setNames(object = mtcars[[1]], nm = row.names(mtcars))
#' v2lv(vec)
#' v2lv(vec, use.names = FALSE)
#' vec <- unlist(mtcars)
#' v2lv(vec, n.break = 32) # n.break > 1L and multiple of length(v)
#' v2lv(vec, n.break = 30) # n.break > 1L and NOT multiple of length(v)
#' @export
v2lv <- function(v, use.names = TRUE, n.break = 1L, warn.break = TRUE, check = TRUE) {

   if (check) {
      checkmate::assertAtomicVector(v)
      checkmate::assertLogical(use.names, any.missing = FALSE, len = 1L)
      checkmate::assertIntegerish(n.break, lower = 1L)
      checkmate::assertLogical(warn.break, any.missing = FALSE, len = 1L)
   }
   if (n.break > 1L) {
      length_v <- length(v)
      n_el <- length_v / n.break
      if (!checkmate::testIntegerish(n_el)) {
         if (warn.break) warning("NAs added to `v` due to length(`v`) not equal to a multiple of `n.break`")
         decimal_remainder <- n_el %% 1 # %% and %/% are both prone to floating point errors according to their R help page
         length_extra <- n.break - (n.break * decimal_remainder)
         # print(sprintf("%.20f", extra)) extra is far enough away from 2 for the `times` argument in rep.int() to interpret it as a 1
         na_extra <- rep.int(x = NA, times = round(length_extra))
         v <- append(x = v, values = na_extra, after = length(v))
         length_v <- length(v)
         n_el <- length_v / n.break
      }
      tmp <- lapply(X = 1L:n_el, FUN = rep.int, times = n.break)
      fct <- factor(unlist(tmp))
      lv <- split(x = v, f = fct)
   }
   if (n.break == 1L) lv <- as.list.default(v)
   if (!use.names) lv <- unname(lv)
   return(lv)
}

# m2lv #

#' Matrix to List of (Atomic) Vectors
#'
#' \code{m2lv} converts a matrix to a list of (atomic) vectors. This is useful
#' since there is no \code{as.list.matrix} method. When rownames and/or colnames
#' are NULL, they are replaced by their position numerals so that the dimension
#' information is retained.
#'
#' @param m matrix (i.e., array with 2 dimensions).
#'
#' @param along numeric vector of length 1 specifying which dimension to slice
#' the matrix along. If 1, then the matrix is sliced by rows. If 2, then the
#' matrix is sliced by columns.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{m} is a matrix.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list of (atomic) vectors. If \code{along} = 1, then the names are the
#' rownames of \code{m} and the vectors are rows from \code{m}. If \code{along} = 2,
#' then the names are the colnames of \code{m} and the vector are columns from \code{m}.
#' Note, the vectors always have the same length as \code{nrow(m)}.
#'
#' @examples
#' m2lv(VADeaths, along = 1)
#' m2lv(VADeaths, along = 2)
#' m2lv(m = as.matrix(x = attitude, rownames.force = TRUE), along = 1)
#' m2lv(m = as.matrix(x = attitude, rownames.force = TRUE), along = 2)
#' m2lv(m = as.matrix(x = unname(attitude), rownames.force = FALSE),
#'    along = 1) # dimnames created as position numerals
#' m2lv(m = as.matrix(x = unname(attitude), rownames.force = FALSE),
#'    along = 2) # dimnames created as position numerals
#' # check = FALSE
#' try_expr(m2lv(VADeaths, along = 3, check = FALSE)) # less informative error message
#' try_expr(m2lv(VADeaths, along = 3, check = TRUE)) # more informative error message
#' @export
m2lv <- function(m, along, check = TRUE) {

   if (check) {
      checkmate::assertMatrix(m)
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
   }
   if (is.null(rownames(m))) rownames(m) <- as.character(1:nrow(m))
   if (is.null(colnames(m))) colnames(m) <- as.character(1:ncol(m))
   tmp <- asplit(x = m, MARGIN = along)
   lv <- lapply(X = tmp, FUN = undim)
   return(lv)
}

# d2lv #

#' Data-Frame to List of (Atomic) Vectors
#'
#' \code{d2lv} converts a data.frame to a list of (atomic) vectors. This function
#' is really only worthwhile when \code{along} = 1 since when \code{along} = 2,
#' the function is essentially \code{as.list.data.frame(d)}.
#'
#' @param d data.frame.
#'
#' @param along numeric vector of length 1 specifying which dimension to slice
#' the data.frame along. If 1, then the data.frame is sliced by rows. If 2, then the
#' data.frame is sliced by columns.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{d} is a data.frame.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list of (atomic) vectors. If \code{along} = 1, then the names are the
#' rownames of \code{d} and the vectors are rows from \code{d}. If \code{along} = 2,
#' then the names are the colnames of \code{d} and the vector are columns from \code{d}.
#' Note, the vectors always have the same length as \code{nrow(d)}.
#'
#' @examples
#' d2lv(mtcars, along = 1)
#' d2lv(mtcars, along = 2)
#' d2lv(CO2, along = 1) # all vectors converted to typeof character
#' d2lv(CO2, along = 2) # each column stays its own typeof (or class for factors)
#' # check = FALSE
#' try_expr(d2lv(mtcars, along = 3, check = FALSE)) # less informative error message
#' try_expr(d2lv(mtcars, along = 3, check = TRUE)) # more informative error message
#' @export
d2lv <- function(d, along, check = TRUE) {

   if (check) {
      checkmate::assertDataFrame(d)
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
   }
   if (is.null(rownames(d))) rownames(d) <- as.character(1:nrow(d))
   if (is.null(colnames(d))) colnames(d) <- as.character(1:ncol(d))
   if (along == 2) {
      tmp <- as.list.data.frame(d)
      lv <- lapply(X = tmp, FUN = `names<-`, row.names(d))
   }
   if (along == 1) {
      lv <- m2lv(m = as.matrix.data.frame(d, row.names.force = TRUE),
         along = 1, check = FALSE)
   }
   return(lv)
}

# LM2 ####

# lm2a #

#' List of Matrices to 3D Array
#'
#' \code{lm2a} converts a list of matrices to a 3D array where the list dimension
#' becomes the third dimension of the array (layers). \code{lm2a} is a simple
#' wrapper function for \code{abind::abind}.
#'
#' @param lm list of matrices which each have the same dimensions.
#'
#' @param dim.order integer vector of length 3 specifying the order of dimensions for
#' the returned array. The default is \code{c(1,2,3)} which means the rows of the
#' matrices in \code{lm} is the first dimension (i.e., rows), the columns of the
#' matrices in \code{lm} is the second dimension (i.e., columns), and the list
#' elements of \code{lm} is the third dimension (i.e., layers).
#'
#' @param dimlab.list character vector of length 1 specifying the dimlabel for
#' the list dimension.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{lm} is a list of matrices.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return 3D array where the list elements of \code{lm} is now a dimension. The
#' order of the dimensions is determined by the argument \code{dim.order} with dimnames
#' specified by names(\code{lm}). The dimnames of the returned array is determined
#' by the dimnames in \code{lm[[1]]} and \code{names(lm)}.
#'
#' @examples
#' lm <- asplit(HairEyeColor, MARGIN = 3L)
#' lm2a(lm) # default
#' lm2a(lm, dimlab.list = "Sex")
#' lm2a(lm, dim.order = c(3,1,2))
#' lm2a(lm, dim.order = c(3,1,2), dimlab.list = "Sex")
#' @export
lm2a <- function(lm, dim.order = c(1,2,3), dimlab.list = NULL, check = TRUE) {

   if (check) {
      checkmate::assertList(lm, any.missing = FALSE)
      if (!(all(unlist(lapply(X = lm, FUN = is.matrix)))))
         stop("all elements of `lm` must be matrices")
      if (0 != var(unlist(lapply(X = lm, FUN = function(mat) dim(mat)[1]))))
         stop("all elements of `lm` must have the same number of rows")
      if (0 != var(unlist(lapply(X = lm, FUN = function(mat) dim(mat)[2]))))
         stop("all elements of `lm` must have the same number of columns")
      checkmate::assertIntegerish(dim.order, any.missing = FALSE, len = 3L)
      checkmate::assertCharacter(dimlab.list, any.missing = FALSE, len = 1L, null.ok = TRUE)
   }
   tmp <- abind::abind(lm, along = 3L, force.array = TRUE, make.names = FALSE,
      use.first.dimnames = TRUE, use.dnns = TRUE)
   if (!(is.null(dimlab.list))) names(dimnames(tmp))[[3L]] <- dimlab.list
   a <- aperm(a = tmp, perm = dim.order)
   return(a)
}

# lm2d #

#' List of Matrices to Data-Frame
#'
#' \code{lm2d} converts a list of matrices to a data.frame. The function is
#' primarily for rbinding a list of matrices (\code{along} = 1). An option to
#' cbind the list of matrices is included (\code{along} = 2), but is just a call to
#' \code{data.frame(lapply(lm, m2d), stringsAsFactors = stringsAsFactors, check.names = check.names)}.
#'
#' Another way to convert a list of matrices to a data.frame is to convert the list
#' dimension, row dimension, and column dimension in the list of matrices all to
#' variable dimensions in the data.frame. If this is desired, call \code{a2d(lm2a(lm))}
#' instead of \code{lm2d}.
#'
#' @param lm list of matrices.
#'
#' @param along numeric vector of length 1 specifying which dimension the matrices
#' from \code{lm} should be binded along: 1 is for rows and 2 is for columns.
#'
#' @param fill logical vector of length 1 specifying whether to fill in missing values
#' for any matrices from \code{lm} that do not have all the columns. At this time,
#' \code{fill} is only available for rbinding and only used if \code{along} = 1.
#'
#' @param rtn.listnames.nm character of length 1 specifying what the name of the
#' column containing the names/positions of \code{lm} should be in the returned
#' data.frame. If NULL, then no column is created for the names/positions of \code{lm}
#' in the returned data.frame.
#'
#' @param rtn.rownames.nm character of length 1 specifying what the name of the
#' column containing the names/positions of the rows within \code{lm}'s matrices
#' should be in the returned data.frame. If NULL, then no column is created for
#' the rownames of \code{lm}'s matrices in the returned data.frame.
#'
#' @param stringsAsFactors logical vector of length 1 specifying whether character columns
#' from \code{lm} should be converted to factors. Note, that is a matrix is character,
#' then \code{stringsAsFactors} would apply to all columns.
#'
#' @param check.names logical vector of length 1 specifying whether the colnames
#' of the returned data.frame should be checked for duplicates and made unique.
#' Only used if for cbinding with \code{along} = 2.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{lm} is a list of matrices.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame with the rows (if \code{along} = 1) or columns (if \code{along}
#' = 2) of \code{lm} binded together.
#'
#' @examples
#' # list names and rownames
#' lm <- asplit(HairEyeColor, MARGIN = 3L)
#' lm2d(lm) # default
#' lm2d(lm, rtn.listnames.nm = "Sex", rtn.rownames.nm = "Hair")
#' # no list names
#' lm2 <- `names<-`(lm, value = NULL)
#' lm2d(lm2)
#' lm2d(lm2, rtn.listnames.nm = NULL)
#' # no rownames too
#' lm3 <- lapply(lm2, `rownames<-`, value = NULL)
#' lm2d(lm3)
#' lm2d(lm3, rtn.rownames.nm = NULL)
#' lm2d(lm3, rtn.listnames.nm = NULL, rtn.rownames.nm = NULL)
#' # cbinding as columns
#' lm2d(lm3, along = 2)
#' lm2d(lm3, along = 2, check.names = TRUE)
#' @export
lm2d <- function(lm, along = 1, fill = FALSE, rtn.listnames.nm = "list_names",
   rtn.rownames.nm = "row_names", stringsAsFactors = FALSE, check.names = FALSE,
   check = TRUE) {

   if (check) {
      checkmate::assertList(lm, any.missing = FALSE)
      if (!(all(unlist(lapply(X = lm, FUN = is.matrix)))))
         stop("all elements of `lm` must be matrices")
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(fill, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(rtn.listnames.nm, any.missing = FALSE, len = 1L, null.ok = TRUE)
      checkmate::assertCharacter(rtn.rownames.nm, any.missing = FALSE, len = 1L, null.ok = TRUE)
      checkmate::assertLogical(stringsAsFactors, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(check.names, any.missing = FALSE, len = 1L)
      if (!fill) {
         if (along == 2 && 0 != var(unlist(lapply(X = lm, FUN = function(mat) dim(mat)[1]))))
            stop("if `along` = 2, all elements of `lm` must have the same number of rows")
         # have not figured out to apply `fill` = TRUE and `along` = 2...
         if (along == 1 && 0 != var(unlist(lapply(X = lm, FUN = function(mat) dim(mat)[2]))))
            stop("if `fill` = FALSE and `along` = 1, all elements of `lm` must have the same number of columns")
      }
   }

   ld <- lapply(X = lm, FUN = m2d, stringsAsFactors = stringsAsFactors, check = FALSE)
   d <- ld2d(ld = ld, along = along, fill = fill, rtn.listnames.nm = rtn.listnames.nm,
      rtn.rownames.nm = rtn.rownames.nm, stringsAsFactors = stringsAsFactors,
      check.names = check.names, check = FALSE)
   return(d)
}

# lm2v #

#' List of Matrices to (Atomic) Vector
#'
#' \code{lm2v} converts a list of matrices to a (atomic) vector. This function is
#' a combination of \code{m2v} and \code{lv2v}. This function can be useful in
#' conjunction with the \code{boot::boot} function when wanting to generate a
#' \code{statistic} function that returns an atomic vector.
#'
#' When \code{list.names} and \code{use.dimnames} are both TRUE (default), the returned
#' vector elements the following naming scheme: "[listname][sep][rowname][sep][colname]".
#'
#' If the matrices in \code{lm} are not all the same typeof, then the return object
#' is coerced to the most complex type of any matrix (e.g., character > double >
#' integer > logical). See \code{unlist} for details about the hierarchy of object types.
#'
#' @param lm list of matrices. They do NOT have to be the same typeof or have the
#' same dimensions.
#'
#' @param along numeric vector of length one that is equal to either 1 or 2.
#' 1 means that each matrix in \code{lm} is split along rows (i.e., dimension 1)
#' and then concatenated. 2 means that each matrix in \code{lm} is split along columns
#' (i.e., dimension 2) and then concatenated.
#'
#' @param use.listnames logical vector of length 1 specifying whether the returned
#' vector should have names based on the list the element came from. If \code{lm}
#' does not have names, \code{use.listnames} = TRUE will have the list positions
#' serve as the list names (e.g., "1", "2", "3", etc.)
#'
#' @param use.dimnames logical vector of length 1 specifying whether the returned
#' vector should have named based on the dimnames of the matrix the element came from.
#' If a matrix within \code{lm} does not have dimnames, \code{use.dimnames} = TRUE
#' will have the dimension positions serve as the dimnames (e.g., "1", "2", "3", etc.)
#'
#' @param sep character vector of length 1 specifying the string used to separate
#' the listnames and dimnames from each other when creating the names of the returned
#' vector.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{lm} is a list of matrices.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return (atomic) vector with an element for each element from `lm`.
#'
#' @examples
#' lm <- list("numeric" = data.matrix(npk), "character" = as.matrix(npk))
#' # use.listnames = TRUE & use.dimnames = TRUE
#' lm2v(lm) # the first part of the name is the list names followed by the dimnames
#' # use.listnames = FALSE & use.dimnames = TRUE
#' lm2v(lm, use.listnames = FALSE) # only dimnames used,
#'    # which can result in repeat names
#' # use.listnames = TRUE & use.dimnames = FALSE
#' lm2v(lm, use.dimnames = FALSE) # listnames and vector position without any
#'    # reference to matrix dimensions
#' # use.listnames = FALSE & use.dimnames = FALSE
#' lm2v(lm, use.listnames = FALSE, use.dimnames = FALSE) # no names at all
#' # when list does not have names
#' lm <- replicate(n = 3, expr = as.matrix(attitude, rownames.force = TRUE), simplify = FALSE)
#' lm2v(lm) # the first digit of the names is the list position and
#'    # the subsequent digits are the matrix dimnames
#' lm2v(lm, use.listnames = FALSE) # no listnames; only dimnames used,
#'    # which can result in repeat names
#' @export
lm2v <- function(lm, along = 2, use.listnames = TRUE, use.dimnames = TRUE,
   sep = "_", check = TRUE) {

   if (check) {
      checkmate::assertList(lm, any.missing = FALSE)
      if (!(all(unlist(lapply(X = lm, FUN = is.matrix)))))
         stop("all elements of `lm` must be matrices")
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(use.listnames, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(use.dimnames, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(sep, any.missing = FALSE, len = 1L)
   }
   lv <- lapply(X = lm, FUN = m2v, along = along, use.dimnames = use.dimnames,
      sep = sep, check = FALSE)
   v <- lv2v(lv = lv, use.listnames = use.listnames, use.vecnames = TRUE,
      sep = sep, check = FALSE)
   return(v)
}

# 2LM ####

# a2lm #

#' (3D) Array to List of Matrices
#'
#' \code{a2lm} converts a (3D) array to a list of matrices. This is a simple call
#' to \code{asplit} with a default to convert the third dimension to a list dimension.
#'
#' @param a 3D array.
#'
#' @param along integer vector of length 1 specifying the dimension to slice the array
#' along. This dimension is converted to the list dimension. 1 = rows; 2 = columns;
#' 3 = layers.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{a} is a 3D array.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list of matrices - all with the same dimensions.
#'
#' @examples
#' a2lm(HairEyeColor)
#' a2lm(HairEyeColor, along = 1)
#' try_expr(a2lm(mtcars)) # error b/c  not a 3D array
#' @export
a2lm <- function(a, along = 3L, check = TRUE) {

   if (check) {
      checkmate::assertArray(a, min.d = 3L, max.d = 3L)
      checkmate::assertIntegerish(along, lower = 1L, upper = 3L,
         any.missing = FALSE, len = 1L)
   }
   asplit(x = a, MARGIN = along)
}

# LD2 ####

# ld2d #

#' List of Data-Frames to Data-Frame
#'
#' \code{ld2d} converts a list of data.frames to a data.frame. The function is
#' primarily for rbinding a list of data.frames (\code{along} = 1). An option to
#' cbind the list of data.frames is included (\code{along} = 2), but is just a call to
#' \code{data.frame(ld, stringsAsFactors = stringsAsFactors, check.names = check.names)}.
#'
#' @param ld list of data.frames.
#'
#' @param along integer vector of length 1 specifying which dimension the data.frames
#' from \code{ld} should be binded along: 1 is for rows and 2 is for columns.
#'
#' @param fill logical vector of length 1 specifying whether to fill in missing values
#' for any data.frames from \code{ld} that do not have all the columns. At this time,
#' \code{fill} is only available for rbinding and only used if \code{along} = 1.
#'
#' @param rtn.listnames.nm character of length 1 specifying what the name of the
#' column containing the names/positions of \code{ld} should be in the returned
#' data.frame. If NULL, then no column is created for the names/positions of \code{ld}
#' in the returned data.frame.
#'
#' @param rtn.rownames.nm character of length 1 specifying what the name of the
#' column containing the rownames of \code{ld}'s data.frames should be in the returned
#' data.frame. If NULL, then no column is created for the rownames of \code{ld}'s
#' data.frames in the returned data.frame.
#'
#' @param stringsAsFactors logical vector of length 1 specifying whether character columns
#' from \code{ld} should be converted to factors. Only available and used if \code{fill}
#' = FALSE.
#'
#' @param check.names logical vector of length 1 specifying whether the colnames
#' of the returned data.frame should be checked for duplicates and made unique.
#' Only used if for cbinding with \code{along} = 2.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{ld} is a list of data.frames.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return data.frame with the rows (if \code{along} = 1) or columns (if \code{along}
#' = 2) of \code{ld} binded together.
#'
#' @examples
#' # without listnames and default rownames
#' ld <- list(BOD*1, BOD*2, BOD*3)
#' ld2d(ld)
#' # with listnames and default rownames
#' names(ld) <- LETTERS[1:3]
#' ld2d(ld)
#' # without listnames and custom rownames
#' ld <- lapply(unname(ld), FUN = `row.names<-`, letters[1:6])
#' ld2d(ld)
#' # with listnames and custom rownames
#' ld <- setNames(ld, LETTERS[1:3])
#' ld2d(ld)
#' # can handle same named columns in different positions
#' ld <- list(BOD*1, rev(BOD*2), rev(BOD*3))
#' ld2d(ld)
#' # can handle some columns being absent with fill = TRUE
#' ld[[2]]$"demand" <- NULL
#' try_expr(ld2d(ld, fill = FALSE)) # error
#' ld2d(ld, fill = TRUE) # NAs added
#' # along = 2 for cbinding
#' ld2d(ld, along = 2) # does not check/rename for double colnames
#' ld2d(ld, along = 2, check.names = TRUE) # makes unique colnames
#' @export
ld2d <- function(ld, along = 1, fill = FALSE, rtn.listnames.nm = "list_names", rtn.rownames.nm = "row_names",
   stringsAsFactors = FALSE, check.names = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertList(ld, any.missing = FALSE)
      if (!(all(unlist(lapply(X = ld, FUN = is.data.frame)))))
         stop("all elements of `ld` must be data.frames")
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(fill, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(rtn.listnames.nm, any.missing = FALSE, len = 1L, null.ok = TRUE)
      checkmate::assertCharacter(rtn.rownames.nm, any.missing = FALSE, len = 1L, null.ok = TRUE)
      checkmate::assertLogical(stringsAsFactors, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(check.names, any.missing = FALSE, len = 1L)
      if (!fill) {
         if (along == 2 && 0 != var(unlist(lapply(X = ld, FUN = function(dfm) dim(dfm)[1]))))
            stop("if `along` = 2, all elements of `ld` must have the same number of rows")
         # have not figured out to apply `fill` = TRUE and `along` = 2...
         if (along == 1 && 0 != var(unlist(lapply(X = ld, FUN = function(dfm) dim(dfm)[2]))))
            stop("if `fill` = FALSE and `along` = 1, all elements of `ld` must have the same number of columns")
      }
   }
   if (along == 1) {
      if (!(is.null(rtn.rownames.nm))) {
         # don't need to check for empty rownames because data.frames have rownames by definition
         ld <- lapply(X = ld, FUN = function(d) {
            row_nm <- setNames(data.frame("tmp" = row.names(d)), nm = rtn.rownames.nm)
            cbind.data.frame(row_nm, d)
         })
      }
      if (!(is.null(rtn.listnames.nm))) {
         if (is.null(names(ld))) names(ld) <- seq_along(ld)
            ld <- Map(d = ld, nm = names(ld), f = function(d, nm) {
            list_nm <- setNames(data.frame("tmp" = rep.int(nm, times = nrow(d))), nm = rtn.listnames.nm)
            cbind.data.frame(list_nm, d)
         })
      }
      if (!fill) {
         ld_args <- ld
         ld_args[["stringsAsFactors"]] <- stringsAsFactors
         d <- do.call(what = `rbind.data.frame`, args = ld_args)
      }
      if (fill) d <- plyr::rbind.fill(ld) # cannot use ld_args because of the stringsAsFactors argument
      row.names(d) <- seq_len(nrow(d))
   }
   if (along == 2) {
      d <- data.frame(ld, stringsAsFactors = stringsAsFactors, check.names = check.names)
   }
   return(d)
}

# ld2v #

#' List of Data-Frames to (Atomic) Vector
#'
#' \code{ld2v} converts a list of data.frames to a (atomic) vector. This function is
#' a combination of \code{d2v} and \code{lv2v}. This function can be useful in
#' conjunction with the \code{boot::boot} function when wanting to generate a
#' \code{statistic} function that returns an atomic vector.
#'
#' When \code{use.listnames} and \code{use.dimnames} are both TRUE (default), the returned
#' vector elements the following naming scheme: "[listname][sep][rowname][sep][colname]".
#'
#' If the columns of the data.frames in \code{ld} are not all the same typeof, then
#' the return object is coerced to the most complex type of any data.frame column (e.g.,
#' character > double > integer > logical). See \code{unlist} for details about
#' the hierarchy of object types.
#'
#' @param ld list of data.frames. They do NOT have to have the same dimensions.
#'
#' @param along numeric vector of length one that is equal to either 1 or 2.
#' 1 means that each data.frame in \code{ld} is split along rows (i.e., dimension 1)
#' and then concatenated. 2 means that each data.frame in \code{ld} is split along columns
#' (i.e., dimension 2) and then concatenated.
#'
#' @param fct character vector of length 1 specifying what factors should be converted
#' to. There are three options: 1) "chr" for converting to character vectors (i.e.,
#' factor labels), 2) "int" for converting to integer vectors (i.e., factor codes),
#' or 3) "fct" for keeping the factor as is without any changes.
#'
#' @param chr character vector of length 1 specifying what character vectors should
#' be converted to. There are three options: 1) "fct" for converting to factors (i.e.,
#' elements will be factor labels), 2) "int" for converting to integer vectors (i.e.,
#' factor codes after first converting to a factor), or 3) "chr" for keeping the
#' character vectors as is without any changes.
#'
#' @param lgl character vector of length 1 specifying what logical vectors should
#' be converted to. There are four options: 1) "fct" for converting to factors (i.e.,
#' "TRUE" and "FALSE" will be factor labels), 2) "chr" for converting to character
#' vectors (i.e., elements will be "TRUE" and "FALSE"), 3) "int" for converting to
#' integer vectors (i.e., TRUE = 1; FALSE = 0), and 4) "lgl" for keeping the logical
#' vectors as is without any changes.
#'
#' @param order.lvl character vector of length 1 specifying how you want to order
#' the levels of the factor. The options are "alphanum", which sorts the levels
#' alphanumerically (with NA last); "position", which sorts the levels by the position
#' the level first appears; "frequency", which sorts the levels by their frequency.
#' If any frequencies are tied, then the ties are sorted alphanumerically (with NA last).
#'
#' @param decreasing logical vector of length 1 specifying whether the ordering of the
#' levels should be decreasing (TRUE) rather than increasing (FALSE).
#'
#' @param na.lvl logical vector of length 1 specifying if NA should be considered a level.
#'
#' @param use.listnames logical vector of length 1 specifying whether the returned
#' vector should have names based on the list the element came from. If \code{ld}
#' does not have names, \code{use.listnames} = TRUE will have the list positions
#' serve as the list names (e.g., "1", "2", "3", etc.)
#'
#' @param use.dimnames logical vector of length 1 specifying whether the returned
#' vector should have names based on the dimnames of the data.frame the element came from.
#' If a data.frame within \code{ld} does not have dimnames, \code{use.dimnames} = TRUE
#' will have the dimension positions serve as the dimnames (e.g., "1", "2", "3", etc.)
#'
#' @param sep character vector of length 1 specifying the string used to separate
#' the listnames and dimnames from each other when creating the names of the returned
#' vector.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{ld} is a list of data.frames.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return (atomic) vector with an element for each element from \code{ld}.
#'
#' @examples
#' ld <- list("cars" = cars, "mtcars" = mtcars)
#' # use.listnames = TRUE & use.dimnames = TRUE
#' ld2v(ld) # the first part of the name is the list names followed by the dimnames
#' # use.listnames = FALSE & use.dimnames = TRUE
#' ld2v(ld, use.listnames = FALSE) # only dimnames used,
#'    # which can result in repeat names
#' # use.listnames = TRUE & use.dimnames = FALSE
#' ld2v(ld, use.dimnames = FALSE) # listnames and vector position without any
#'    # reference to matrix dimensions
#' # use.listnames = FALSE & use.dimnames = FALSE
#' ld2v(ld, use.listnames = FALSE, use.dimnames = FALSE) # no names at all
#' # when list does not have names
#' ld <- replicate(n = 3, expr = attitude, simplify = FALSE)
#' ld2v(ld) # the first digit of the names is the list position and
#'    # the subsequent digits are the matrix dimnames
#' ld2v(ld, use.listnames = FALSE) # only dimnames used,
#'    # which can result in repeat names
#' @export
ld2v <- function(ld, along = 2, use.listnames = TRUE, use.dimnames = TRUE,
   sep = "_", fct = "chr", chr = "chr", lgl = "int", order.lvl = "alphanum",
   decreasing = FALSE, na.lvl = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertList(ld, any.missing = FALSE)
      if (!(all(unlist(lapply(X = ld, FUN = is.data.frame)))))
         stop("all elements of `ld` must be data.frames")
      if (!(is.element(el = along, set = c(1,2))))
         stop("`along` must be equal to 1 or 2")
      checkmate::assertLogical(use.listnames, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(use.dimnames, any.missing = FALSE, len = 1L)
      checkmate::assertCharacter(sep, any.missing = FALSE, len = 1L)
      fct <- match.arg(arg = fct, choices = c("chr","int","fct"))
      chr <- match.arg(arg = chr, choices = c("fct","int","chr"))
      lgl <- match.arg(arg = lgl, choices = c("fct","chr","int","lgl"))
      order.lvl <- match.arg(arg = order.lvl,
         choices = c("alphanum","position","frequency"), several.ok = FALSE)
      checkmate::assertLogical(decreasing, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(na.lvl, any.missing = FALSE, len = 1L)
   }
   lv <- lapply(X = ld, FUN = d2v, along = along, use.dimnames = use.dimnames,
      sep = sep, fct = fct, chr = chr, lgl = lgl, order.lvl = order.lvl,
      decreasing = decreasing, na.lvl = na.lvl, check = FALSE)
   v <- lv2v(lv = lv, use.listnames = use.listnames, use.vecnames = TRUE,
      sep = sep, check = FALSE)
   return(v)
}

# ld2a

#' List of Data-Frames to a 3D Array
#'
#' \code{ld2a} converts a list of data.frames to a 3D array. The data.frames must
#' have the same dimensions.
#'
#' If the columns of the data.frames in \code{ld} are not all the same typeof, then
#' the return object is coerced to the most complex type of any data.frame column (e.g.,
#' character > double > integer > logical). See \code{unlist} for details about
#' the hierarchy of object types.
#'
#' @param ld list of data.frames that all have the same dimensions.
#'
#' @param dim.order integer vector of length 3 specifying the order of dimensions for
#' the returned array. The default is \code{c(1,2,3)} which means the rows of the
#' data.frames in \code{ld} is the first dimension (i.e., rows), the columns of the
#' data.frames in \code{ld} is the second dimension (i.e., columns), and the list
#' elements of \code{ld} is the third dimension (i.e., layers).
#'
#' @param dimlab.list character vector of length 1 specifying the dimlabel for
#' the list dimension.
#'
#' @param fct character vector of length 1 specifying what factors should be converted
#' to. There are three options: 1) "chr" for converting to character vectors (i.e.,
#' factor labels), 2) "int" for converting to integer vectors (i.e., factor codes),
#' or 3) "fct" for keeping the factor as is without any changes.
#'
#' @param chr character vector of length 1 specifying what character vectors should
#' be converted to. There are three options: 1) "fct" for converting to factors (i.e.,
#' elements will be factor labels), 2) "int" for converting to integer vectors (i.e.,
#' factor codes after first converting to a factor), or 3) "chr" for keeping the
#' character vectors as is without any changes.
#'
#' @param lgl character vector of length 1 specifying what logical vectors should
#' be converted to. There are four options: 1) "fct" for converting to factors (i.e.,
#' "TRUE" and "FALSE" will be factor labels), 2) "chr" for converting to character
#' vectors (i.e., elements will be "TRUE" and "FALSE"), 3) "int" for converting to
#' integer vectors (i.e., TRUE = 1; FALSE = 0), and 4) "lgl" for keeping the logical
#' vectors as is without any changes.
#'
#' @param order.lvl character vector of length 1 specifying how you want to order
#' the levels of the factor. The options are "alphanum", which sorts the levels
#' alphanumerically (with NA last); "position", which sorts the levels by the position
#' the level first appears; "frequency", which sorts the levels by their frequency.
#' If any frequencies are tied, then the ties are sorted alphanumerically (with NA last).
#'
#' @param decreasing logical vector of length 1 specifying whether the ordering of the
#' levels should be decreasing (TRUE) rather than increasing (FALSE).
#'
#' @param na.lvl logical vector of length 1 specifying if NA should be considered a level.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{ld} is a list of data.frames.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return 3D array with all the elements from \code{ld} organized into dimensions
#' specified by \code{dim.order}.
#'
#' @examples
#' ld <- list("first" = BOD, "second" = BOD*2, "third" = BOD*3)
#' ld2a(ld)
#' ld <- list("cars" = cars, "mtcars" = mtcars)
#' try_expr(ld2a(ld)) # error
#' @export
ld2a <- function(ld, dim.order = c(1, 2, 3), dimlab.list = NULL, fct = "chr",
   chr = "chr", lgl = "int", order.lvl = "alphanum", decreasing = FALSE,
   na.lvl = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertList(ld, any.missing = FALSE)
      if (!(all(unlist(lapply(X = ld, FUN = is.data.frame)))))
         stop("all elements of `ld` must be data.frames")
      if (0 != var(unlist(lapply(X = ld, FUN = function(dfm) dim(dfm)[1]))))
         stop("all elements of `ld` must have the same number of rows")
      if (0 != var(unlist(lapply(X = ld, FUN = function(dfm) dim(dfm)[2]))))
         stop("all elements of `ld` must have the same number of columns")
      checkmate::assertIntegerish(dim.order, any.missing = FALSE, len = 3L)
      checkmate::assertCharacter(dimlab.list, any.missing = FALSE, len = 1L, null.ok = TRUE)
      fct <- match.arg(arg = fct, choices = c("chr","int","fct"))
      chr <- match.arg(arg = chr, choices = c("fct","int","chr"))
      lgl <- match.arg(arg = lgl, choices = c("fct","chr","int","lgl"))
      order.lvl <- match.arg(arg = order.lvl,
         choices = c("alphanum","position","frequency"), several.ok = FALSE)
      checkmate::assertLogical(decreasing, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(na.lvl, any.missing = FALSE, len = 1L)
   }

   lm <- lapply(ld, FUN = d2m, fct = fct, chr = chr, lgl = lgl, order.lvl = order.lvl,
      decreasing = decreasing, na.lvl = na.lvl, check = FALSE)
   a <- lm2a(lm = lm, dim.order = dim.order, dimlab.list = dimlab.list, check = FALSE)
   return(a)
}

# 2LD ####

# a2ld #

#' 3D Array to List of Data-Frames
#'
#' \code{a2ld} converts a 3D array to a list of data.frames. This is a simple call
#' to \code{a2lm} followed by \code{m2d}. The default is to convert the third
#' dimension to the list dimension.
#'
#' @param a 3D array.
#'
#' @param along integer vector of length 1 specifying the dimension to slice the array
#' along. This dimension is converted to the list dimension. 1 = rows; 2 = columns;
#' 3 = layers.
#'
#' @param stringsAsFactors logical vector of length 1 specifying whether character
#' vectors should be converted to factors. Note, that if the array is character
#' and \code{stringsAsFactors} = TRUE, then all columns in the returned list of
#' data.frames will be factors.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{a} is a 3D array.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list of data.frames - all with the same dimensions.
#'
#' @examples
#' a2ld(HairEyeColor)
#' a2ld(HairEyeColor, along = 1)
#' try_expr(a2ld(mtcars)) # error b/c not a 3D array
#' @export
a2ld <- function(a, along = 3L, stringsAsFactors = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertArray(a, min.d = 3L, max.d = 3L)
      checkmate::assertIntegerish(along, lower = 1L, upper = 3L,
         any.missing = FALSE, len = 1L)
   }
   lm <- a2lm(a = a, along = along, check = FALSE)
   ld <- lapply(X = lm, FUN = m2d, stringsAsFactors = stringsAsFactors)
   return(ld)
}

# LA2 ####

# la2a #

#' List of (3D+) Arrays to (3D+) Array
#'
#' \code{la2a} converts a list of (3D+) arrays to a one dimension larger (3D+)
#' array where the list dimension becomes the additional dimension of the array.
#' \code{la2a} is a simple wrapper function for \code{abind::abind}. If you have
#' a list of matrices, then use \code{lm2a}.
#'
#' @param la list of 3D+ arrays which each have the same dimensions.
#'
#' @param dim.order integer vector of length = \code{ndim(la[[1]]) + 1L} specifying
#' the order of dimensions for the returned array. The default is \code{1:(ndim(la[[1]]) + 1L)}
#' which means the arrays within \code{la} maintain their dimensions and the list
#' dimension is appended as the last dimension.
#'
#' @param dimlab.list character vector of length 1 specifying the dimlabel for
#' the list dimension.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{la} is a list of 3D+ arrays.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return 3D+ array where the list elements of \code{la} is now a dimension. The
#' order of the dimensions is determined by the argument \code{dim.order}. The
#' dimnames of the returned array is determined by the dimnames in \code{la[[1]]}
#' and \code{names(la)}.
#'
#' @examples
#' la <- list("one" = HairEyeColor, "two" = HairEyeColor*2, "three" = HairEyeColor*3)
#' la2a(la) # default
#' la2a(la, dimlab.list = "Multiple")
#' la2a(la, dim.order = c(4,3,1,2))
#' la2a(la, dim.order = c(4,3,1,2), dimlab.list = "Multiple")
#' @export
la2a <- function(la, dim.order = 1:(ndim(la[[1]]) + 1L), dimlab.list = NULL, check = TRUE) {

   if (check) {
      checkmate::assertList(la, any.missing = FALSE)
      if (!(all(unlist(lapply(X = la, FUN = checkmate::testArray, min.d = 3L)))))
         stop("all elements of `la` must be 3D+ arrays")
      la_dim <- lapply(X = la, FUN = dim)
      if (0 != var(unlist(lapply(X = la_dim, FUN = length))))
         stop("all elements of `la` must have the same number of dimensions")
      t_la_dim <- t_list(la_dim, rtn.atomic = TRUE)
      if (any(unlist(lapply(X = t_la_dim, FUN = function(vec) 0 != var(vec)))))
         stop("all elements of `la` must have the same dimensions in the same order")
      checkmate::assertIntegerish(dim.order, any.missing = FALSE, min.len = 4L)
      checkmate::assertCharacter(dimlab.list, any.missing = FALSE, len = 1L, null.ok = TRUE)
   }
   add_dim <- ndim(la[[1]]) + 1L
   tmp <- abind::abind(la, along = add_dim, force.array = TRUE, make.names = FALSE,
      use.first.dimnames = TRUE, use.dnns = TRUE)
   if (!(is.null(dimlab.list))) names(dimnames(tmp))[[add_dim]] <- dimlab.list
   a <- aperm(a = tmp, perm = dim.order)
   return(a)
}

# 2LA ####

# a2la #

#' (3D+) Array to List of (3D+) Arrays
#'
#' \code{a2la} converts an (3D+) array to a list of (3D+) arrays. This function
#' is a simple wrapper for \code{asplit(x = a, MARGIN = along)}.
#'
#' @param a (3D+) array
#'
#' @param along integerish vector of length 1 specifying the dimension to split
#' the array along. Default is the last dimension of \code{a}.
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{a} is a 3D+ array.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list of arrays where each array is one dimension less than \code{a}
#' and the names of the list are \code{dimnames(a)[[along]]}.
#'
#' @examples
#' # without dimnames
#' a <- abind::abind(HairEyeColor*1, HairEyeColor*2, HairEyeColor*3, along = 4L)
#' a2la(a)
#' # with dimnames
#' a <- abind::abind("one" = HairEyeColor*1, "two" = HairEyeColor*2,
#'    "three" = HairEyeColor*3, along = 4L)
#' a2la(a)
#' a2la(a, along = 1) # along = 1
#' @export
a2la <- function(a, along = ndim(a), check = TRUE) {

   if (check) {
      checkmate::assertArray(a, min.d = 3L)
      checkmate::assertIntegerish(along, lower = 1L, upper = ndim(a),
         any.missing = FALSE, len = 1L)
   }
   asplit(x = a, MARGIN = along)
}

# ENVIRONMENT ####

# e2l #

#' Environment to List
#'
#' \code{e2l} converts an environment to a list. The function assumes you don't
#' want *all* objects in an environment and uses \code{pick} to determine which
#' objects you want included. If you want all objects in an environment, then use
#' \code{grab(x = objects(envir, all.names = TRUE), envir)}.
#'
#' @param e environment to pull the objects from. Default is the global environment.
#'
#' @param val character vector specifying which objects from \code{e} will be extracted.
#' If \code{pat} = FALSE (default), then \code{val} can have length > 1, and exact
#' matching will be done via \code{is.element} (essentially \code{match}). If
#' \code{pat} = TRUE, then \code{val} has to be a character vector of length 1 and
#' partial matching will be done via \code{grepl} with the option of regular expressions
#' if \code{fixed} = FALSE (default).
#'
#' @param pat logical vector of length 1 specifying whether \code{val} should refer to
#' exact matching (FALSE) via \code{is.element} (essentially \code{match}) or partial
#' matching (TRUE) and/or use of regular expressions via \code{grepl}. See details
#' for a brief description of some common symbols and \code{help(regex)} for more.
#'
#' @param not logical vector of length 1 specifying whether \code{val} indicates
#' values that should be retained (FALSE) or removed (TRUE).
#'
#' @param fixed logical vector of length 1 specifying whether \code{val} refers to
#' values as is (TRUE) or a regular expression (FALSE). Only used if \code{pat} = TRUE.
#'
#' @param sorted logical vector of length 1 specifying whether the objects should
#' be sorted alphanumerically. If FALSE, the objects are usually in the order they
#' were initially created, but not always (see \code{help(objects)}).
#'
#' @param check logical vector of length 1 specifying whether to check the structure
#' of the input arguments. For example, check whether \code{e} is an environment.
#' This argument is available to allow flexibility in whether the user values
#' informative error messages (TRUE) vs. computational efficiency (FALSE).
#'
#' @return list with object contents from environment \code{e} with names as the
#' object names.
#'
#' @examples
#' model_1 <- lm(v2frm(names(attitude)), data = attitude)
#' model_2 <- lm(v2frm(names(mtcars)), data = mtcars)
#' model_3 <- lm(v2frm(names(airquality)), data = airquality)
#' e2l(val = "model_", pat = TRUE)
#' @export
e2l <- function(e = sys.frame(), val, pat = FALSE, not = FALSE, fixed = FALSE,
   sorted = FALSE, check = TRUE) {

   if (check) {
      checkmate::assertEnvironment(e)
      checkmate::assertCharacter(val, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(pat, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(not, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(fixed, any.missing = FALSE, len = 1L)
      checkmate::assertLogical(sorted, any.missing = FALSE, len = 1L)
   }

   tmp <- objects(envir = e, all.names = TRUE, sorted = sorted)
   x <- pick(x = tmp, val = val, pat = pat, not = not, nm = FALSE, fixed = fixed)
   l <- grab(x = x, envir = e)
   return(l)
}
