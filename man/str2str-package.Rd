% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str2str_functions.R
\docType{package}
\name{str2str-package}
\alias{str2str}
\alias{str2str-package}
\title{Structure to Structure}
\description{
\code{str2str} is a package for converting R objects to
  different structures. It focuses on four primary R objects: (atomic)
  vectors, matrices, data.frames, and arrays as well as lists of these
  objects. For example, converting a (atomic) vector to a data.frame (i.e.,
  \code{v2d()}) or a list of (atomic) vectors to a matrix (i.e.,
  \code{lv2m()}. The current version of the package does not have a function
  for every convertion (e.g., \code{a2m()}), but some additional convertion
  functions may be included in future versions if I find a use for them. The
  package was motivated by limitations of the base R \code{as.<str>.<method>}
  suite of functions and the \code{plyr} R package \code{**ply(.fun = NULL)}
  suite of functions for converting R objects to different structures. While
  those functions are often useful, there are times different conversions are
  desired or different naming schemes are desired. That is what this package
  offers R users. It also contains various utility functions for working with
  common R objects. For example, \code{is.colnames} and \code{ndim}.
}
\section{Limitations}{
 This packages does NOT handle the nuances of R objects.
  It is not for comprehensive restructuring of any version of R objects, but
  rather for restructuring commonly used versions of R objects. For example,
  the functions are not tested with the raw and complex typeof atomic
  vectors, list arrays, or data.frames containing non-atomic vector columns
  (e.g., matrix or list columns). The base R \code{as.<str>.<method>}
  functions allow for comprehensive restructuring of R objects; however, at
  the cost of less convenient convertions for commonly used versions of R
  objects. The \code{str2str} package seeks to fill that gap in useability.
}

\section{Abbreviations}{
 \describe{See the table below
   \item{v}{(atomic) vector}
   \item{m}{matrix}
   \item{d}{data.frame}
   \item{a}{(3D+) array}
   \item{l}{list}
   \item{el}{elements}
   \item{nm}{names}
   \item{uv}{unique values}
   \item{lgl}{logical}
   \item{int}{integer}
   \item{dbl}{double}
   \item{num}{numeric}
   \item{chr}{character}
   \item{fct}{factor}
   \item{lvl}{levels}
   \item{vrb}{variable}
   \item{frm}{formula}
   \item{fun}{function}
   \item{rtn}{return}
   \item{str}{structure}
}
}

\author{
\strong{Maintainer}: David Disabato \email{ddisab01@gmail.com}

}
