\name{extract_array}

\alias{extract_array}
\alias{extract_array,ANY-method}
\alias{extract_array,array-method}
\alias{extract_array,data.frame-method}
\alias{extract_array,DataFrame-method}

\alias{as.array.Array}
\alias{as.array,Array-method}

\alias{as.data.frame.Array}
\alias{as.data.frame,Array-method}
\alias{as.vector.Array}
\alias{as.vector,Array-method}
\alias{as.logical.Array}
\alias{as.logical,Array-method}
\alias{as.integer.Array}
\alias{as.integer,Array-method}
\alias{as.numeric.Array}
\alias{as.numeric,Array-method}
\alias{as.complex.Array}
\alias{as.complex,Array-method}
\alias{as.character.Array}
\alias{as.character,Array-method}
\alias{as.raw.Array}
\alias{as.raw,Array-method}

\title{extract_array}

\description{
  \code{extract_array} is an internal generic function not intended to be
  used directly by the end user. It has methods defined for array, data.frame,
  \link[S4Vectors]{DataFrame} objects, and other array-like objects.

  Note that \code{extract_array} is part of the \emph{seed contract} as
  defined in the \emph{Implementing A DelayedArray Backend} vignette from
  the \pkg{DelayedArray} package.
}

\usage{
## The extract_array() S4 generic:

extract_array(x, index)

## extract_array() methods defined in the S4Arrays package:

\S4method{extract_array}{ANY}(x, index)

\S4method{extract_array}{array}(x, index)

\S4method{extract_array}{data.frame}(x, index)

\S4method{extract_array}{DataFrame}(x, index)
}

\arguments{
  \item{x}{
    An array-like object.

    This can be an ordinary array, a \link[SparseArray]{SparseArray}
    object from the \pkg{SparseArray} package, a \linkS4class{dgCMatrix}
    object from the \pkg{Matrix} package, a \link[DelayedArray]{DelayedArray}
    object from the \pkg{DelayedArray} package, or any object with an array
    semantic (i.e. an object for which \code{dim(x)} is not NULL).

    Note that data.frame and \link[S4Vectors]{DataFrame} objects are also
    supported.
  }
  \item{index}{
    An unnamed list of integer vectors, one per dimension in \code{x}.
    Each vector is called a \emph{subscript} and can only contain
    positive integers that are valid 1-based indices along the corresponding
    dimension in \code{x}.

    Empty or missing subscripts are allowed. They must be represented
    by list elements set to \code{integer(0)} or \code{NULL}, respectively.

    The subscripts cannot contain NAs or non-positive values.

    Individual subscripts are allowed to contain duplicated indices.
  }
}

\details{
  \code{extract_array()} methods need to support empty or missing subscripts.
  For example, if \code{x} is an M x N matrix-like object, then
  \code{extract_array(x, list(NULL, integer(0)))} must return an M x 0
  ordinary matrix, and \code{extract_array(x, list(integer(0), integer(0)))}
  a 0 x 0 ordinary matrix.

  Also subscripts are allowed to contain duplicated indices so things like
  \code{extract_array(x, list(c(1:3, 3:1), 2L))} need to be supported.

  Finally, for maximum efficiency, \code{extract_array()} methods
  should not try to do anything with the dimnames on \code{x}.
}

\value{
  An \emph{ordinary} array of the same \code{type()} as \code{x}.
  For example, if \code{x} is an object representing an M x N matrix
  of complex numbers (i.e. \code{type(x) == "complex"}), then
  \code{extract_array(x, list(NULL, 2L))} must return the 2nd column
  in \code{x} as an M x 1 \emph{ordinary} matrix of \code{type()}
  \code{"complex"}.
}

\seealso{
  \itemize{
    \item \code{S4Arrays::\link[S4Arrays]{type}} to get the type of the
          elements of an array-like object.

    \item \link[base]{array} and \link[base]{data.frame} objects in base R.

    \item \link[SparseArray]{SparseArray} objects implemented in the
          \pkg{SparseArray} package.

    \item \link[DelayedArray]{DelayedArray} objects implemented in the
          \pkg{DelayedArray} package.

    \item \link[S4Vectors]{DataFrame} objects implemented in the
          \pkg{S4Vectors} package.
  }
}

\examples{
extract_array
showMethods("extract_array")

## extract_array() works on array-like objects like SparseArray objects,
## dgCMatrix objects, DataFrame objects, etc...

## --- On a SparseArray object ---

library(SparseArray)
a <- array(0L, 5:3)
a[c(1:2, 8, 10, 15:17, 20, 24, 40, 56:60)] <- (1:15)*10L
svt <- as(a, "SparseArray")
svt

extract_array(svt, list(NULL, c(4L,2L,4L), 1L))
extract_array(svt, list(NULL, c(4L,2L,4L), 2:3))
extract_array(svt, list(NULL, c(4L,2L,4L), integer(0)))

## Sanity checks:
stopifnot(
  identical(extract_array(svt, list(NULL, c(4L,2L,4L), 1L)),
            as.array(svt)[ , c(4L,2L,4L), 1L, drop=FALSE]),
  identical(extract_array(svt, list(NULL, c(4L,2L,4L), 2:3)),
            as.array(svt)[ , c(4L,2L,4L), 2:3]),
  identical(extract_array(svt, list(NULL, c(4L,2L,4L), integer(0))),
            as.array(svt)[ , c(4L,2L,4L), integer(0)])
)

## --- On a dgCMatrix object ---

library(Matrix)
m <- a[ , , 1]
dgcm <- as(m, "dgCMatrix")
dgcm

extract_array(dgcm, list(NULL, c(4L,2L,4L)))

## Sanity check:
stopifnot(
  identical(extract_array(dgcm, list(NULL, c(4L,2L,4L))),
            as.matrix(dgcm)[ , c(4L,2L,4L)])
)

## --- On a data.frame or DataFrame object ---

df <- data.frame(a=44:49, b=letters[1:6], c=c(TRUE, FALSE))
DF <- as(df, "DataFrame")

extract_array(df, list(4:2, c(1L,3L)))
extract_array(DF, list(4:2, c(1L,3L)))

## Sanity check:
target <- as.matrix(df)[4:2, c(1L,3L)]
dimnames(target) <- NULL
stopifnot(
  identical(extract_array(df, list(4:2, c(1L,3L))), target),
  identical(extract_array(DF, list(4:2, c(1L,3L))), target)
)
}
\keyword{internal}
\keyword{array}
