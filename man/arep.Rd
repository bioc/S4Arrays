\name{arep}

\alias{arep}
\alias{arep_times}
\alias{arep_times,ANY-method}
\alias{arep_each}
\alias{arep_each,ANY-method}

\title{Replicate array elements}

\description{
  \code{arep_times()} and \code{arep_each()} are multidimensional
  versions of \code{base::rep( , times=)} and \code{base::rep( , each=)},
  respectively.

  They're both generic functions with default methods that work on any
  array-like object that supports \code{[} (single-bracket subsetting).
}

\usage{
arep_times(x, times)
arep_each(x, each)
}

\arguments{
  \item{x}{
    An array-like object, that is, an ordinary array or any object with
    dimensions.
  }
  \item{times, each}{
    An integer vector \emph{parallel} to \code{dim(x)} i.e. with one element
    per dimension in \code{x}.
  }
}

\value{
  An array-like object of the same class as \code{x} and with dimensions
  \code{dim(x) * times} for \code{arep_times} or \code{dim(x) * each}
  for \code{arep_each}.
  The dimnames on \code{x} are propagated, if any.
}

\seealso{
  \itemize{
    \item \code{base::\link{rep}} in the \pkg{base} package.

    \item \link[base]{array} and \link[base]{matrix} objects in base R.
  }
}

\examples{
m <- matrix(1:10, nrow=2)
arep_times(m, c(4, 2))
arep_each(m, c(4, 2))

## Note that the output array is 'prod(times)' (or 'prod(each)') times
## bigger than the input array!
}
\keyword{array}
\keyword{methods}
