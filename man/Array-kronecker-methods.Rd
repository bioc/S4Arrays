\name{Array-kronecker-methods}

\alias{Array-kronecker-methods}
\alias{Array_kronecker-methods}
\alias{Array-kronecker}
\alias{Array_kronecker}
\alias{kronecker}

\alias{kronecker,Array,ANY-method}
\alias{kronecker,ANY,Array-method}
\alias{kronecker,Array,Array-method}
\alias{kronecker2}

\title{Kronecker products on Array objects}

\description{
  The \pkg{S4Arrays} package implements \code{kronecker()} methods
  for \link{Array} objects that work out-of-the-box on \link{Array}
  derivatives that support \code{[} and \code{*}.

  Note that \code{kronecker} is a generic function defined in the
  \pkg{methods} package but documented in the \pkg{base} package.
  See \code{?base::\link{kronecker}}.
}

\usage{
\S4method{kronecker}{Array,ANY}(X, Y, FUN="*", make.dimnames=FALSE, ...)
\S4method{kronecker}{ANY,Array}(X, Y, FUN="*", make.dimnames=FALSE, ...)
\S4method{kronecker}{Array,Array}(X, Y, FUN="*", make.dimnames=FALSE, ...)

## The workhorse behind the three above methods.
kronecker2(X, Y, FUN="*", make.dimnames=FALSE, ...)
}

\arguments{
  \item{X, Y}{
    Array-like objects. Alternatively \code{X} and/or \code{Y} can
    be vectors, in which case they are converted to 1D-arrays with
    \code{as.array()}.

    Note that \code{X} and \code{Y} are expected to have the same
    number of dimensions. However, when they don't, \emph{ineffective
    dimensions} (i.e. dimensions with an extent of 1) are added to the
    object with less dimensions.

    For the S4 methods, at least one of \code{X} or \code{Y}
    must be an \link{Array} derivative.
  }
  \item{FUN, make.dimnames, ...}{
    See \code{?base::\link[base]{kronecker}} for a description of
    these arguments.
  }
}

\details{
  The three \code{kronecker()} methods listed above
  delegate to \code{kronecker2()}, a re-implementation of
  \code{base::\link{kronecker}()}.

  \code{kronecker2()} is semantically equivalent to
  \code{base::\link{kronecker}}. However, unlike the latter which
  calls \code{as.array()} on \code{X} and \code{Y} internally,
  the former \emph{operates natively} on the input objects regardless
  of their internal representations, as long as they are array-like
  objects that support \code{[} (single-bracket subsetting) and \code{*}.
  In particular, when \code{X} and \code{Y} use the same internal
  representations, the returned object will also use that representation.
  In other words, the output object will have the same class as the
  input objects (\emph{endomorphism}).

  The \emph{endomorphism} property is particularly important when the input
  objects are sparse (e.g. \link[SparseArray]{SVT_SparseArray} objects
  from the \pkg{SparseArray} package) or when they use an on-disk
  representation (e.g. \link[DelayedArray]{DelayedArray} objects from
  the \pkg{DelayedArray} package). For example, if \code{X} and \code{Y}
  are \link[DelayedArray]{DelayedArray} objects, the returned object is
  another \link[DelayedArray]{DelayedArray} object. Also in that case,
  calling \code{kronecker2()} is virtually instantaneous because all the
  operations that the function performs internally on \code{X} and
  \code{Y} by the are delayed!
}

\value{
  An array-like object with dimensions \code{dim(X) * dim(Y)}.
}

\seealso{
  \itemize{
    \item \code{base::\link{kronecker}} in the \pkg{base} package.

    \item The "Kronecker product" page on Wikipedia:
          \url{https://en.wikipedia.org/wiki/Kronecker_product}

    \item The \link{Array} class.

    \item \link[SparseArray]{SparseArray} objects implemented in the
          \pkg{SparseArray} package.

    \item \link[DelayedArray]{DelayedArray} objects implemented in the
          \pkg{DelayedArray} package.

    \item \link[HDF5Array]{TENxMatrix} objects implemented in the
          \pkg{HDF5Array} package.
  }
}

\examples{
## ---------------------------------------------------------------------
## SIMPLE kronecker2() EXAMPLES
## ---------------------------------------------------------------------

m1 <- matrix(1:10, nrow=2)     # 2 x 5 matrix
m2 <- matrix(101:106, nrow=3)  # 3 x 2 matrix
kronecker2(m1, m2)             # 6 x 10 matrix

a1 <- array(1:16, dim=c(4, 2, 2))
a2 <- array(1:30, dim=c(3, 5, 2))
kronecker2(a1, a2)    # 12 x 10 x 4 array

## The Kronecker product is **not** commutative:
m1 <- matrix(LETTERS[1:10], nrow=2)
m2 <- matrix(letters[1:6], nrow=3)
kronecker2(m1, m2, paste, sep="*")
kronecker2(m2, m1, paste, sep="*")

## Sanity checks:
stopifnot(
  identical(kronecker2(m1, m2, paste0), kronecker(m1, m2, paste0)),
  identical(kronecker2(m2, m1, paste0), kronecker(m2, m1, paste0))
)

## ---------------------------------------------------------------------
## USING kronecker() ON Array DERIVATIVES
## ---------------------------------------------------------------------
## The user should typically avoid direct calls to kronecker2() and
## stick to kronecker(). Because this is a generic function, it will
## dispatch to the appropriate method based on the classes of the input
## objects. If one of them is an Array derivative, kronecker2() will
## be called thanks to the methods defined in the S4Arrays package and
## listed in the Usage section above.

## With SparseMatrix objects (Array derivatives):
library(SparseArray)
sm1 <- poissonSparseMatrix(300, 15, density=0.25)
sm2 <- poissonSparseMatrix(80, 500, density=0.15)
kronecker2(sm1, sm2)  # 24000 x 7500 SparseMatrix object

## With TENxMatrix objects (DelayedArray derivatives, therefore also
## Array derivatives):
library(HDF5Array)
M1 <- writeTENxMatrix(sm1)  # 300 x 15 TENxMatrix object
M2 <- writeTENxMatrix(sm2)  # 80 x 500 TENxMatrix object
K <- kronecker2(M1, M2)     # instantaneous! (all operations are delayed)
showtree(K)  # show delayed operations details

## ---------------------------------------------------------------------
## VERIFYING THE MIXED-PRODUCT PROPERTY (JUST FOR FUN!)
## ---------------------------------------------------------------------
## See https://en.wikipedia.org/wiki/Kronecker_product for details
## about "The mixed-product property".

## We verify the property on 4 random matrices:
A <- matrix(runif(1000), ncol=40)
B <- matrix(runif(800), ncol=100)
C <- matrix(runif(600), nrow=40)
D <- matrix(runif(5000), nrow=100)

kAB <- kronecker2(A, B)
kCD <- kronecker2(C, D)
kAB_x_kCD <- kAB \%*\% kCD
A_x_C <- A \%*\% C
B_x_D <- B \%*\% D
stopifnot(all.equal(kAB_x_kCD, kronecker2(A_x_C, B_x_D)))

## The mixed-product property also for the element-wise product
## (Hadamard product). We verify this on 4 random arrays:
A <- array(1:60, dim=5:3)
B <- array(101:180, dim=c(2,10,4))
C <- array(runif(60), dim=5:3)
D <- array(runif(80), dim=c(2,10,4))

kAB <- kronecker2(A, B)
kCD <- kronecker2(C, D)
kAB_o_kCD <- kAB * kCD
A_o_C <- A * C
B_o_D <- B * D
stopifnot(all.equal(kAB_o_kCD, kronecker2(A_o_C, B_o_D)))
}
\keyword{array}
\keyword{methods}
