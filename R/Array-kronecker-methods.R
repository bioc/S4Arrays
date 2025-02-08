### =========================================================================
### kronecker() methods for Array objects
### -------------------------------------------------------------------------
###


.normarg_X <- function(X, ndim, argname="X")
{
    X_dim <- dim(X)
    if (is.null(X_dim))
        stop(wmsg("'", argname, "' must be an array-like object (or a vector)"))
    if (length(X_dim) >= ndim)
        return(X)
    set_dim(X, rpad_dim(X_dim, ndim))
}

.make_kronecker_dinmames <- function(Ldimnames, Rdimnames, ndim)
{
    if (is.null(Ldimnames) && is.null(Rdimnames))
        return(NULL)
    lapply(seq_len(ndim),
        function(along) {
            Ldn <- Ldimnames[[along]]
            Rdn <- Rdimnames[[along]]
            ## base::kronecker() will still combine the dimnames along a
            ## given dimension even if there's nothing to combine. As a result
            ## the returned object gets a bunch of meaningless ":" names along
            ## that dimension. We don't do that.
            if (is.null(Ldn) && is.null(Rdn))
                return(NULL)
            paste0(Ldn, ":", Rdn)
        })
}

### A simple re-implementation of base::kronecker() based on arep_times()
### and arep_each() with the main difference that kronecker2() acts as an
### endomorphism when 'X' and 'Y' have the same class.
kronecker2 <- function(X, Y, FUN="*", make.dimnames=FALSE, ...)
{
    FUN <- match.fun(FUN)
    if (!isTRUEorFALSE(make.dimnames))
        stop(wmsg("'make.dimnames' must be TRUE or FALSE"))
    if (is.vector(X))
        X <- as.array(X)  # 1D-array
    if (is.vector(Y))
        Y <- as.array(Y)  # 1D-array
    X <- .normarg_X(X, length(dim(Y)), argname="X")
    Y <- .normarg_X(Y, length(dim(X)), argname="Y")
    XX <- arep_each(X, each=dim(Y))
    YY <- arep_times(Y, times=dim(X))
    ans <- set_dim(FUN(XX, YY, ...), dim(XX))
    if (make.dimnames) {
        ans_dimnames <- .make_kronecker_dinmames(dimnames(XX), dimnames(YY),
                                                 length(dim(ans)))
    } else {
        ans_dimnames <- NULL
    }
    set_dimnames(ans, ans_dimnames)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### kronecker() methods for Array objects
###

setMethod("kronecker", c("Array", "ANY"), kronecker2)
setMethod("kronecker", c("ANY", "Array"), kronecker2)
setMethod("kronecker", c("Array", "Array"), kronecker2)

