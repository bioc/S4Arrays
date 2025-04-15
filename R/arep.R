### =========================================================================
### arep_times() & arep_each()
### -------------------------------------------------------------------------
###


### Also used to normalize the 'each' argument of arep_each().
.normarg_times <- function(times, x_dim, argname="times")
{
    if (!(is.vector(times) && is.numeric(times)))
        stop(wmsg("'", argname, "' must be a numeric vector"))
    if (!is.integer(times))
        times <- as.integer(times)
    if (length(times) != length(x_dim))
        stop(wmsg("'", argname, "' must have one element per array ",
                  "dimension, that is, 'length(", argname, ")' must ",
                  "equal 'length(dim(x))'"))
    if (isTRUE(any(times < 0L)))
        stop(wmsg("'", argname, "' cannot contain negative values"))
    isna <- is.na(times)
    if (any(isna & x_dim != 0L))
        stop(wmsg("NAs in '", argname, "' are only allowed along ",
                  "array dimensions equal to zero"))
    times[isna] <- 1L
    times
}

.check_returned_dim <- function(returned_dim, expected_dim, .Generic, x_class)
{
    ok <- length(returned_dim) == length(expected_dim) &&
          all(returned_dim == expected_dim)
    if (!ok)
        stop(wmsg("the ", .Generic, "() method for ", x_class, " objects ",
                  "returned an array with incorrect dimensions"))
}

### About 3x faster than 'base::rep(x, each=)'.
.fast_rep_each <- function(x, each)
{
    stopifnot(isSingleNumber(each))
    if (!is.integer(each))
        each <- as.integer(each)
    rep.int(x, rep.int(each, length(x)))
}

### Not used at the moment.
.head_of_ones_meets_tail_of_ones  <- function(a, b)
{
    stopifnot(is.integer(a), is.integer(b), length(a) == length(b))
    idxa <- which(a != 1L)
    if (length(idxa) == 0L)
        return(TRUE)
    idxb <- which(b != 1L)
    if (length(idxb) == 0L)
        return(TRUE)
    idx1 <- idxa[[1L]]            # index of first non-one in 'a'
    idx2 <- idxb[[length(idxb)]]  # index of last non-one in 'b'
    idx1 > idx2
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### arep_times()
###

### Multidimensional version of 'base::rep(x, times=)'.
### The 'times' argument is expected to be an integer vector with the
### same length as 'dim(x)'.
### Must act as an endomorphism i.e. must return an array-like object
### of the same class as 'x'.
### Notes:
### - output array is 'prod(times)' times bigger than input array!
### - 'arep_times(x, rep.int(1, length(dim(x))))' should always act as a no-op.
setGeneric("arep_times", signature="x",
    function(x, times)
    {
        x_dim <- dim(x)
        if (is.null(x_dim))
            stop(wmsg("the first argument to arep_times() must be an ",
                      "array-like object (i.e. it must have dimensions)"))
        times <- .normarg_times(times, x_dim)
        ans <- standardGeneric("arep_times")
        .check_returned_dim(dim(ans), x_dim * times,
                            "arep_times", class(x)[[1L]])
        ans
    }
)

### We can use subassignment with linear recycling if and only if all
### the dimensions in 'x_dim' that are on the right of the first non-one
### value in 'times' are ineffective dimensions (i.e. have an extent of 1).
.can_use_linear_recycling <- function(x_dim, times)
{
    idx <- which(times != 1L)
    if (length(idx) == 0L)
        return(TRUE)
    idx1 <- idx[[1L]]  # index of first non-one in 'times'
    all(tail(x_dim, n=-idx1) == 1L)
}

### Equivalent to '.head_of_ones_meets_tail_of_ones(x_dim, times)'.
.can_use_linear_rep_each <- function(x_dim, times)
{
    idx <- which(times != 1L)
    if (length(idx) == 0L)
        return(TRUE)
    idx2 <- idx[[length(idx)]]  # index of last non-one in 'times'
    all(head(x_dim, n=idx2) == 1L)
}

### Only for ordinary arrays at the moment.
.arep_times_using_linear_recycling <- function(x, times)
{
    stopifnot(is.array(x), is.integer(times))
    x_dim <- dim(x)
    stopifnot(length(x_dim) == length(times))
    array(x, dim=x_dim*times)
}

### Only for ordinary arrays at the moment.
.arep_times_using_linear_rep_each <- function(x, times)
{
    stopifnot(is.array(x), is.integer(times))
    x_dim <- dim(x)
    stopifnot(length(x_dim) == length(times))
    array(.fast_rep_each(x, prod(times)), dim=x_dim*times)
}

### Default method. Works on any array-like object 'x' that supports `[`.
.default_arep_times <- function(x, times)
{
    ## The arep_times() generic already took care of checking/validating 'x'
    ## and 'times'. See above.
    x_dim <- dim(x)
    if (is.array(x)) {
        if (.can_use_linear_recycling(x_dim, times))
            return(.arep_times_using_linear_recycling(x, times))
        ## Not the speedup I was hoping for!
        #if (.can_use_linear_rep_each(x_dim, times))
        #    return(.arep_times_using_linear_rep_each(x, times))
    }
    index <- lapply(seq_along(times),
        function(along) rep.int(seq_len(x_dim[[along]]), times[[along]]))
    subset_by_Nindex(x, index)  # relies on `[`
}

### An alternative to .default_arep_times() based on abind().
### Timings on ordinary arrays indicate that .default_arep_times() tend to
### be significantly faster than .default_arep_times2(), except when 'x' is
### big and the values in 'times' are small (e.g. 'prod(times)' < 100), in
### which case the latter is only marginally faster. So not worth it!
.default_arep_times2 <- function(x, times)
{
    ## The arep_times() generic already took care of checking/validating 'x'
    ## and 'times'. See above.
    for (along in seq_along(times)) {
        t <- times[[along]]
        if (t == 1L)
            next
        args <- c(rep.int(list(x), t), list(along=along))
        x <- do.call(abind, args)
    }
    x
}

setMethod("arep_times", "ANY", .default_arep_times)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### arep_each()
###

### Multidimensional version of 'base::rep(x, each=)'.
### The 'each' argument is expected to be an integer vector with the
### same length as 'dim(x)'.
### Must act as an endomorphism i.e. must return an array-like object
### of the same class as 'x'.
### Notes:
### - output array is 'prod(each)' times bigger than input array!
### - 'arep_each(x, rep.int(1, length(dim(x))))' should always act as a no-op.
setGeneric("arep_each", signature="x",
    function(x, each)
    {
        x_dim <- dim(x)
        if (is.null(x_dim))
            stop(wmsg("the first argument to arep_each() must be an ",
                      "array-like object (i.e. it must have dimensions)"))
        each <- .normarg_times(each, x_dim, argname="each")
        ans <- standardGeneric("arep_each")
        .check_returned_dim(dim(ans), x_dim * each,
                            "arep_each", class(x)[[1L]])
        ans
    }
)

### Default method. Works on any array-like object 'x' that supports `[`.
.default_arep_each <- function(x, each)
{
    ## The arep_each() generic already took care of checking/validating 'x'
    ## and 'each'. See above.
    x_dim <- dim(x)
    index <- lapply(seq_along(each),
        function(along) .fast_rep_each(seq_len(x_dim[[along]]), each[[along]]))
    subset_by_Nindex(x, index)  # relies on `[`
}

setMethod("arep_each", "ANY", .default_arep_each)

