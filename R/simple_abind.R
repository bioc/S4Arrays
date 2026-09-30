### =========================================================================
### A stripped-down version of abind::abind() that is also faster
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###


.normarg_along <- function(along)
{
    if (!isSingleNumber(along))
        stop(wmsg("'along' must be a single number"))
    if (!is.integer(along))
        along <- as.integer(along)
    if (along <= 0L)
        stop(wmsg("'along' must be a positive integer"))
    along
}

### Return a matrix with one row per dim and one column per object if the
### objects are "bindable". Otherwise return a string describing why they
### are not. This design allows the function to be used in the context of
### a validity method.
get_dims_to_bind <- function(objects, along)
{
    stopifnot(is.list(objects))
    along <- .normarg_along(along)
    dims <- lapply(objects, dim)
    ndims <- lengths(dims)
    if (any(ndims < along))
        stop(wmsg("the array-like objects to bind must have at least ",
                  along, " dimensions for this binding operation"))
    ndim <- ndims[[1L]]
    if (!all(ndims == ndim))
        return(paste0("all the objects to bind must have ",
                      "the same number of dimensions"))
    tmp <- unlist(dims, use.names=FALSE)
    if (is.null(tmp))
        return("the objects to bind have no dimensions")
    dims <- matrix(tmp, nrow=ndim)
    tmp <- dims[-along, , drop=FALSE]
    if (!all(tmp == tmp[ , 1L]))
        return("the objects to bind have incompatible dimensions")
    dims
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### combine_dims_along() and combine_dimnames_along()
###
### Compute the expected dims and dimnames of abind()'s result.
### Both combine_dims_along() and combine_dimnames_along() generalize what
### rbind/cbind do in the 2D case to the multidimensional case.
###

### Combine the dims the rbind/cbind way.
combine_dims_along <- function(dims, along)
{
    stopifnot(is.matrix(dims), is.integer(dims))
    along <- .normarg_along(along)
    stopifnot(along <= nrow(dims))
    ans_dim <- dims[ , 1L]
    ans_dim[[along]] <- sum(dims[along, ])
    ans_dim
}

### Assume all the arrays in 'objects' have the same number of dimensions.
.combine_dimnames <- function(objects)
{
    lapply(seq_along(dim(objects[[1L]])),
        function(n) {
            for (object in objects) {
                dn <- dimnames(object)[[n]]
                if (!is.null(dn))
                    return(dn)
            }
            NULL
        })
}

### Combine the dimnames the rbind/cbind way.
combine_dimnames_along <- function(objects, dims, along)
{
    stopifnot(is.matrix(dims), is.integer(dims),
              isSingleInteger(along), along >= 1L, along <= nrow(dims))
    dimnames <- .combine_dimnames(objects)
    along_names <- lapply(objects, function(object) dimnames(object)[[along]])
    if (!all(S4Vectors:::sapply_isNULL(along_names))) {
        fix_idx <- which(lengths(along_names) != dims[along, ])
        along_names[fix_idx] <- lapply(dims[along, fix_idx], character)
        dimnames[[along]] <- unlist(along_names, use.names=FALSE)
    }
    simplify_NULL_dimnames(dimnames)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### simple_abind()
###

### 'objects' is assumed to be a list of vector-like objects.
### 'nblock' is assumed to be a single integer value (stored as a numeric)
### that is a common divisor of the object lengths.
.intertwine_blocks <- function(objects, nblock, ans_dim)
{
    x0 <- unlist(lapply(objects, `[`, 0L), recursive=FALSE, use.names=FALSE)
    objects_lens <- lengths(objects)
    if (all(objects_lens == 0L))
        return(set_dim(x0, ans_dim))

    idx <- which(vapply(objects,
        function(object) { typeof(object) != typeof(x0) },
        logical(1),
        USE.NAMES=FALSE))
    if (length(idx) != 0L)
        objects[idx] <- lapply(objects[idx], `storage.mode<-`, typeof(x0))

    .Call2("C_abind", objects, nblock, ans_dim, PACKAGE="S4Arrays")
}

### A stripped-down version of abind::abind().
### Some differences:
###   (a) Treatment of dimnames: simple_abind() treatment of dimnames is
###       consistent with base::rbind() and base::cbind(). This is not the
###       case for abind::abind() which does some strange things with the
###       dimnames.
###   (b) Performance: simple_abind() is much faster than abind::abind()
###       (between 3x and 15x). Also note that in the 'along=1L' and 'along=2L'
###       cases, it's generally as fast (and most of the time faster) than
###       base::rbind() and base::cbind().
###       For example, with 'm <- matrix(1:30000000, nrow=5000)',
###       'simple_abind(m, m, m, along=1L)' is 14x faster than
###       'abind::abind(m, m, m, along=1L)' and 11x faster than
###       'base::rbind(m, m, m)'.
###   (c) abind::abind() is broken on matrices of type "list".
simple_abind <- function(..., along)
{
    along <- .normarg_along(along)
    objects <- S4Vectors:::delete_NULLs(list(...))
    if (length(objects) == 0L)
        return(NULL)

    ## Check dim compatibility.
    dims <- get_dims_to_bind(objects, along)
    if (is.character(dims))
        stop(wmsg(dims))
    if (length(objects) == 1L)
        return(objects[[1L]])

    ## Perform the binding.
    nblock <- prod(dims[-seq_len(along), 1L])  # numeric that can be >
                                               # .Machine$integer.max
    ans <- .intertwine_blocks(objects, nblock, combine_dims_along(dims, along))

    ## Combine and set the dimnames.
    set_dimnames(ans, combine_dimnames_along(objects, dims, along))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### homogenize_abind_input_objects()
###

.normarg_rev_along <- function(rev.along)
{
    if (!isSingleNumber(rev.along))
        stop(wmsg("'rev.along' must be a single number"))
    if (!is.integer(rev.along))
        rev.along <- as.integer(rev.along)
    if (rev.along < 0L)
        stop(wmsg("'rev.along' must be a non-negative integer"))
    rev.along
}

### To be re-used by other abind() methods e.g. by the method for SparseArray
### objects.
get_abind_along <- function(N, along=NULL, rev.along=NULL)
{
    if (is.null(along) && is.null(rev.along))
        return(N)
    if (is.null(rev.along)) {
        along <- .normarg_along(along)
        if (along > N + 1L)
            stop(wmsg("'along' must be <= N + 1, where N is the number of ",
                      "dimensions of the arrays with the most dimensions"))
    } else {
        rev.along <- .normarg_rev_along(rev.along)
        if (rev.along > N)
            stop(wmsg("'rev.along' must be <= N, where N is the number of ",
                      "dimensions of the arrays with the most dimensions"))
        along <- N + 1L - rev.along
    }
    along
}

### 'ndim' is expected to be >= number of dimensions of the arrays with
### the most dimensions.
### To be re-used by other abind() methods e.g. by the method for SparseArray
### objects.
.add_missing_dims <- function(objects, ndim)
{
    stopifnot(is.list(objects), isSingleInteger(ndim))
    lapply(objects,
        function(object) {
            object_dim <- dim(object)
            object_ndim <- length(object_dim)
            if (object_ndim >= ndim)
                return(object)
            set_dim(object, c(object_dim, rep.int(1L, ndim - object_ndim)))
        }
    )
}

### Has its own unit tests!
### Homogenization transforms the input objects in two ways:
###   1. Coerce them to a common class (if 'coerce.to.target' is TRUE).
###      See coerce_abind_input_objects() in R/coerce_abind_input_objects.R
###      for the details.
###   2. Make them have the same number of dimensions.
### Note that none of these transformations is guaranteed to succeed.
homogenize_abind_input_objects <- function(objects, along=NULL, rev.along=NULL,
                                           coerce.to.target=FALSE)
{
    stopifnot(is.list(objects), isTRUEorFALSE(coerce.to.target))

    objects <- S4Vectors:::delete_NULLs(objects)
    if (length(objects) == 0L)
        return(NULL)

    ## Make sure to do this **before**  coercing the objects to the target
    ## class below so we fail early in case some objects are not array-like
    ## or the supplied 'along' or 'rev.along' don't make sense.
    ndims <- vapply(objects,
        function(object) {
            object_dim <- dim(object)
            if (is.null(object_dim))
                stop(wmsg("some of the objects to bind don't have dimensions"))
            length(object_dim)
        }, integer(1))
    N <- max(ndims)
    along <- get_abind_along(N, along=along, rev.along=rev.along)

    if (coerce.to.target)
        objects <- coerce_abind_input_objects(objects)

    objects <- .add_missing_dims(objects, max(N, along))

    ## Check dim compatibility.
    dims <- get_dims_to_bind(objects, along)
    if (is.character(dims))
        stop(wmsg(dims))

    attr(objects, "dims") <- dims
    attr(objects, "along") <- along
    objects
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### simple_abind2()
###
### A wrapper to simple_abind() that adds some of the capabilities originally
### found in abind::abind():
### 1. Not all arrays (supplied via 'objects') are required to have the same
###    number of dimensions. If N is the number of dimensions of the arrays
###    with the most dimensions, then artifical (a.k.a. ineffective) dimensions
###    are added to the arrays with less dimensions so that they also have
###    N dimensions.
### 2. 'along' can be any integer value between 1 and N+1. When set to N+1,
###    then one more artifical dimension is added to all arrays.
### 3. simple_abind2() supports the 'rev.along' argument which can be any
###    value between 0 and N.
### 4. Arguments 'along' and 'rev.along' can be omitted, in which case binding
###    happens along the N-th dimension.

simple_abind2 <- function(objects, along=NULL, rev.along=NULL)
{
    objects <- homogenize_abind_input_objects(objects,
                                              along=along, rev.along=rev.along)
    if (length(objects) == 0L)
        return(NULL)
    do.call(simple_abind, c(objects, list(along=attr(objects, "along"))))
}

