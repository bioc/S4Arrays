### =========================================================================
### read_block()
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The read_block_as_dense() generic
###
### Not meant to be called directly by the end user. They should call
### higher-level user-facing read_block() function instead, with
### the 'as.sparse' argument set to FALSE.
### Must return an ordinary array.
### Note that the read_block() frontend will take care of propagating the
### dimnames, so, for the sake of efficiency, individual methods should not
### try to do it.

setGeneric("read_block_as_dense", signature="x",
    function(x, viewport) standardGeneric("read_block_as_dense")
)

### This default read_block_as_dense() method will work on any object 'x'
### that supports extract_array() e.g. an ordinary array, a sparseMatrix
### derivative from the Matrix package, a SparseArray derivative,
### a DelayedArray object, a DelayedOp object, an HDF5ArraySeed object, etc...
### Does NOT propagate the dimnames.
setMethod("read_block_as_dense", "ANY",
    function(x, viewport)
    {
        Nindex <- makeNindexFromArrayViewport(viewport, expand.RangeNSBS=TRUE)
        extract_array(x, Nindex)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### read_block()
###

.load_SparseArray_for_read_block <- function(...)
    load_package_gracefully("SparseArray", "calling read_block() ", ...)

.read_block <- function(x, viewport, as.sparse=NA)
{
    if (is_sparse(x)) {
        .load_SparseArray_for_read_block("on a ", class(x), " object ")
        ## Should return a SparseArray derivative (COO_SparseArray or
        ## SVT_SparseArray) from the SparseArray package.
        ans <- SparseArray::read_block_as_sparse(x, viewport)
        SparseArray:::check_returned_SparseArray(
                             ans, dim(viewport),
                             "read_block_as_sparse", class(x))
        if (isFALSE(as.sparse))
            ans <- as.array(ans)
    } else {
        ## Should return an ordinary array (i.e. dense).
        ans <- read_block_as_dense(x, viewport)
        check_returned_array(ans, dim(viewport),
                             "read_block_as_dense", class(x))
        if (isTRUE(as.sparse)) {
            .load_SparseArray_for_read_block("with 'as.sparse=TRUE'")
            ans <- as(ans, "SparseArray")
        }
    }
    ans
}

### A user-facing frontend for read_block_as_dense() and
### SparseArray::read_block_as_sparse().
### Reads a block of data from array-like object 'x'. Depending on the value
### of argument 'as.sparse', the block is returned either as an ordinary
### array (dense representation) or a SparseArray object (sparse
### representation).
### 'as.sparse' can be TRUE, FALSE, or NA. If FALSE, the block is returned
### as an ordinary array. If TRUE, it's returned as a SparseArray object.
### Using 'as.sparse=NA' (the default) is equivalent to
### using 'as.sparse=is_sparse(x)'. This is the most efficient way to read
### a block.
### Propagates the dimnames.
read_block <- function(x, viewport, as.sparse=NA)
{
    x_dim <- dim(x)
    if (is.null(x_dim))
        stop(wmsg("the first argument to read_block() must be an ",
                  "array-like object (i.e. it must have dimensions)"))
    stopifnot(is(viewport, "ArrayViewport"),
              identical(refdim(viewport), x_dim),
              is.logical(as.sparse),
              length(as.sparse) == 1L)

    ans <- .read_block(x, viewport, as.sparse=as.sparse)

    ## Individual read_block_as_dense() and read_block_as_sparse() methods
    ## are not expected to propagate the dimnames so we take care of this
    ## now.
    Nindex <- makeNindexFromArrayViewport(viewport)
    ans_dimnames <- subset_dimnames_by_Nindex(dimnames(x), Nindex)
    set_dimnames(ans, ans_dimnames)
}

