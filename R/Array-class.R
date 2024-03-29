### =========================================================================
### Array objects
### -------------------------------------------------------------------------


### A virtual class with no slots to be extended by concrete subclasses with
### an array-like semantic.
setClass("Array", representation("VIRTUAL"))

### Note that some objects with dimensions (i.e. with a non-NULL dim()) can
### have a length() that is not 'prod(dim(x))' e.g. data-frame-like objects
### (for which 'length(x)' is 'ncol(x)') and SummarizedExperiment derivatives
### (for which 'length(x)' is 'nrow(x)').
### Terminology: Should we still consider that these objects are "array-like"
### or "matrix-like"? Or should these terms be used only for objects that
### have dimensions **and** have a length() defined as 'prod(dim(x))'?

### Even though prod() always returns a double, it seems that the length()
### primitive function takes care of turning this double into an integer if
### it's <= .Machine$integer.max
setMethod("length", "Array", function(x) prod(dim(x)))

setMethod("isEmpty", "Array", function(x) any(dim(x) == 0L))

### 'subscripts' is assumed to be an integer vector parallel to 'dim(x)' and
### with no out-of-bounds subscripts (i.e. 'all(subscripts >= 1)' and
### 'all(subscripts <= dim(x))').
### NOT exported for now but should probably be at some point (like
### S4Vectors::getListElement() is).
setGeneric("getArrayElement", signature="x",
    function(x, subscripts) standardGeneric("getArrayElement")
)

### Support multidimensional and linear subsetting.
### TODO: Multidimensional subsetting should support things like
###       x[[5, 15, 2]] and x[["E", 15, "b"]].
### TODO: Linear subsetting should support a single *numeric* subscript.
setMethod("[[", "Array",
    function(x, i, j, ...)
    {
        if (missing(x))
            stop("'x' is missing")
        Nindex <- extract_Nindex_from_syscall(sys.call(), parent.frame())
        nsubscript <- length(Nindex)
        x_dim <- dim(x)
        x_ndim <- length(x_dim)
        if (!(nsubscript == x_ndim || nsubscript == 1L))
            stop("incorrect number of subscripts")
        ok <- vapply(Nindex, isSingleInteger, logical(1), USE.NAMES=FALSE)
        if (!all(ok))
            stop(wmsg("each subscript must be a single integer ",
                      "when subsetting an ", class(x), " object with [["))
        if (nsubscript == x_ndim) {
            ## Multidimensional subsetting.
            subscripts <- unlist(Nindex, use.names=FALSE)
            if (!(all(subscripts >= 1L) && all(subscripts <= x_dim)))
                stop("some subscripts are out of bounds")
        } else {
            ## Linear subsetting.
            ## We turn this into a multidimensional subsetting by
            ## transforming the user-supplied linear index into an array
            ## (i.e. multidimensional) index.
            i <- Nindex[[1L]]
            if (i < 1L || i > prod(x_dim))
                stop("subscript is out of bounds")
            subscripts <- as.integer(arrayInd(i, x_dim))
        }
        getArrayElement(x, subscripts)
    }
)

.SLICING_TIP <- c(
    "Consider reducing its number of effective dimensions by slicing it ",
    "first (e.g. x[8, 30, , 2, ]). Make sure that all the indices used for ",
    "the slicing have length 1 except at most 2 of them which can be of ",
    "arbitrary length or missing."
)

.from_Array_to_matrix <- function(x)
{
    if (!isS4(x)) {
        ## The arrow package does not define any as.matrix method for
        ## arrow::Array objects (or their ancestors) at the moment, so this is
        ## a preventive hack only. See as.vector.Array in the extract_array.R
        ## file for the details.
        x_class <- class(x)
        if (length(x_class) >= 2L) {
            ## Call "next" S3 as.matrix method.
            class(x) <- tail(x_class, n=-1L)
            on.exit(class(x) <- x_class)
            return(base::as.matrix(x))
        }
    }
    x_dim <- dim(x)
    if (sum(x_dim != 1L) > 2L)
        stop(wmsg(class(x), " object has more than 2 effective dimensions ",
                  "--> cannot coerce it to a matrix. ", .SLICING_TIP))
    ans <- drop(as.array(x))  # this could drop all the dimensions!
    if (length(x_dim) == 2L) {
        ans <- set_dim(ans, x_dim)
        ans <- set_dimnames(ans, dimnames(x))
    } else {
        as.matrix(ans)
    }
    ans
}

### S3/S4 combo for as.matrix.Array
as.matrix.Array <- function(x, ...) .from_Array_to_matrix(x, ...)
setMethod("as.matrix", "Array", .from_Array_to_matrix)

### S3/S4 combo for t.Array
### t() will work out-of-the-box on any Array derivative that supports aperm().
t.Array <- function(x)
{
    if (!isS4(x)) {
        ## The arrow package does not define any t method for
        ## arrow::Array objects (or their ancestors) at the moment, so this is
        ## a preventive hack only. See as.vector.Array in the extract_array.R
        ## file for the details.
        x_class <- class(x)
        if (length(x_class) >= 2L) {
            ## Call "next" S3 t method.
            class(x) <- tail(x_class, n=-1L)
            on.exit(class(x) <- x_class)
            return(base::t(x))
        }
    }
    if (length(dim(x)) != 2L)
        stop(wmsg("the ", class(x), " object to transpose ",
                  "must have exactly 2 dimensions"))
    aperm(x)
}
setMethod("t", "Array", t.Array)

