### =========================================================================
### Multidimensional array recycling a.k.a. "tile recycling"
### -------------------------------------------------------------------------
###
### Typically used in the context of 'Ops' operations on arrays or array-like
### objects.
###
### Motivating use case for this is to replace cumbersome, inefficient, and
### obfuscated code like:
###
###     cs <- colSums(x)
###     t(t(x) / cs)  # double transposition can be very expensive!
###
### with:
###
###     cs <- colSums(x)
###     x / as_tile(cs, along=2)
###
### Sure, more typing, but the intention of the code is clearer and it's also
### more efficient (it avoids the double transposition). E.g. it's 3x faster
### on a 8000 x 20,000 matrix generated with 'matrix(runif(16e7), ncol=2e4)'.
### The speedup is even more drastic (e.g. 10x or more) when 'x' is an
### SVT_SparseMatrix object.
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### as_tile()
###

setClass("tile", contains="array")

.vector_as_tile <- function(x, along=1L)
{
    stopifnot(is.vector(x))
    if (!isSingleNumber(along))
        stop(wmsg("'along' must be a single integer"))
    if (!is.integer(along))
        along <- as.integer(along)
    if (along <= 0L)
        stop(wmsg("'along' must be a positive integer"))
    new("tile", array(x, dim=c(rep.int(1L, along-1L), length(x))))
}

### Only returns an actual tile object when 'x' is an ordinary vector or
### array. Otherwise, when 'x' is an array-like object that is **not** an
### ordinary array, as_tile() acts as an endomorphism i.e. it preserves the
### class of 'x' (note that this endomorphism is a no-op if 'dim' is not
### specified). This is the case for example when 'x' is a dgCMatrix,
### SparseArray, or DelayedArray object, etc.. or a tile object itself.
as_tile <- function(x, along=1L, dim=NULL)
{
    if (is.vector(x)) {
        if (is.null(dim))
            return(.vector_as_tile(x, along=along))
        if (!identical(along, 1L))
            stop(wmsg("only one of 'along' or 'dim' can be specified"))
        dim(x) <- dim
    } else {
        if (is.null(dim(x)))
            stop(wmsg("'x' must be an array-like object or a vector"))
        if (!identical(along, 1L))
            stop(wmsg("'along' can only be used on a vector"))
        if (!is.null(dim))
            x <- set_dim(x, dim)  # reshape the array (not all array-like
                                  # objects will support this)
        if (!is.array(x))
            return(x)  # endomorphism (and no-op if 'dim' is NULL)
    }
    new("tile", x)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### recycle_tile()
###

### 'tile_dim' and 'x_dim' must be "parallel" integer vectors (i.e. vectors
### of same length). Returns another parallel integer vector.
.compute_recycling_extent <- function(tile_dim, x_dim)
{
    stopifnot(is.integer(tile_dim), is.integer(x_dim))
    if (length(tile_dim) != length(x_dim))
        stop(wmsg("non-conformable arrays"))
    ok <- all((x_dim %% tile_dim == 0L) | x_dim == 0L)
    if (is.na(ok))
        stop(wmsg("no tile dimension can be equal to 0 unless the  ",
                  "corresponding array dimension is also equal to 0"))
    if (!ok)
        stop(wmsg("each array dimension must be a multiple ",
                  "of the corresponding tile dimension"))
    x_dim %/% tile_dim
}

### NOT exported for now.
### Should **always** return an array-like object with dimensions 'x_dim'.
### Preserves the class of 'tile' (endomorphism) except when it's a tile
### object, in which case the class attribute is dropped and an ordinary
### array or matrix is returned.
recycle_tile <- function(tile, x_dim)
{
    if (is.vector(tile))
        tile <- as.array(tile)  # 1D-array
    tile_dim <- dim(tile)
    if (is.null(tile_dim))
        stop(wmsg("'tile' must be an array-like object or a vector"))
    ndim <- length(x_dim)
    if (length(tile_dim) < ndim) {
        tile_dim <- rpad_dim(tile_dim, ndim)
        tile <- set_dim(tile, tile_dim)
    }
    extent <- .compute_recycling_extent(tile_dim, x_dim)
    arep_times(tile, extent)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Ops() methods for tile objects
###

### The returned array is guaranteed to have the dimensions of the left array.
setMethod("Ops", c("array", "tile"),
    function(e1, e2)
    {
        e2 <- recycle_tile(e2, dim(e1))
        callNextMethod()
    }
)

### The returned array is guaranteed to have the dimensions of the right array.
setMethod("Ops", c("tile", "array"),
    function(e1, e2)
    {
        e1 <- recycle_tile(e1, dim(e2))
        callNextMethod()
    }
)

### Returns one of "L", "R", "LR", or an error.
.which_side_to_recycle <- function(Ldim, Rdim)
{
    if (length(Ldim) != length(Rdim))
        stop(wmsg("non-conformable arrays"))
    errmsg <- "neither array can be used as a tile to lay on the other array"
    L_is_zero <- Ldim == 0L
    R_is_zero <- Rdim == 0L
    cannot_be_left  <- any(L_is_zero > R_is_zero)
    cannot_be_right <- any(L_is_zero < R_is_zero)
    if (cannot_be_left && cannot_be_right)
        stop(wmsg(errmsg))
    could_be_left  <- all(Rdim %% Ldim == 0L, na.rm=TRUE)
    could_be_right <- all(Ldim %% Rdim == 0L, na.rm=TRUE)
    if (cannot_be_left) {
        if (could_be_right)
            return("R")
        stop(wmsg(errmsg))
    }
    if (cannot_be_right) {
        if (could_be_left)
            return("L")
        stop(wmsg(errmsg))
    }
    if (could_be_left && could_be_right)
        return("LR")  # means the 2 arrays are conformable (i.e. same dims)
    if (could_be_left)
        return("L")
    if (could_be_right)
        return("R")
    stop(wmsg(errmsg))
}

setMethod("Ops", c("tile", "tile"),
    function(e1, e2)
    {
        dim1 <- dim(e1)
        dim2 <- dim(e2)
        ndim1 <- length(dim1)
        ndim2 <- length(dim2)
        if (ndim1 < ndim2) {
            dim1 <- rpad_dim(dim1, ndim2)
            e1 <- set_dim(e1, dim1)
        } else if (ndim1 > ndim2) {
            dim2 <- rpad_dim(dim2, ndim1)
            e2 <- set_dim(e2, dim2)
        }
        side <- .which_side_to_recycle(dim1, dim2)
        if (side == "L") {
            e1 <- recycle_tile(e1, dim2)
        } else if (side == "R") {
            e2 <- recycle_tile(e2, dim1)
        }
        callGeneric()
    }
)

