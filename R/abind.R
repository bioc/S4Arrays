### =========================================================================
### abind() generic and methods
### -------------------------------------------------------------------------


### A NOTE ABOUT CRAN PACKAGE abind 1.4-5: By default, abind::abind() does
### NOT follow the same rules as rbind() and cbind() for propagation of the
### dimnames. This is despite its man page (?abind::abind) claiming that it
### does (see documentation of the 'use.first.dimnames' argument), when in
### fact it has it backward! Very misleading!
### The abind0() function defined below is a simple wrapper around
### abind::abind() that uses 'use.first.dimnames=TRUE' by default to
### correct mishandling of the dimnames.
abind0 <- function(..., use.first.dimnames=TRUE)
{
    abind::abind(..., use.first.dimnames=use.first.dimnames)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .extract_objects_to_abind()
### .extract_abind_crazy_args()
###

### All argument names of abind::abind(), ignoring the ellipsis.
.ORGINAL_ABIND_ARGNAMES <- setdiff(names(formals(abind::abind)), "...")

### Return the supplied objects in an ordinary list. Any argument that is
### not a "recognized" argument of the original abind::abind() is considered
### an input object to the binding operation.
.extract_objects_to_abind <- function(...)
{
    objects <- list(...)
    objects[.ORGINAL_ABIND_ARGNAMES] <- NULL
    unname(S4Vectors:::delete_NULLs(objects))
}

### Return the list of supplied arguments that belong to the original
### abind::abind() interface.
.extract_abind_crazy_args <- function(...)
{
    dots <- list(...)
    dots[intersect(names(dots), .ORGINAL_ABIND_ARGNAMES)]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .default_abind()
###

.default_abind <- function(..., along=NULL, rev.along=NULL)
{
    objects <- .extract_objects_to_abind(...)
    if (length(objects) == 0L)
        return(NULL)

    crazy_args <- .extract_abind_crazy_args(...)
    all_extra_args <- c(list(along=along, rev.along=rev.along), crazy_args)
    ## Identify objects that are:
    ##   1. a list;
    ##   2. an array-like object;
    ##   3. an ordinary array or matrix.
    ## These are not exclusive. For example an object can be both a list
    ## **and** an ordinary array e.g. 'array(vector(mode="list", 12), 4:3)'.
    ok1 <- vapply(objects, is.list, logical(1))
    ok2 <- vapply(objects, function(object) !is.null(dim(object)), logical(1))
    ok3 <- vapply(objects, is.array, logical(1))
    if (any(ok1 & !ok2)) {
        ## The caller passed the objects to bind in a list.
        if (length(objects) != 1L)
            stop(wmsg("when passing the objects to bind in a list, ",
                      "all of them must be supplied via the list"))
        x <- objects[[1L]]  # 'x' is a list
        ## Call the abind() generic.
        return(do.call(S4Arrays::abind, c(x, all_extra_args)))
    }
    if (all(ok3)) {
        ## All the objects to bind are ordinary arrays or matrices.
        if (length(crazy_args) == 0L) {
            ## Call simple_abind2(), which is significantly faster than
            ## abind::abind().
            return(simple_abind2(objects, along=along, rev.along=rev.along))
        }
        ## Call abind0(), a thin wrapper to abind::abind().
        all_extra_args <- S4Vectors:::delete_NULLs(all_extra_args)
        return(do.call(abind0, c(objects, all_extra_args)))
    }

    ## From now own, at least one object in 'objects' is guaranteed to NOT
    ## be an ordinary array or matrix.

    if (length(crazy_args) != 0L) {
        argnames <- paste0("'", names(crazy_args), "'", collapse=",")
        stop(wmsg("unsupported argument(s) when some of the objects to ",
                  "bind are not ordinary arrays or matrices: ", argnames))
    }

    objects <- homogenize_abind_input_objects(objects,
                                              along=along, rev.along=rev.along,
                                              coerce.to.target=TRUE)

    ## Note that unary abind() is not necessarily a no-op because
    ## homogenize_abind_input_objects() can add dimensions to the input object.
    if (length(objects) == 1L)
        return(objects[[1L]])

    ## Call the abind() generic.
    ## Since all objects went thru homogenization they're now guaranteed to
    ## inherit from a common class for which an abind() method is defined.
    ## If that's not the case, then calling S4Arrays::abind() will dispatch
    ## on the default method again which will be the start of an infinite
    ## recursion loop!
    do.call(S4Arrays::abind, c(objects, list(along=attr(objects, "along"))))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The abind() generic and methods
###

### Like in the original abind::abind(), the default for 'along' is the last
### dimension. However, here in the generic function, the default value is
### NULL instead of 'N', but it means the same thing.
setGeneric("abind", signature="...",
    function(..., along=NULL, rev.along=NULL) standardGeneric("abind")
)

setMethod("abind", "ANY", .default_abind)

.abind_Matrix_objects <- function(..., along=NULL, rev.along=NULL)
{
    along <- get_abind_along(2L, along=along, rev.along=rev.along)
    if (along == 1L)
        return(do.call(rbind, list(...)))
    if (along == 2L)
        return(do.call(cbind, list(...)))
    stop(wmsg("the abind() method for Matrix derivatives only ",
              "supports setting 'along' (or 'rev.along') to 1 or 2"))
}

setMethod("abind", "Matrix", .abind_Matrix_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Bind arrays along their 1st or 2nd dimension
###
### TODO: No need for these functions to be generics. They should just be
### simple wrappers to 'abind(..., along=1L)' and 'abind(..., along=2L)',
### respectively.

setGeneric("arbind", function(...) standardGeneric("arbind"))
setGeneric("acbind", function(...) standardGeneric("acbind"))

setMethod("arbind", "ANY", function(...) abind(..., along=1L))
setMethod("acbind", "ANY", function(...) abind(..., along=2L))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### rbind(), cbind()
###

### S3/S4 combo for rbind.Array
rbind.Array <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the rbind() method for Array objects ",
                     "ignores the 'deparse.level' argument"))
    arbind(...)
}
setMethod("rbind", "Array", rbind.Array)

### S3/S4 combo for cbind.Array
cbind.Array <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the cbind() method for Array objects ",
                     "ignores the 'deparse.level' argument"))
    acbind(...)
}
setMethod("cbind", "Array", cbind.Array)

### Arguments 'use.names', 'ignore.mcols', and 'check' are ignored.
setMethod("bindROWS", "Array",
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
    {
        args <- c(list(x), unname(objects))
        do.call(rbind, args)
    }
)

