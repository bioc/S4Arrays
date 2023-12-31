### =========================================================================
### Manipulation of array selections
### -------------------------------------------------------------------------
###


### Like base::arrayInd() but faster and accepts a matrix for 'dim' (with 1
### row per element in 'Lindex').
Lindex2Mindex <- function(Lindex, dim, use.names=FALSE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ## 'dim' can be a matrix so it's important to use storage.mode()
    ## instead of as.integer().
    if (storage.mode(dim) == "double")
        storage.mode(dim) <- "integer"
    ## 'Lindex' and 'dim' will be fully checked at the C level.
    .Call2("C_Lindex2Mindex", Lindex, dim, use.names, PACKAGE="S4Arrays")
}

Mindex2Lindex <- function(Mindex, dim, use.names=FALSE, as.integer=FALSE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    if (!isTRUEorFALSE(as.integer))
        stop("'as.integer' must be TRUE or FALSE")
    ## 'dim' and/or 'Mindex' can be matrices so it's important to use
    ## storage.mode() instead of as.integer(). Also, unlike as.integer(),
    ## this preserves the names/dimnames.
    if (storage.mode(dim) == "double")
        storage.mode(dim) <- "integer"
    if (storage.mode(Mindex) == "double")
        storage.mode(Mindex) <- "integer"
    ## 'Mindex' and 'dim' will be fully checked at the C level.
    .Call2("C_Mindex2Lindex", Mindex, dim, use.names,
                              as.integer, PACKAGE="S4Arrays")
}

