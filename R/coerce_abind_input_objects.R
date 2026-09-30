### =========================================================================
### coerce_abind_input_objects()
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###


### DAG of coercing power:
###
### Class A "has more coercing power" than class B in the context of abind()
### means that abind(A, B) will choose to coerce B to A and return A.
### Note that we cannot simply order array-like classes by coercing power
### (e.g. from lowest to highest) because the "has more coercing power than"
### relationship doesn't define a total order (e.g. SparseArray and NaArray).
### In other words, we cannot use a linear graph to represent the relationship
### so we use a DAG (Directed Acyclic Graph) instead.
### From lowest coercing power at the top (array) to highest coercing power
### at the bottom (DelayedArray).
###
###                array
###                  ^
###                  |
###                Matrix          (from Matrix package)
###                ^    ^
###                |    |
###      sparseMatrix   |          (from Matrix package)
###                ^    |
###                |    |
###       SparseArray  NaArray     (from SparseArray package)
###                ^    ^
###                |    |
###             DelayedArray       (from DelayedArray package)
###
### NOTES:
### - This is NOT an inheritance graph! For example DelayedArray does not
###   extend SparseArray but it has more coercing power.
### - Except for array, each node in the graph should be "covered" by an
###   abind() method other than the default method, that is, by an abind()
###   method defined for the node itself or for a super class of the node.
### - Add new nodes as new abind() methods get defined.

### '.ABIND_COERCING_POWER_GRAPH' is a 2-column character matrix where each
### row represents an edge in the DAG above. Add new edges as needed.
.ABIND_COERCING_POWER_GRAPH <- rbind(
    c("Matrix", "array"),
    c("sparseMatrix", "Matrix"),
    c("SparseArray", "sparseMatrix"),
    c("NaArray", "Matrix"),
    c("DelayedArray", "SparseArray"),
    c("DelayedArray", "NaArray")
)

.build_DAG_transitive_matrix <- function(DAG)
{
    stopifnot(is.matrix(DAG), ncol(DAG) == 2L, is.character(DAG))
    all_nodes <- unique(as.vector(DAG))
    adj_mat <- matrix(FALSE, nrow=length(all_nodes), ncol=length(all_nodes),
                             dimnames=list(all_nodes, all_nodes))
    adj_mat[DAG] <- TRUE  # adjacency matrix
    prev_mat <- mat <- adj_mat
    while (TRUE) {
        mat <- (prev_mat %*% prev_mat + prev_mat) != 0L
        if (identical(mat, prev_mat))
            break
        prev_mat <- mat
    }
    if (any(mat & t(mat)))
        stop(wmsg("graph is not acyclic"))
    mat
}

### '.ABIND_TRANSITIVE_COERCION_MATRIX' is a square logical matrix with
### dimnames that can be queried to know whether a class has **strictly**
### more coercing power than another class. The rownames and colnames are
### the nodes (i.e. class names) in the "DAG of coercing power" above.
### Given 2 class names, 'Class1' and 'Class2', querying the matrix with:
###
###   .ABIND_TRANSITIVE_COERCION_MATRIX[Class1, Class2]
###
### will return TRUE if 'Class1' has strictly more coercing power than
### 'Class2', and FALSE otherwise.
### Examples:
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["DelayedArray", "Matrix"]
###   [1] TRUE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["Matrix", "DelayedArray"]
###   [1] FALSE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["SparseArray", "SparseArray"]
###   [1] FALSE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["SparseArray", "Matrix"]
###   [1] TRUE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["NaArray", "Matrix"]
###   [1] TRUE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["Matrix", "NaArray"]
###   [1] FALSE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["SparseArray", "NaArray"]
###   [1] FALSE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["NaArray", "SparseArray"]
###   [1] FALSE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["NaArray", "array"]
###   [1] TRUE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["array", "NaArray"]
###   [1] FALSE
###   > .ABIND_TRANSITIVE_COERCION_MATRIX["array", "array"]
###   [1] FALSE
.ABIND_TRANSITIVE_COERCION_MATRIX <-
    .build_DAG_transitive_matrix(.ABIND_COERCING_POWER_GRAPH)

### Has its own unit tests!
### The supplied nodes must be valid nodes in the "DAG of coercing power"
### defined above.
### Returns the node(s) with most coercing power.
### IMPORTANT NOTE: Since the "has more coercing power" relationship defines
### a **partial** order only, .extract_strongest_nodes() can return more than
### one node. Said otherwise, 'nodes' can contain more than one maximum for
### the underlying partial order. For example:
###   > nodes <- c("sparseMatrix", "array", "NaArray", "Matrix")
###   > .extract_strongest_nodes(nodes)
###   [1] "sparseMatrix" "NaArray"
###   > nodes <- c("NaArray", "Matrix", "SparseArray")
###   > .extract_strongest_nodes(nodes)
###   [1] "NaArray"     "SparseArray"
### But:
###   > nodes <- c("DelayedArray", "NaArray", "Matrix", "SparseArray")
###   > .extract_strongest_nodes(nodes)
###   [1] "DelayedArray"
.extract_strongest_nodes <- function(nodes)
{
    stopifnot(is.character(nodes), length(nodes) != 0L,
              all(nodes %in% rownames(.ABIND_TRANSITIVE_COERCION_MATRIX)))
    nodes <- unique(nodes)
    m <- .ABIND_TRANSITIVE_COERCION_MATRIX[nodes, nodes, drop=FALSE]
    colnames(m)[!apply(m, 2L, any)]
}

### Has its own unit tests!
### The "target class" is the strongest node in the "DAG of coercing power"
### defined above with at least one derivative in 'objects'.
.compute_abind_target_class <- function(objects)
{
    stopifnot(is.list(objects), length(objects) != 0L)
    all_nodes <- unique(as.vector(.ABIND_COERCING_POWER_GRAPH))
    object2node <- vapply(objects,
        function(object) {
            idx <- which(vapply(all_nodes,
                                function(node) is(object, node), logical(1)))
            errmsg <- c("failed to map ", class(objects)[[1L]], " object ",
                        "to a unique node in DAG of coercing power")
            if (length(idx) > 2L)
                stop(wmsg(errmsg))
            if (length(idx) == 2L) {
                node1 <- all_nodes[[idx[[1L]]]]
                node2 <- all_nodes[[idx[[2L]]]]
                if (extends(node1, node2)) {
                    idx <- idx[[1L]]
                } else if (extends(node2, node1)) {
                    idx <- idx[[2L]]
                } else {
                    stop(wmsg(errmsg))
                }
            }
            if (length(idx) == 1L)
                return(all_nodes[[idx]])
            if (is.array(object))
                return("array")
            NA_character_
        }, character(1))
    na_idx <- which(is.na(object2node))
    if (length(na_idx) != 0L) {
        unsupported_classes <- vapply(objects[na_idx],
            function(object) class(object)[[1]], character(1))
        in1string <- paste(unique(unsupported_classes), collapse=", ")
        stop(wmsg("abind() does not support objects of the ",
                  "following classe(s): ", in1string))
    }
    ans <- .extract_strongest_nodes(object2node)
    if (length(ans) != 1L) {
        in1string <- paste(ans, collapse=", ")
        stop(wmsg("abind() doesn't know how to bind objects that are of (or ",
                  "derive from) the following classes together: ", in1string))
    }
    ans
}

### Has its own unit tests!
### If 'target_class' is not supplied, then the "target class" returned
### by .compute_abind_target_class() will be used, which is guaranteed to be
### a node in the "DAG of coercing power" (see .compute_abind_target_class()
### above for the details). Assuming that an abind() method should be defined
### for the target class (or for a super class of the target class), calling
### the abind() generic on the returned objects should be able to dispatch
### on the method defined for the target class.
coerce_abind_input_objects <- function(objects, target_class=NULL)
{
    if (is.null(target_class)) {
        ## 'target_class' is guaranteed to be a node in the "DAG of
        ## coercing power" defined above in this file.
        target_class <- .compute_abind_target_class(objects)
    } else {
        stopifnot(isSingleString(target_class))
    }

    ## Coerce all objects to 'target_class'.
    objects <- lapply(objects,
        function(object) {
            if (is(object, target_class))
                return(object)
            class0 <- class(object)[[1L]]
            object <- try(as(object, target_class), silent=TRUE)
            if (inherits(object, "try-error"))
                stop(wmsg("failed to coerce ", class0, " ",
                          "object to ", target_class))
            object
        })
    attr(objects, "target_class") <- target_class
    objects
}

