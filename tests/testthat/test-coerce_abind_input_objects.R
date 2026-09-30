
test_that(".extract_strongest_nodes()", {
    extract_strongest_nodes <-
        S4Arrays:::.extract_strongest_nodes

    nodes <- "array"
    expect_true(identical(
        extract_strongest_nodes(nodes), "array"))

    nodes <- c("array", "Matrix")
    expect_true(identical(
        extract_strongest_nodes(nodes), "Matrix"))

    nodes <- c("Matrix", "array", "sparseMatrix")
    expect_true(identical(
        extract_strongest_nodes(nodes), "sparseMatrix"))

    nodes <- c("array", "NaArray")
    expect_true(identical(
        extract_strongest_nodes(nodes), "NaArray"))

    nodes <- c("sparseMatrix", "array", "NaArray")
    expect_true(setequal(
        extract_strongest_nodes(nodes), c("sparseMatrix", "NaArray")))

    nodes <- c("sparseMatrix", "array", "SparseArray", "NaArray", "Matrix")
    expect_true(setequal(
        extract_strongest_nodes(nodes), c("SparseArray", "NaArray")))

    nodes <- c("array", "DelayedArray")
    expect_true(identical(
        extract_strongest_nodes(nodes), "DelayedArray"))

    nodes <- c("DelayedArray", "Matrix")
    expect_true(identical(
        extract_strongest_nodes(nodes), "DelayedArray"))

    nodes <- "NaArray"
    expect_true(identical(
        extract_strongest_nodes(nodes), "NaArray"))

    nodes <- c("array", "SparseArray", "Matrix")
    expect_true(identical(
        extract_strongest_nodes(nodes), "SparseArray"))

    nodes <- c("NaArray", "Matrix")
    expect_true(identical(
        extract_strongest_nodes(nodes), "NaArray"))

    nodes <- c("NaArray", "Matrix", "SparseArray", "array")
    expect_true(setequal(
        extract_strongest_nodes(nodes), c("SparseArray", "NaArray")))

    nodes <- c("NaArray", "DelayedArray", "SparseArray")
    expect_true(identical(
        extract_strongest_nodes(nodes), "DelayedArray"))
})

### Each list element is itself a list that contains the objects to bind.
### The names on the list indicate the expected "target class" for each
### combination of objects.
library(SparseArray)
library(DelayedArray)
.COERCE_ABIND_INPUT_OBJECTS <- list(
    array       =list(matrix()),
    Matrix      =list(new("ngeMatrix")),
    Matrix      =list(new("dgeMatrix"),
                      array(),
                      new("lgeMatrix")),
    sparseMatrix=list(new("ngRMatrix")),
    sparseMatrix=list(array(),
                      new("dgCMatrix")),
    sparseMatrix=list(new("dgeMatrix"),
                      array(),
                      new("dgCMatrix")),
    SparseArray =list(new("SVT_SparseMatrix"),
                      matrix()),
    SparseArray =list(new("SVT_SparseMatrix"),
                      matrix(),
                      new("ngCMatrix")),
    SparseArray =list(new("dgeMatrix"),
                      new("SVT_SparseArray"),
                      matrix(),
                      new("ngCMatrix")),
    DelayedArray=list(array(),
                      new("DelayedMatrix")),
    DelayedArray=list(new("DelayedMatrix"),
                      new("dgCMatrix")),
    DelayedArray=list(new("lgeMatrix"),
                      new("DelayedMatrix"),
                      new("dgCMatrix")),
    DelayedArray=list(new("DelayedArray"),
                      new("NaMatrix")),
    DelayedArray=list(new("SVT_SparseArray"),
                      array(),
                      new("DelayedMatrix"),
                      matrix(),
                      new("dgeMatrix"),
                      new("SVT_SparseMatrix"),
                      new("dgCMatrix"),
                      new("NaMatrix"))
)

test_that(".compute_abind_target_class()", {
    compute_abind_target_class <- S4Arrays:::.compute_abind_target_class

    for (i in seq_along(.COERCE_ABIND_INPUT_OBJECTS)) {
        objects <- .COERCE_ABIND_INPUT_OBJECTS[[i]]
        expected_target_class <- names(.COERCE_ABIND_INPUT_OBJECTS)[[i]]
        expect_true(compute_abind_target_class(objects) ==
                    expected_target_class)
    }

    expected_words <- c("does", "not", "support")
    regexp <- paste0("\\b", expected_words, "\\b", collapse=".*")
    objects <- list(new("SVT_SparseMatrix"), matrix(), IRanges())
    expect_error(compute_abind_target_class(objects), regexp, ignore.case=TRUE)

    expected_words <- c("doesn't", "know", "how", "to", "bind")
    regexp <- paste0("\\b", expected_words, "\\b", collapse=".*")
    objects <- list(new("NaMatrix"), new("dgCMatrix"))
    expect_error(compute_abind_target_class(objects), regexp, ignore.case=TRUE)
    objects <- list(new("SVT_SparseArray"), new("NaMatrix"))
    expect_error(compute_abind_target_class(objects), regexp, ignore.case=TRUE)
})

test_that("coerce_abind_input_objects()", {
    coerce_abind_input_objects <- S4Arrays:::coerce_abind_input_objects

    for (i in seq_along(.COERCE_ABIND_INPUT_OBJECTS)) {
        objects <- .COERCE_ABIND_INPUT_OBJECTS[[i]]
        expected_target_class <- names(.COERCE_ABIND_INPUT_OBJECTS)[[i]]
        current <- coerce_abind_input_objects(objects)
        expect_true(is.list(current))
        expect_true(length(current) == length(objects))
        target_class <- attr(current, "target_class")
        expect_true(target_class == expected_target_class)
        expect_true(all(sapply(current, is, target_class)))
    }
})

