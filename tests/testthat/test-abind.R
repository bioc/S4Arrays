.TEST_matrices <- list(
    matrix(1:15, nrow=3, ncol=5,
           dimnames=list(NULL, paste0("M1y", 1:5))),
    matrix(101:135, nrow=7, ncol=5,
           dimnames=list(paste0("M2x", 1:7), paste0("M2y", 1:5))),
    matrix(1001:1025, nrow=5, ncol=5,
           dimnames=list(paste0("M3x", 1:5), NULL))
)

.TEST_arrays <- list(
    array(1:60, c(3, 5, 4),
           dimnames=list(NULL, paste0("M1y", 1:5), NULL)),
    array(101:240, c(7, 5, 4),
           dimnames=list(paste0("M2x", 1:7), paste0("M2y", 1:5), NULL)),
    array(10001:10100, c(5, 5, 4),
           dimnames=list(paste0("M3x", 1:5), NULL, paste0("M3z", 1:4)))
)

test_that("default abind() method", {
    default_abind <- S4Arrays:::.default_abind
    abind0 <- S4Arrays:::abind0

    m1 <- .TEST_matrices[[1L]]
    m2 <- .TEST_matrices[[2L]][1:3, ]
    m3 <- .TEST_matrices[[3L]][1:3, ]

    a1 <- .TEST_arrays[[1L]]
    a2 <- .TEST_arrays[[2L]]
    a3 <- .TEST_arrays[[3L]]

    ## The default abind() method (S4Arrays:::.default_abind()) implements
    ## its own little dispatch mechanism. See S4Arrays/R/abind.R for the
    ## details.

    ## --- 1. Pass the objects to bind in a list ---

    expected <- abind0(.TEST_arrays, along=1)
    expect_identical(default_abind(.TEST_arrays, along=1), expected)
    expect_identical(default_abind(.TEST_arrays, rev.along=3), expected)

    expected <- abind0(.TEST_arrays, along=1, use.first.dimnames=TRUE)
    current <- default_abind(.TEST_arrays, along=1, use.first.dimnames=TRUE)
    expect_identical(current, expected)

    expected <- abind0(.TEST_arrays, along=1, use.first.dimnames=FALSE)
    current <- default_abind(.TEST_arrays, along=1, use.first.dimnames=FALSE)
    expect_identical(current, expected)

    expected_words <- c("passing", "objects", "to", "bind", "in", "list")
    regexp <- paste0("\\b", expected_words, "\\b", collapse=".*")
    expect_error(default_abind(a1, .TEST_arrays), regexp, ignore.case=TRUE)
    expect_error(default_abind(.TEST_arrays, a1), regexp, ignore.case=TRUE)

    ## --- 2. Dispatch on S4Arrays:::simple_abind2() ---

    expected <- abind0(m1, m2, m3)
    expect_identical(default_abind(m1, m2, m3), expected)

    expected <- abind0(m1, m2, m3, rev.along=0)
    expect_identical(default_abind(m1, m2, m3, along=3), expected)
    expect_identical(default_abind(m1, m2, m3, rev.along=0), expected)

    expected <- abind0(a1, m3)
    expect_identical(default_abind(a1, m3), expected)

    expected <- abind0(m3, a1)
    expect_identical(default_abind(m3, a1), expected)

    a4 <- a1[ , , 4, drop=FALSE]
    a5 <- S4Arrays:::set_dim(m3, c(dim(m3), 1L))

    expected <- abind0(a4, a5, rev.along=0)
    expect_identical(default_abind(a4, m3, along=4), expected)
    expect_identical(default_abind(a4, m3, rev.along=0), expected)

    expected <- abind0(a5, a4, rev.along=0)
    expect_identical(default_abind(m3, a4, along=4), expected)
    expect_identical(default_abind(m3, a4, rev.along=0), expected)

    ## --- 3. Because we use crazy args, dispatch is on abind::abind() ---
    ## ---     wrapper abind0() instead of S4Arrays:::simple_abind2()  ---

    expected <- abind0(m1, m2, m3, use.first.dimnames=TRUE)
    current <- default_abind(m1, m2, m3, use.first.dimnames=TRUE)
    expect_identical(current, expected)

    expected <- abind0(m1, m2, m3, use.first.dimnames=FALSE)
    current <- default_abind(m1, m2, m3, use.first.dimnames=FALSE)
    expect_identical(current, expected)

    ## --- 4. At least one object in 'objects' is not an ordinary array ---
    ## ---                          or matrix                           ---

    ## Bind ordinary matrix and lgeMatrix/dgCMatrix objects.
    x <- m1
    y <- as(m2 %% 4L <= 1L, "lgeMatrix")
    z <- as(m3, "dgCMatrix")
    for (along in 1:2) {
        FUN <- if (along == 1L) rbind else cbind
        expect_identical(default_abind(x, along=along), FUN(x))
        expect_identical(default_abind(y, along=along), FUN(y))
        expect_identical(default_abind(z, along=along), FUN(z))
        expect_identical(default_abind(x, y, along=along), FUN(x, y))
        expect_identical(default_abind(y, x, along=along), FUN(y, x))
        expect_identical(default_abind(x, z, along=along), FUN(x, z))
        expect_identical(default_abind(z, x, along=along), FUN(z, x))
        FUN2 <- function(...) as(FUN(...), "dgCMatrix")
        expect_identical(default_abind(y, z, along=along), FUN2(y, z))
        expect_identical(default_abind(z, y, along=along), FUN2(z, y))
        expect_identical(default_abind(x, y, z, along=along), FUN2(x, y, z))
        expect_identical(default_abind(x, z, y, along=along), FUN2(x, z, y))
        expect_identical(default_abind(y, x, z, along=along), FUN2(y, x, z))
        expect_identical(default_abind(y, z, x, along=along), FUN2(y, z, x))
        expect_identical(default_abind(z, x, y, along=along), FUN2(z, x, y))
        expect_identical(default_abind(z, y, x, along=along), FUN2(z, y, x))
        expect_identical(
            default_abind(NULL, x, NULL, y, NULL, NULL, x, z, along=along),
            FUN2(NULL, x, NULL, y, NULL, NULL, x, z))
    }

    ## Add SparseArray objects to the mix.
    library(SparseArray)
    svt <- as(m3, "SVT_SparseArray")
    coo <- as(m3, "COO_SparseArray")
    for (along in 1:2) {
        current <- default_abind(svt, x, y, z, along=along)
        expect_true(is(current, "SparseArray"))
        current <- default_abind(coo, x, y, z, along=along)
        expect_true(is(current, "SparseArray"))
        current <- default_abind(x, coo, NULL, y, svt, z, along=along)
        expect_true(is(current, "SparseArray"))
    }

    ## --- 5. Some edge cases ---
    expect_identical(default_abind(a1), a1)
    expect_identical(default_abind(), NULL)
})

test_that("arbind() on matrices", {
    ## on non-empty matrices
    matrices <- .TEST_matrices
    expect_identical(do.call(arbind, matrices), do.call(rbind, matrices))

    ## on empty matrices
    m1 <- matrix(nrow=0, ncol=3, dimnames=list(NULL, letters[1:3]))
    m2 <- matrix(1:15, ncol=3, dimnames=list(NULL, LETTERS[1:3]))
    expect_identical(arbind(m1, m2), rbind(m1, m2))
    expect_identical(arbind(m2, m1), rbind(m2, m1))
    expect_identical(arbind(m1, m1), rbind(m1, m1))

    ## on matrices of type "list"
    m3 <- matrix(list(), nrow=0, ncol=5)
    m4 <- matrix(list(10:9, NULL, letters[1:3], TRUE, raw()), nrow=4, ncol=5)
    expect_identical(arbind(m3, m4), rbind(m3, m4))
    expect_identical(arbind(m4, m3), rbind(m4, m3))
    expect_identical(arbind(m3, m3), rbind(m3, m3))
    matrices <- c(matrices, list(m3, m4))
    expect_identical(do.call(arbind, matrices), do.call(rbind, matrices))
})

test_that("acbind() on matrices", {
    ## on non-empty matrices
    matrices <- lapply(.TEST_matrices, t)
    expect_identical(do.call(acbind, matrices), do.call(cbind, matrices))

    ## on empty matrices
    m1 <- matrix(nrow=3, ncol=0, dimnames=list(letters[1:3], NULL))
    m2 <- matrix(1:15, nrow=3, dimnames=list(LETTERS[1:3], NULL))
    expect_identical(acbind(m1, m2), cbind(m1, m2))
    expect_identical(acbind(m2, m1), cbind(m2, m1))
    expect_identical(acbind(m1, m1), cbind(m1, m1))

    ## on matrices of type "list"
    m3 <- matrix(list(), nrow=5, ncol=0)
    m4 <- matrix(list(10:9, NULL, letters[1:3], TRUE, raw()), nrow=5, ncol=4)
    expect_identical(acbind(m3, m4), cbind(m3, m4))
    expect_identical(acbind(m4, m3), cbind(m4, m3))
    expect_identical(acbind(m3, m3), cbind(m3, m3))
    matrices <- c(matrices, list(m3, m4))
    expect_identical(do.call(acbind, matrices), do.call(cbind, matrices))
    m5 <- matrix(list(), nrow=5, ncol=10)
    expect_identical(acbind(m3, m5), cbind(m3, m5))
    expect_identical(acbind(m5, m3), cbind(m5, m3))
    matrices <- c(matrices, list(m3, m4, m5))
    expect_identical(do.call(acbind, matrices), do.call(cbind, matrices))
})

test_that("arbind() on arrays", {
    ## on 3D arrays
    current <- do.call(arbind, .TEST_arrays)
    check_2D_slice <- function(k) {
        slices <- lapply(.TEST_arrays, `[`, , , k)
        expected_slice <- do.call(rbind, slices)
        expect_identical(current[ , , k], expected_slice)
    }
    for (k in seq_len(dim(current)[[3L]])) check_2D_slice(k)

    ## on 1D arrays
    a1 <- array(11:15, 5, dimnames=list(LETTERS[1:5]))
    expect_identical(arbind(a1), a1)                # unary op
    b1 <- array(letters[1:3])
    expected <- array(c(a1, b1), 8, dimnames=list(c(LETTERS[1:5], rep("", 3))))
    expect_identical(arbind(a1, b1), expected)      # binary op
    expected <- array(c(b1, a1), 8, dimnames=list(c(rep("", 3), LETTERS[1:5])))
    expect_identical(arbind(b1, a1), expected)      # binary op
    a1b1a1 <- arbind(a1, b1, a1)                    # ternary op
    expect_identical(arbind(a1, arbind(b1, a1)), a1b1a1)
    expect_identical(arbind(arbind(a1, b1), a1), a1b1a1)
})

test_that("acbind() on arrays", {
    ## transpose first 2 dimensions of arrays in .TEST_arrays
    arrays <- lapply(.TEST_arrays,
        function(a) {
            a_dimnames <- dimnames(a)
            dim(a)[1:2] <- dim(a)[2:1]
            a_dimnames[1:2] <- a_dimnames[2:1]
            dimnames(a) <- a_dimnames
            a
    })

    ## on 3D arrays
    current <- do.call(acbind, arrays)
    check_2D_slice <- function(k) {
        slices <- lapply(arrays, `[`, , , k)
        expected_slice <- do.call(cbind, slices)
        expect_identical(current[ , , k], expected_slice)
    }
    for (k in seq_len(dim(current)[[3L]])) check_2D_slice(k)

    ## on 1D arrays
    a1 <- array(11:15, 5, dimnames=list(LETTERS[1:5]))
    expected <- S4Arrays:::set_dim(a1, c(5, 1))
    expect_identical(acbind(a1), expected)          # unary op
    b1 <- array(letters[1:5])
    expected <- cbind(a1, b1, deparse.level=0)
    expect_identical(acbind(a1, b1), expected)      # binary op
    expected <- cbind(b1, a1, deparse.level=0)
    expect_identical(acbind(b1, a1), expected)      # binary op
    expected <- cbind(a1, b1, a1, deparse.level=0)
    expect_identical(acbind(a1, b1, a1), expected)  # ternary op
})

test_that("rbind()/cbind() on array-like objects", {
    m1 <- .TEST_matrices[[1L]]
    m2 <- .TEST_matrices[[2L]][1:3, ]
    m3 <- .TEST_matrices[[3L]][1:3, ]

    x <- m1
    y <- as(m2 %% 4L <= 1L, "lgeMatrix")
    z <- as(m3, "dgCMatrix")

    library(SparseArray)
    svt <- as(m3, "SVT_SparseMatrix")
    coo <- as(m3, "COO_SparseMatrix")

    library(DelayedArray)
    M <- DelayedArray(m1)

    FUN2 <- function(FUN, ...) do.call(FUN, lapply(list(...), as.matrix))
    for (FUN in list(rbind, cbind)) {
        current <- FUN(svt, x, y, z)
        expect_true(is(current, "SVT_SparseMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, svt, x, y, z))
        current <- FUN(coo, x, y, z)
        expect_true(is(current, "SVT_SparseMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, coo, x, y, z))
        current <- FUN(x, coo, NULL, y, svt, z)
        expect_true(is(current, "SVT_SparseMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, x, coo, y, svt, z))

        current <- FUN(svt, NULL, M)
        expect_true(is(current, "DelayedMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, svt, M))
        current <- FUN(M, z)
        expect_true(is(current, "DelayedMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, M, z))
        current <- FUN(z, M)
        expect_true(is(current, "DelayedMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, z, M))
        current <- FUN(x, M, y)
        expect_true(is(current, "DelayedMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, x, M, y))
        current <- FUN(M, x, coo, NULL, y, svt, NULL, z)
        expect_true(is(current, "DelayedMatrix"))
        expect_identical(as.matrix(current), FUN2(FUN, M, x, coo, y, svt, z))
    }
})

