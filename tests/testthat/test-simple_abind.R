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

test_that("simple_abind()", {
    simple_abind <- S4Arrays:::simple_abind
    abind0 <- S4Arrays:::abind0

    m1 <- .TEST_matrices[[1L]]
    m2 <- .TEST_matrices[[2L]][1:3, ]
    m3 <- .TEST_matrices[[3L]][1:3, ]
    expected <- abind0(m1, m2, m3, along=2)
    expect_identical(simple_abind(m1, m2, m3, along=2), expected)

    a1 <- .TEST_arrays[[1L]]
    a2 <- .TEST_arrays[[2L]]
    a3 <- .TEST_arrays[[3L]]
    expected <- abind0(a1, a2, a3, along=1)
    expect_identical(simple_abind(a1, a2, a3, along=1), expected)

    a4 <- a1[ , , 4, drop=FALSE]
    a5 <- S4Arrays:::set_dim(m3, c(dim(m3), 1L))
    expected <- abind0(a4, a5, along=3)
    expect_identical(simple_abind(a4, a5, along=3), expected)

    expected <- abind0(a5, a4, along=3)
    expect_identical(simple_abind(a5, a4, along=3), expected)
})

test_that("homogenize_abind_input_objects()", {
    homogenize_abind_input_objects <- S4Arrays:::homogenize_abind_input_objects

    objects <- list(NULL, NULL, matrix(nrow=2, ncol=5), NULL)
    for (along in 1:2) {
        current <- homogenize_abind_input_objects(objects, along=along)
        expect_true(is.list(current))
        expect_true(length(current) == 1L)
        expect_identical(attr(current, "dims"), cbind(c(2L, 5L)))
        expect_identical(attr(current, "along"), along)
        expect_identical(current[[1L]], objects[[3L]])
    }
    current <- homogenize_abind_input_objects(objects, along=3)
    expect_true(is.list(current))
    expect_true(length(current) == 1L)
    expect_identical(current[[1L]], array(dim=c(2, 5, 1)))
    expect_identical(attr(current, "dims"), cbind(dim(current[[1L]])))
    expect_identical(attr(current, "along"), 3L)

    objects <- list(matrix(nrow=2, ncol=5), NULL, array(1:24, c(2, 3, 1)))
    current <- homogenize_abind_input_objects(objects, along=2)
    expect_true(is.list(current))
    expect_true(length(current) == 2L)
    expect_identical(attr(current, "dims"), cbind(c(2L, 5L, 1L), c(2L, 3L, 1L)))
    expect_identical(attr(current, "along"), 2L)
    expect_true(all(sapply(current, class) == "array"))
    expect_identical(sapply(current, dim), attr(current, "dims"))
})

test_that("simple_abind2()", {
    simple_abind2 <- S4Arrays:::simple_abind2
    abind0 <- S4Arrays:::abind0

    m1 <- .TEST_matrices[[1L]]
    m2 <- .TEST_matrices[[2L]][1:3, ]
    m3 <- .TEST_matrices[[3L]][1:3, ]

    a1 <- .TEST_arrays[[1L]]
    a2 <- .TEST_arrays[[2L]]
    a3 <- .TEST_arrays[[3L]]

    objects <- list(m1, m2, m3)
    expect_identical(simple_abind2(objects), abind0(objects))

    expected <- abind0(objects, rev.along=0)
    expect_identical(simple_abind2(objects, along=3), expected)
    expect_identical(simple_abind2(objects, rev.along=0), expected)

    objects <- list(a1, m3)
    expect_identical(simple_abind2(objects), abind0(objects))

    objects <- list(m3, a1)
    expect_identical(simple_abind2(objects), abind0(objects))

    a4 <- a1[ , , 4, drop=FALSE]
    a5 <- S4Arrays:::set_dim(m3, c(dim(m3), 1L))

    expected <- abind0(a4, a5, rev.along=0)
    expect_identical(simple_abind2(list(a4, m3), along=4), expected)
    expect_identical(simple_abind2(list(a4, m3), rev.along=0), expected)

    expected <- abind0(a5, a4, rev.along=0)
    expect_identical(simple_abind2(list(m3, a4), along=4), expected)
    expect_identical(simple_abind2(list(m3, a4), rev.along=0), expected)

    ## --- Some edge cases ---
    expect_identical(simple_abind2(list(a1)), a1)
    expect_identical(simple_abind2(list()), NULL)
})

