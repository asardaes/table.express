context("  Select (a.k.a Transmute)")

test_that("An empty clause is not an error.", {
    ans <- DT %>% start_expr %>% select() %>% end_expr
    expect_identical(ans, DT)
})

test_that("The select verb works with single columns.", {
    expectations <- function(ans) {
        expect_identical(ncol(ans), 1L)
        expect_identical(ans$mpg, mtcars$mpg)
    }

    ans <- DT %>% start_expr %>% select(mpg) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(1L) %>% end_expr
    expectations(ans)

    v <- rlang::sym("mpg")
    ans <- DT %>% start_expr %>% select(!!v) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select("mpg", .parse = TRUE) %>% end_expr
    expectations(ans)

    v <- "mpg"
    ans <- DT %>% start_expr %>% select(!!v, .parse = TRUE) %>% end_expr
    expectations(ans)
})

test_that("The select verb works with multiple columns.", {
    expectations <- function(ans) {
        expect_identical(ncol(ans), 2L)
        expect_identical(ans$mpg, mtcars$mpg)
        expect_identical(ans$cyl, mtcars$cyl)
    }

    ans <- DT %>% start_expr %>% select(mpg, cyl) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(mpg:cyl) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(1:2) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(1L:2L) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(c(1L, 2L)) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select("mpg", "cyl", .parse = TRUE) %>% end_expr
    expectations(ans)

    v <- c("mpg", "cyl")
    ans <- DT %>% start_expr %>% select(!!!v, .parse = TRUE) %>% end_expr
    expectations(ans)

    v <- rlang::syms(c("mpg", "cyl"))
    ans <- DT %>% start_expr %>% select(!!!v) %>% end_expr
    expectations(ans)
})

test_that("Transmuting by value without parsing works.", {
    ans <- DT %>% start_expr %>% transmute(ans = mpg * 2) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans_name <- "ans"
    ans <- DT %>% start_expr %>% transmute(!!ans_name := mpg * 2) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans <- DT %>% start_expr %>% transmute(mpg2 = mpg * 2, disp0.5 = disp / 2) %>% end_expr
    expect_identical(ncol(ans), 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Transmuting by value with parsing works.", {
    ans <- DT %>% start_expr %>% transmute(ans = "mpg * 2", .parse = TRUE) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans_name <- "ans"
    ans <- DT %>% start_expr %>% transmute(!!ans_name := "mpg * 2", .parse = TRUE) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans <- DT %>% start_expr %>% transmute(mpg2 = "mpg * 2", disp0.5 = "disp / 2", .parse = TRUE) %>% end_expr

    expect_identical(ncol(ans), 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Select with tidyselect works.", {
    expectations <- function(ans, nc) {
        expect_identical(nrow(ans), nrow(DT))
        expect_identical(ncol(ans), nc)
    }

    ans <- DT %>% start_expr %>% select(starts_with("M", T)) %>% end_expr
    expectations(ans, 1L)

    ans <- DT %>% start_expr %>% select(ends_with("M", T)) %>% end_expr
    expectations(ans, 1L)

    ans <- DT %>% start_expr %>% select(contains("M", T)) %>% end_expr
    expectations(ans, 2L)

    ans <- DT %>% start_expr %>% select(matches(".*M.*", T)) %>% end_expr
    expectations(ans, 2L)

    ans <- DT %>% start_expr %>% select(one_of("mpg", "am")) %>% end_expr
    expectations(ans, 2L)

    ans <- DT %>% start_expr %>% select(last_col()) %>% end_expr
    expectations(ans, 1L)

    ans <- DT %>% start_expr %>% select(everything()) %>% end_expr
    expectations(ans, 11L)

    colnames(DT) <- paste0("var", 0:10)
    ans <- DT %>% start_expr %>% select(num_range("var", 0:1)) %>% end_expr
    expectations(ans, 2L)
})
