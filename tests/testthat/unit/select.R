context("  Select")

test_that("The select verb works with single columns.", {
    expectations <- function(ans) {
        expect_null(dim(ans))
        expect_identical(ans, mtcars$mpg)
    }

    ans <- DT %>% select(mpg) %>% eval_expr
    expectations(ans)

    expectations <- function(ans) {
        expect_identical(ncol(ans), 1L)
        expect_identical(ans$mpg, mtcars$mpg)
    }

    ans <- DT %>% select(.(mpg)) %>% eval_expr
    expectations(ans)

    ans <- DT %>% select("mpg", with = FALSE) %>% eval_expr
    expectations(ans)

    v <- "mpg"
    ans <- DT %>% select(..v) %>% eval_expr
    expectations(ans)
})

test_that("The select verb works with multiple columns.", {
    expectations <- function(ans) {
        expect_identical(ncol(ans), 2L)
        expect_identical(ans$mpg, mtcars$mpg)
        expect_identical(ans$cyl, mtcars$cyl)
    }

    ans <- DT %>% select(mpg, cyl) %>% eval_expr
    expectations(ans)

    ans <- DT %>% select(.(mpg, cyl)) %>% eval_expr
    expectations(ans)

    ans <- DT %>% select("mpg", "cyl", with = FALSE) %>% eval_expr
    expectations(ans)

    ans <- DT %>% select(c("mpg", "cyl"), with = FALSE) %>% eval_expr
    expectations(ans)

    v <- c("mpg", "cyl")
    ans <- DT %>% select(..v) %>% eval_expr
    expectations(ans)
})

test_that("Computing expressions in select work", {
    ans <- DT %>% select(sum(vs + am == 2L)) %>% eval_expr
    expect_identical(ans, 7L)
})
