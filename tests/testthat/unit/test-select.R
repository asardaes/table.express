context("  Select")

test_that("The select verb works with single columns.", {
    expectations <- function(ans) {
        expect_identical(ncol(ans), 1L)
        expect_identical(ans$mpg, mtcars$mpg)
    }

    ans <- DT %>% start_expr %>% select(mpg) %>% end_expr
    expectations(ans)

    v <- rlang::sym("mpg")
    ans <- DT %>% start_expr %>% select(!!v) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select("mpg", with = FALSE) %>% end_expr
    expectations(ans)

    v <- "mpg"
    ans <- DT %>% start_expr %>% select(!!v, with = FALSE) %>% end_expr
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

    ans <- DT %>% start_expr %>% select("mpg", "cyl", with = FALSE) %>% end_expr
    expectations(ans)

    v <- c("mpg", "cyl")
    ans <- DT %>% start_expr %>% select(!!!v, with = FALSE) %>% end_expr
    expectations(ans)

    v <- rlang::syms(c("mpg", "cyl"))
    ans <- DT %>% start_expr %>% select(!!!v) %>% end_expr
    expectations(ans)
})

test_that("Computing expressions in select work", {
    ans <- DT %>% start_expr %>% select(sum(vs + am == 2L)) %>% end_expr
    expect_identical(ans$V1, 7L)
})
