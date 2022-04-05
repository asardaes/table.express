context("  Misc")

test_that("Namespaced calls to tidyselect helpers are correctly detected.", {
    expect_true(is_tidyselect_call(rlang::expr(tidyselect::everything())))
})
