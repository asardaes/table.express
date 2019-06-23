context("  Left join chain")

test_that("A chain of left joins works.", {
    expected <- website[, .(name, session_id)][paypal, on = "name", allow = TRUE]
    expected <- website[expected, on = c("name", "session_id")]

    ans <- paypal %>%
        start_expr %>%
        left_join(website[, .(name, session_id)], name) %>%
        frame_append(allow = TRUE) %>%
        left_join(website, name, session_id) %>%
        end_expr

    expect_identical(ans, expected)
})

# https://stackoverflow.com/q/56565051/5793905
test_that("Recursive left joining works.", {
    a <- data.table::data.table(id = c(1, 2, 3),
                                city = c("city1", "city2", "city2"),
                                height = c("tall", "tall", "short"),
                                hair = c("black", "black", "blonde"))

    a_unique <- a[, lapply(.SD, function(x) { list(unique(x)) })]

    b <- data.table::data.table(city = c("city1", "city1", "city2", "city2"),
                                height = c(NA, "tall", "short", NA),
                                hair = c("black", NA, "blonde", NA),
                                name = c("dave", "harry", "jack", "william"))

    b_names <- names(b)

    expanded <- Map(a_unique, names(a_unique), f = function(au, nm) {
        au <- au[[1L]]
        na_class <- class(au)
        ans <- data.table::data.table(as(NA, na_class), all = au)
        data.table::setnames(ans, c(nm, "all"))
        ans
    })

    harmonize <- function(mat) {
        ans <- as.vector(t(mat))
        ans[!is.na(ans)]
    }

    expand_recursively <- function(dt, cols, penv = rlang::caller_env()) {
        if (length(cols) == 0L) return(dt$eval(parent_env = penv, by_ref = TRUE))

        current <- cols[1L]
        next_cols <- cols[-1L]
        not_current <- setdiff(b_names, current)

        harmonize_call <- rlang::call2("list", !!current := rlang::expr(harmonize(as.matrix(.SD))))

        next_dt <- dt %>%
            left_join(expanded[[current]], !!current) %>%
            select(c(!!harmonize_call, mget(!!not_current))) %>%
            frame_append(.SDcols = c(!!current, "all"), allow = TRUE)

        expand_recursively(next_dt, next_cols, penv)
    }

    b_expanded <- expand_recursively(start_expr(b), intersect(names(a), names(b)))
    data.table::setcolorder(b_expanded, names(b))

    expected <- data.table::data.table(
        city = c(
            "city1",
            "city1",
            "city1",
            "city1",
            "city2",
            "city2",
            "city2",
            "city2",
            "city2"
        ),
        height = c(
            "tall",
            "short",
            "tall",
            "tall",
            "short",
            "tall",
            "tall",
            "short",
            "short"
        ),
        hair = c(
            "black",
            "black",
            "black",
            "blonde",
            "blonde",
            "black",
            "blonde",
            "black",
            "blonde"
        ),
        name = c(
            "dave",
            "dave",
            "harry",
            "harry",
            "jack",
            "william",
            "william",
            "william",
            "william"
        )
    )

    expect_identical(b_expanded, expected)
})
