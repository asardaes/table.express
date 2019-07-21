require("data.table")

data("mtcars")

DT <- as.data.table(mtcars)

# ====================================================================================
# Simple dplyr-like transformations

DT %>%
    group_by(cyl) %>%
    filter(vs == 0, am == 1) %>%
    transmute(mean_mpg = mean(mpg)) %>%
    arrange(-cyl)

# Equivalent to previous
DT %>%
    start_expr %>%
    transmute(mean_mpg = mean(mpg)) %>%
    where(vs == 0, am == 1) %>%
    group_by(cyl) %>%
    order_by(-cyl) %>%
    end_expr

# Modification by reference
DT %>%
    where(gear %% 2 != 0, carb %% 2 == 0) %>%
    mutate(wt_squared = wt ^ 2)

print(DT)

# Deletion by reference
DT %>%
    mutate(wt_squared = NULL) %>%
    print

# Support for tidyslect helpers

DT %>%
    select(ends_with("t"))

# ====================================================================================
# Helpers to transform a subset of data

# Like DT[, (whole) := lapply(.SD, as.integer), .SDcols = whole]
whole <- names(DT)[sapply(DT, function(x) { all(x %% 1 == 0) })]
DT %>%
    mutate_sd(as.integer, .SDcols = whole)

sapply(DT, class)

# Like DT[, lapply(.SD, fun), .SDcols = ...]
DT %>%
    transmute_sd((.COL - mean(.COL)) / sd(.COL),
                 .SDcols = setdiff(names(DT), whole))

# Filter several with the same condition
DT %>%
    filter_sd(.COL == 1, .SDcols = c("vs", "am"))

# Using secondary indices, i.e. DT[.(4, 5), on = .(cyl, gear)]
DT %>%
    filter_on(cyl = 4, gear = 5) # note we don't use ==

# Chaining
DT %>%
    start_expr %>%
    mutate_sd(as.integer, .SDcols = whole) %>%
    chain %>%
    filter_sd(.COL == 1, .SDcols = c("vs", "am"), .collapse = `|`) %>%
    transmute_sd(scale, .SDcols = !is.integer(.COL)) %>%
    end_expr

# The previous is quivalent to
DT[, (whole) := lapply(.SD, as.integer), .SDcols = whole
   ][vs == 1 | am == 1,
     lapply(.SD, scale),
     .SDcols = names(DT)[sapply(DT, Negate(is.integer))]]

# Alternative to keep all columns (*copying* non-scaled ones)
scale_non_integers <- function(x) {
    if (is.integer(x)) x else scale(x)
}

DT %>%
    filter_sd(.COL == 1, .SDcols = c("vs", "am"), .collapse = `|`) %>%
    transmute_sd(everything(), scale_non_integers)

# Without copying non-scaled
DT %>%
    where(vs == 1 | am == 1) %>%
    mutate_sd(scale, .SDcols = names(DT)[sapply(DT, Negate(is.integer))])

print(DT)
