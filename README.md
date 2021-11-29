<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/table.express)](https://cran.r-project.org/package=table.express)
[![R-CMD-check](https://github.com/asardaes/table.express/workflows/R-CMD-check/badge.svg)](https://github.com/asardaes/table.express/actions)
[![codecov](https://codecov.io/gh/asardaes/table.express/branch/master/graph/badge.svg)](https://codecov.io/gh/asardaes/table.express)

# Table express

Use `dplyr` verbs, as well as custom ones, to build `data.table`
expressions. Check the
[vignette](https://asardaes.github.io/table.express/articles/table.express.html)
for more information.

## Essence

The basic idea is to parse the input from all the verbs and create a
single expression that, after evaluation, simply delegates all the
actual computations to `data.table`, letting it handle all optimizations
as usual.

``` r
# the expression is what matters here, input is left empty
data.table() %>%
    start_expr %>%
    select(col) %>%
    where(var == val) %>%
    order_by(v)
#> .DT_[var == val, list(col)][order(v)]
```

The input `data.table` is always assigned in the evaluation’s
environment as the `.DT_` pronoun

In many cases character input can also be supported, which could be
useful for other packages that use `data.table`.

## License

[Mozilla Public License Version 2.0](LICENSE)

This software package was developed independently of any organization or
institution that is or has been associated with the author.
