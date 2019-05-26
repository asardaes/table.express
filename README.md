<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/table.express)](https://cran.r-project.org/package=table.express)
[![Travis-CI Build
Status](https://travis-ci.org/asardaes/table.express.svg?branch=master)](https://travis-ci.org/asardaes/table.express)
[![AppVeyor Build
status](https://ci.appveyor.com/api/projects/status/bb9606tfe648gajn?svg=true)](https://ci.appveyor.com/project/asardaes/table-express)
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
environment as the `.DT_` variable.

In many cases character input can also be supported, which could be
useful for other packages that use `data.table`.
