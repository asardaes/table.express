<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/asardaes/table.express.svg?branch=master)](https://travis-ci.org/asardaes/table.express)
[![AppVeyor Build
status](https://ci.appveyor.com/api/projects/status/bb9606tfe648gajn?svg=true)](https://ci.appveyor.com/project/asardaes/table-express)
[![codecov](https://codecov.io/gh/asardaes/table.express/branch/master/graph/badge.svg)](https://codecov.io/gh/asardaes/table.express)

# Table express

Use `dplyr` verbs to build `data.table` expressions.

This is WIP. Feel free to help :)

Aliases also provided to resemble SQL syntax more closely.

Powered mostly by [`rlang`](http://cran.r-project.org/package=rlang).

## Essence

The basic idea is to parse the input from all the verbs and create a
single expression that, after evaluation, simply delegates all the
actual computations to `data.table`, letting it handle all optimizations
as usual.

``` r
# the expression is what matters here, input is left empty
data.table() %>%
    start_expr %>%
    select(.(col)) %>%
    where(var == val) %>%
    order_by(v)
#> .DT_[var == val, .(col), with = TRUE][order(v)]
```

The input `data.table` is alwas assigned in the evaluation’s environment
as the `.DT_` variable.

In many cases character input can also be supported, which could be
useful for other packages that use `data.table`.

## Examples

``` r
data("mtcars")

# Equivalent to:
# as.data.table(mtcars)[
#     cyl < 8L, mpg:disp][
#         order(-cyl), .(mean_mpg = mean(mpg), mean_disp = mean(disp)), by = cyl]
as.data.table(mtcars) %>%
    start_expr %>%
    
    select(mpg:disp) %>%
    where(cyl < 8L) %>%
    
    chain %>%
    
    select(mean_mpg = mean(mpg), mean_disp = mean(disp)) %>%
    group_by(cyl) %>%
    order_by(-cyl) %>%
    
    end_expr
#>    cyl mean_mpg mean_disp
#> 1:   6 19.74286  183.3143
#> 2:   4 26.66364  105.1364

as.data.table(mtcars) %>%
    start_expr %>%
    select("mpg", "cyl", "disp", with = FALSE) %>%
    where("vs == 0L", "am == 0L", .collapse = `|`, .parse = TRUE) %>%
    order_by("mpg", "-cyl", .parse = TRUE) %>%
    end_expr
#>      mpg cyl  disp
#>  1: 10.4   8 472.0
#>  2: 10.4   8 460.0
#>  3: 13.3   8 350.0
#>  4: 14.3   8 360.0
#>  5: 14.7   8 440.0
#>  6: 15.0   8 301.0
#>  7: 15.2   8 275.8
#>  8: 15.2   8 304.0
#>  9: 15.5   8 318.0
#> 10: 15.8   8 351.0
#> 11: 16.4   8 275.8
#> 12: 17.3   8 275.8
#> 13: 17.8   6 167.6
#> 14: 18.1   6 225.0
#> 15: 18.7   8 360.0
#> 16: 19.2   8 400.0
#> 17: 19.2   6 167.6
#> 18: 19.7   6 145.0
#> 19: 21.0   6 160.0
#> 20: 21.0   6 160.0
#> 21: 21.4   6 258.0
#> 22: 21.5   4 120.1
#> 23: 22.8   4 140.8
#> 24: 24.4   4 146.7
#> 25: 26.0   4 120.3
#>      mpg cyl  disp
```
