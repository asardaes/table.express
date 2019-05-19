context("  SQL-like")

test_that("Queries like select-where-group-order work as expected.", {
    expected <- state[area > 10000, .(mp = mean(population), mi = max(income)), by = "region"][order(region)]

    ans <- state %>%
        start_expr %>%
        select(mp = mean(population), mi = max(income)) %>%
        where(area > 10000) %>%
        group_by(region) %>%
        order_by(region) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- state["Northeast", .(cx = mean(center_x), cy = mean(center_y)), by = division][order(-division)]

    ans <- state %>%
        start_expr %>%
        select(cx = mean(center_x), cy = mean(center_y)) %>%
        where(.("Northeast")) %>%
        group_by(division) %>%
        order_by(-division) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- state[grepl("^N", name), .(name, sp = sum(population), mi = median(income)), by = .(region, division)][order(region, -division)]

    ans <- state %>%
        start_expr %>%
        select(name, sp = sum(population), mi = median(income)) %>%
        where(grepl("^N", name)) %>%
        group_by(region, division) %>%
        order_by(region, -division) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Queries like select-where-keyby-order work as expected.", {
    expected <- state[area > 10000, .(mp = mean(population), mi = max(income)), keyby = "region"][order(region)]

    ans <- state %>%
        start_expr %>%
        select(mp = mean(population), mi = max(income)) %>%
        where(area > 10000) %>%
        key_by(region) %>%
        order_by(region) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- state["Northeast", .(cx = mean(center_x), cy = mean(center_y)), keyby = division][order(-division)]

    ans <- state %>%
        start_expr %>%
        select(cx = mean(center_x), cy = mean(center_y)) %>%
        where(.("Northeast")) %>%
        key_by(division) %>%
        order_by(-division) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- state[grepl("^N", name), .(name, sp = sum(population), mi = median(income)), keyby = .(region, division)][order(region, -division)]

    ans <- state %>%
        start_expr %>%
        select(name, sp = sum(population), mi = median(income)) %>%
        where(grepl("^N", name)) %>%
        key_by(region, division) %>%
        order_by(region, -division) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Special symbols can be used as usual.", {
    expected <- state[order(region), .I[which.min(center_y)], by = .(region, division)]

    ans <- state %>%
        start_expr %>%
        select(.I[which.min(center_y)]) %>%
        group_by(region, division) %>%
        order_by(region) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- state[region %in% c("South", "West"), .(group_id = .GRP, count = .N), by = .(region, division)]

    ans <- state %>%
        start_expr %>%
        select(group_id = .GRP, count = .N) %>%
        where(region %in% c("South", "West")) %>%
        group_by(region, division) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Complex chains are correctly expressed and evaluated.", {
    expected <- state[population > 1000
                      ][, region:center_y
                        ][, .(region, center_x, center_y, group_id = .GRP), by = division
                          ][order(region)
                            ][abs(center_x) > 85 & abs(center_y) > 40, .(min(center_x), max(center_y)), keyby = group_id]

    ans <- state %>%
        start_expr %>%

        select(region:center_y) %>%
        where(population > 1000) %>%

        select(region, center_x, center_y, group_id = .GRP) %>%
        group_by(division) %>%
        order_by(region) %>%

        select(min(center_x), max(center_y)) %>%
        where(abs(center_x) > 85, abs(center_y) > 40) %>%
        key_by(group_id) %>%

        end_expr

    expect_identical(ans, expected)

    ans <- state %>%
        start_expr %>%

        select(region:center_y) %>%
        where(population > 1000) %>%
        chain %>%
        select(region, center_x, center_y, group_id = .GRP) %>%
        group_by(division) %>%
        order_by(region) %>%
        chain %>%
        select(min(center_x), max(center_y)) %>%
        where(abs(center_x) > 85, abs(center_y) > 40) %>%
        key_by(group_id) %>%

        end_expr

    expect_identical(ans, expected)
})
