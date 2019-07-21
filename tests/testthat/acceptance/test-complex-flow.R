context("  Complex flow")

# https://stackoverflow.com/q/56297316/5793905
test_that("The complex flow shown can be expressed with verbs.", {
    set.seed(322902L)

    consEx = data.table::data.table(
        begin = as.POSIXct(c(
            "2019-04-01 00:00:10",
            " 2019-04-07 10:00:00",
            "2019-04-10 23:00:00",
            "2019-04-12 20:00:00",
            "2019-04-15 10:00:00",
            "2019-04-20 10:00:00",
            "2019-04-22 13:30:00",
            "2019-04-10 15:30:00",
            "2019-04-12 21:30:00",
            "2019-04-15 20:00:00"
        )),
        end = as.POSIXct(c(
            "2019-04-01 20:00:00",
            "2019-04-07 15:00:00",
            "2019-04-11 10:00:00",
            "2019-04-12 23:30:00",
            "2019-04-15 15:00:00",
            "2019-04-21 12:00:00",
            "2019-04-22 17:30:00",
            "2019-04-10 20:00:00",
            "2019-04-13 05:00:00",
            "2019-04-15 12:30:00"
        )),
        carId = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5),
        tripId = c(1:10)
    )

    alertsEx = data.table::data.table(
        timestamp = as.POSIXct(c(
            "2019-04-01 10:00:00",
            "2019-04-01 10:30:00",
            "2019-04-01 15:00:00",
            "2019-04-15 13:00:00",
            "2019-04-22 14:00:00",
            "2019-04-22 15:10:00",
            "2019-04-22 15:40:00",
            "2019-04-10 16:00:00",
            "2019-04-10 17:00:00",
            "2019-04-13 04:00:00"
        )),
        type = c("T1", "T2", "T1", 'T3', "T1", "T1", "T3", "T2", "T2", "T1"),
        carId = c(1, 1, 1, 2, 3, 3, 3, 4, 4, 4),
        additionalInfo1 = rnorm(10, mean = 10, sd = 4)
    )

    cons_ex <- data.table::copy(consEx)

    types <- unique(alertsEx$type)

    joined <- consEx[alertsEx,
                     .(carId, tripId, type, additionalInfo1),
                     on = .(carId, begin <= timestamp, end >= timestamp)]

    aggregated <- joined[, .(typeCount = .N, typeMean = mean(additionalInfo1)), by = .(carId, tripId, type)]

    totals <- aggregated[, .(totals = sum(typeCount)), by = .(carId, tripId)]

    wide <- data.table::dcast(
        aggregated,
        carId + tripId ~ type,
        value.var = c("typeCount", "typeMean"),
        sep = "",
        fill = 0
    )

    replaceNA <- function(x) { replace(x, is.na(x), 0) }

    consEx[, `:=`(as.character(outer(types, c("Count", "Mean"), paste0)),
                  lapply(wide[consEx,
                              as.character(outer(types, c("typeCount", "typeMean"),
                                                 function(a, b) {
                                                     paste0(b, a)
                                                 })),
                              with = FALSE,
                              on = .(carId, tripId)],
                         replaceNA))]

    consEx[, totals := sapply(totals[consEx, x.totals, on = .(carId, tripId)], replaceNA)]

    data.table::setcolorder(consEx, c("carId", "tripId", "begin", "end"))

    # ----------------------------------------------------------------------------------------------

    types <- alertsEx[, unique(type)]

    aggregated <- cons_ex %>%
        start_expr %>%
        filter(1L:.N) %>% # just to test another 'i' clause before right_join
        right_join(alertsEx, carId, begin <= timestamp, end >= timestamp) %>%
        select(carId, tripId, type, additionalInfo1) %>%
        chain %>%
        group_by(carId, tripId, type) %>%
        transmute(typeCount = .N, typeMean = mean(additionalInfo1)) %>%
        group_by(carId, tripId) %>%
        mutate(totals = sum(typeCount)) %>%
        end_expr

    wide <- data.table::dcast(
        aggregated,
        ... ~ type,
        value.var = c("typeCount", "typeMean"),
        sep = "",
        fill = 0
    )

    sd_cols <- outer(types, c("typeCount", "typeMean"), function(a, b) { paste0(b, a) })
    dim(sd_cols) <- NULL
    names(sd_cols) <- as.character(outer(types, c("Count", "Mean"), paste0))
    sd_cols <- c(sd_cols, totals = "totals")
    sd_cols <- lapply(sd_cols, function(sd_col) {
        col_sym <- rlang::sym(sd_col)
        rlang::expr(replaceNA(!!col_sym))
    })

    replaceNA <- function(x) { replace(x, is.na(x), 0) }

    ans <- data.table::copy(cons_ex) %>%
        start_expr %>%
        mutate_join(wide, carId, tripId, .SDcols = list(!!!sd_cols)) %>%
        end_expr %>%
        (data.table::setcolorder)(c("carId", "tripId", "begin", "end"))

    expect_identical(ans, consEx)

    # ----------------------------------------------------------------------------------------------

    aggregated <- cons_ex %>%
        right_join(alertsEx, carId, begin <= timestamp, end >= timestamp, .expr = TRUE) %>%
        select(carId, tripId, type, additionalInfo1) %>%
        group_by(carId, tripId, type) %>%
        transmute(typeCount = .N, typeMean = mean(additionalInfo1)) %>%
        group_by(carId, tripId) %>%
        mutate(totals = sum(typeCount))

    wide <- data.table::dcast(
        aggregated,
        ... ~ type,
        value.var = c("typeCount", "typeMean"),
        sep = "",
        fill = 0
    )

    ans <- data.table::copy(cons_ex) %>%
        mutate_join(wide, carId, tripId, .SDcols = list(!!!sd_cols)) %>%
        (data.table::setcolorder)(c("carId", "tripId", "begin", "end"))

    expect_identical(ans, consEx)
})

test_that("https://stackoverflow.com/q/56918395/5793905 works.", {
    dt <- data.table::data.table(
        analyst = rep(1:2, 10),
        id = rep(1:5, 4),
        year = rep(as.Date(c('2009-12-31', '2009-12-31', '2010-12-31', '2010-12-31'), format = '%Y-%m-%d'),
                   5),
        fdate = as.Date(format = '%Y-%m-%d', c(
            '2009-07-31',
            '2009-02-26',
            '2010-01-31',
            '2010-05-15',
            '2009-06-30',
            '2009-10-08',
            '2010-07-31',
            '2010-11-30',
            '2009-01-31',
            '2009-06-26',
            '2010-05-03',
            '2010-04-13',
            '2009-10-30',
            '2009-11-02',
            '2010-03-28',
            '2010-10-14',
            '2009-02-17',
            '2009-09-14',
            '2010-08-02',
            '2010-10-03'
        ))
    )

    expected <- data.table::data.table(
        key = c("id", "year", "analyst"),
        analyst = c(
            1L,
            2L,
            1L,
            2L,
            1L,
            2L,
            1L,
            2L,
            1L,
            2L,
            1L,
            2L,
            1L,
            2L,
            1L,
            2L,
            1L,
            2L,
            1L,
            2L
        ),
        id = c(
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            3L,
            3L,
            3L,
            3L,
            4L,
            4L,
            4L,
            4L,
            5L,
            5L,
            5L,
            5L
        ),
        year = structure(
            c(
                14609,
                14609,
                14974,
                14974,
                14609,
                14609,
                14974,
                14974,
                14609,
                14609,
                14974,
                14974,
                14609,
                14609,
                14974,
                14974,
                14609,
                14609,
                14974,
                14974
            ),
            class = "Date"
        ),
        fdate = structure(
            c(
                14456,
                14525,
                14732,
                14896,
                14292,
                14301,
                14821,
                14712,
                14547,
                14501,
                14640,
                14943,
                14275,
                14550,
                14823,
                14744,
                14425,
                14421,
                14696,
                14885
            ),
            class = "Date"
        ),
        first = c(
            1L,
            0L,
            1L,
            0L,
            1L,
            0L,
            0L,
            1L,
            0L,
            1L,
            1L,
            0L,
            1L,
            0L,
            0L,
            1L,
            0L,
            1L,
            1L,
            0L
        ),
        rev = c(
            0L,
            1L,
            0L,
            1L,
            0L,
            1L,
            1L,
            0L,
            1L,
            0L,
            0L,
            1L,
            0L,
            1L,
            1L,
            0L,
            1L,
            0L,
            0L,
            1L
        )
    )

    dt0 <- data.table::copy(dt)

    ans <- dt %>%
        data.table::setkey(id, year, analyst) %>%
        start_expr %>%
        mutate(first = 0L) %>%
        chain %>%
        arrange(fdate) %>%
        distinct(id, year) %>%
        left_join(dt, id, year, analyst, fdate) %>%
        mutate(first = 1L) %>%
        mutate(rev = +(!first)) %>%
        end_expr

    expect_identical(ans, expected)

    dt <- dt0

    ans <- dt %>%
        data.table::setkey(id, year, analyst) %>%
        mutate(first = 0L) %>%
        arrange(fdate) %>%
        distinct(id, year) %>%
        left_join(dt, id, year, analyst, fdate, .expr = TRUE) %>%
        mutate(first = 1L) %>%
        mutate(rev = +(!first))

    expect_identical(ans, expected)
})
