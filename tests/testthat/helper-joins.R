lhs <- data.table::data.table(x = rep(c("b", "a", "c"), each = 3),
                              y = c(1, 3, 6),
                              v = 1:9)

rhs <- data.table::data.table(x = c("c", "b"),
                              v = 8:7,
                              foo = c(4, 2))

# https://www.r-bloggers.com/understanding-data-table-rolling-joins/

website <- data.table::rbindlist(list(
    data.table::data.table(
        name = rep("Indecisive Isabel", 5),
        session_start_time = as.POSIXct(c("2016-01-01 11:01",
                                          "2016-01-02 8:59",
                                          "2016-01-05 18:18",
                                          "2016-01-07 19:03",
                                          "2016-01-08 19:01"))
    ),
    data.table::data.table(
        name = "Spendy Sally",
        session_start_time = as.POSIXct("2016-01-03 10:00")
    ),
    data.table::data.table(
        name = rep("Frequent Francis", 6),
        session_start_time = as.POSIXct(c("2016-01-02 13:09",
                                          "2016-01-03 19:22",
                                          "2016-01-08 8:44",
                                          "2016-01-08 20:22",
                                          "2016-01-10 17:36",
                                          "2016-01-15 16:56"))
    ),
    data.table::data.table(
        name = rep("Error-prone Erica", 2),
        session_start_time = as.POSIXct(c("2016-01-04 19:12",
                                          "2016-01-04 21:05"))
    ),
    data.table::data.table(
        name = rep("Visitor Vivian", 2),
        session_start_time = as.POSIXct(c("2016-01-01 9:10",
                                          "2016-01-09 2:15"))
    )
))

paypal <- data.table::rbindlist(list(
    data.table::data.table(
        name = "Indecisive Isabel",
        purchase_time = as.POSIXct("2016-01-08 19:10")
    ),
    data.table::data.table(
        name = rep("Spendy Sally", 2),
        purchase_time = as.POSIXct(c("2016-01-03 10:06",
                                     "2016-01-03 10:15"))
    ),
    data.table::data.table(
        name = rep("Frequent Francis", 3),
        purchase_time = as.POSIXct(c("2016-01-03 19:28",
                                     "2016-01-08 20:33",
                                     "2016-01-10 17:46"))
    ),
    data.table::data.table(
        name = "Error-prone Erica",
        purchase_time = as.POSIXct("2016-01-03 08:02")
    ),
    data.table::data.table(
        name = "Mom",
        purchase_time = as.POSIXct("2015-12-02 17:58")
    )
))

data.table::setkey(website, name, session_start_time)
data.table::setkey(paypal, name, purchase_time)

website[, session_id := .GRP, by = .(name, session_start_time)]
paypal[, payment_id := .GRP, by = .(name, purchase_time)]
