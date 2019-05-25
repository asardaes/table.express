data("mtcars")
data("state")

DT <- data.table::as.data.table(mtcars)

state <- data.table::data.table(
    region = state.region,
    division = state.division,
    name = state.name,
    abb = state.abb,
    center_x = state.center$x,
    center_y = state.center$y,
    population = state.x77[, 1L],
    income = state.x77[, 2L],
    area = state.x77[, 8L],
    key = c("region", "division")
)
