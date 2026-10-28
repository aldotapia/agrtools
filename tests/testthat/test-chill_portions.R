# reference values from chillR::Dynamic_Model (which lags one hour)
test_that("chill_portions matches the Dynamic model reference", {
  expect_equal(chill_portions(rep(6, 240)), 7.820746, tolerance = 1e-6)
  expect_equal(chill_portions(rep(c(4, 8, 12, 16, 12, 8), 40)), 5.570426, tolerance = 1e-6)
  expect_equal(chill_portions(rep(20, 100)), 0)
  expect_equal(chill_portions(piscoelqui$data$t_mean), 28.07375, tolerance = 1e-6)
})

test_that("chill_portions handles units and cumulative output", {
  t = rep(c(4, 8, 12, 16, 12, 8), 40)
  cp = chill_portions(t, cumulative = TRUE)
  expect_length(cp, length(t))
  expect_false(is.unsorted(cp))
  expect_equal(tail(cp, 1), chill_portions(t))
  expect_equal(chill_portions(t * 9 / 5 + 32, tunit = "fahrenheit"), chill_portions(t))
})

test_that("hourly_temperature builds a bounded daily cycle", {
  h = hourly_temperature(c("2023-07-01", "2023-07-02", "2023-07-03"),
                         t_max = c(15, 17, 16), t_min = c(2, 4, 3), lat = -30.13)
  expect_equal(nrow(h), 72)
  expect_equal(h$hour, rep(0:23, 3))
  day2 = h$temp[h$date == as.Date("2023-07-02")]
  expect_true(all(day2 >= 2 & day2 <= 17))
  expect_equal(max(day2), 17, tolerance = 0.05)
  expect_equal(which.max(day2) - 1, 14)
  expect_false(any(is.na(h$temp)))
})

test_that("hourly_temperature is close to observed Pisco Elqui temperatures", {
  d = hourly_to_daily(piscoelqui$data, use_suffix = TRUE)
  h = hourly_temperature(d$date, d$t_max, d$t_min, lat = -30.13)
  rmse = sqrt(mean((h$temp - piscoelqui$data$t_mean)^2, na.rm = TRUE))
  expect_lt(rmse, 2)
})
