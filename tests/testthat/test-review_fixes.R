test_that("kc_curve handles zero-length stages", {
  expect_silent(kc <- kc_curve(1:6, 2, 0, 2, 2, 0.3, 1.2, 0.5))
  expect_equal(kc, c(0.3, 0.3, 1.2, 1.2, 0.85, 0.5))
  expect_equal(kc_curve(1:3, 0, 2, 1, 0, 0.3, 1.2, 0.5), c(0.75, 1.2, 1.2))
})

test_that("water_balance rejects NA et0 or kc with a clear message", {
  expect_error(water_balance(c(5, NA, 5), c(1, 1, 1), theta_fc = 0.3,
                             theta_wp = 0.1, zr = 1), "should not contain NA")
})

test_that("water_balance stays balanced when depletion hits TAW", {
  wb = water_balance(rep(20, 5), rep(1, 5), theta_fc = 0.2, theta_wp = 0.15,
                     zr = 0.5, p = 0.9)
  expect_equal(max(wb$dr), wb$taw[1])
  expect_equal(tail(wb$dr, 1), sum(wb$etc_adj) + sum(wb$dp))
})

test_that("hourly_to_daily accepts a function for func", {
  a = hourly_to_daily(piscoelqui$data, func = mean)
  b = hourly_to_daily(piscoelqui$data, func = "mean")
  expect_equal(a, b)
})

test_that("chill_portions returns NA with na.rm = FALSE", {
  expect_true(is.na(chill_portions(c(5, NA, 6), na.rm = FALSE)))
  expect_equal(chill_portions(c(5, NA, 6), na.rm = FALSE, cumulative = TRUE),
               rep(NA_real_, 3))
})

test_that("date-times keep their local calendar day", {
  x = as.POSIXct("2023-06-30 22:00", tz = "America/Santiago")
  expect_equal(agrtools:::as_date_vec(x), as.Date("2023-06-30"))
  y = as.POSIXct("2023-06-30 00:30", tz = "Europe/Berlin")
  expect_equal(agrtools:::as_date_vec(y), as.Date("2023-06-30"))
})

test_that("rh_min alone is an error", {
  expect_error(actual_vapor_pressure(25, 18, rh_min = 54), "rh_min alone")
  expect_error(et0pm("2023-06-01", -30, 1240, 15, 2, 2, 150, rh_min = 40),
               "rh_min alone")
})

test_that("hourly_temperature requires consecutive days", {
  expect_error(hourly_temperature(c("2023-07-01", "2023-07-03"), c(15, 16),
                                  c(2, 3), -30), "consecutive")
  expect_error(hourly_temperature(c("2023-07-02", "2023-07-01"), c(15, 16),
                                  c(2, 3), -30), "consecutive")
})
