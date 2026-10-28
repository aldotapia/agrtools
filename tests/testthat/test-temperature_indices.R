test_that("degree days are vectorized", {
  expect_equal(gdd(c(35, 20), c(10, 10)), c(10, 5))
  expect_equal(hdd(c(20, 10), c(10, 0)), c(3, 13))
  expect_equal(cdd(c(30, 10), c(20, 0)), c(7, 0))
})

test_that("daily chilling hours stay within [0, 24 * kf]", {
  expect_equal(dailychillinghours(c(5, 20, 8), c(2, 6, 8)), c(24, 2.06, NA))
})

test_that("chilling hours methods", {
  t = c(5, 6, 7, 8, 15, 18)
  expect_equal(chillinghours(t), 3)
  expect_equal(chillinghours(t, method = "utah"), 3.5)
})

test_that("hourly_to_daily aggregates by day", {
  df = hourly_to_daily(piscoelqui$data, use_suffix = TRUE)
  expect_equal(nrow(df), 365)
  expect_s3_class(df$date, "Date")
  h = piscoelqui$data
  h$date = as.POSIXct(h$date, tz = "UTC")
  expect_equal(nrow(hourly_to_daily(h)), 365)
})
