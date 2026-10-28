test_that("Ra matches FAO-56 example 8", {
  lat = convert_units(-20, "angle", "deg", "rad")
  expect_equal(extraterrestrial_radiation(246, lat), 32.2, tolerance = 0.01)
})

test_that("Ra is finite at polar latitudes", {
  lat = convert_units(80, "angle", "deg", "rad")
  expect_true(all(is.finite(extraterrestrial_radiation(c(1, 172), lat))))
})

test_that("et0pm matches FAO-56 example 18", {
  et0 = et0pm("2024-07-06", 50.8, 100, 21.5, 12.3, 10, 22.07, 84, 63,
              h = 10, u_h_u = "km/h", r_s_u = "MJ/m2/day")
  expect_equal(et0, 3.9, tolerance = 0.02)
  expect_equal(et0pm("2024-07-06", 50.8, 100, 21.5, 12.3, 10, 22.07, 0.84, 0.63,
                     h = 10, u_h_u = "km/h", r_s_u = "MJ/m2/day",
                     rh_max_u = "fraction", rh_min_u = "fraction"), et0)
})

test_that("et0 functions accept character date vectors", {
  d = c("2024-03-09", "2024-03-10")
  expect_length(et0h(d, c(27, 28), c(8, 9), -30.63), 2)
  expect_length(et0hs(d, c(27, 28), c(8, 9), -30.63), 2)
  expect_error(et0h("09/03/2024", 27, 8, -30.63), "date")
})

test_that("et0pan matches FAO-56 pan examples", {
  pan = function(cl, fe) et0pan(7.9, calculate_kp = TRUE, u2 = 1.9, fet = 1000,
                               rh_mean = 73, class = cl, fetch = fe)
  expect_equal(pan("class a", "green"), 6.6, tolerance = 0.01)
  expect_equal(pan("class a", "dry"), 4.8, tolerance = 0.01)
  expect_equal(pan("colorado", "green"), 7.7, tolerance = 0.01)
  expect_equal(pan("colorado", "dry"), 5.4, tolerance = 0.01)
  expect_equal(et0pan(7.9, calculate_kp = TRUE, u2 = 1.9, fet = 1000, rh_mean = 73),
               pan("class a", "green"))
})

test_that("psychrometric helpers match FAO-56 examples", {
  expect_equal(atm_pressure(1800), 81.8, tolerance = 0.01)
  expect_equal(psychrometric_constant(atm_pressure(1800)), 0.054, tolerance = 0.01)
  expect_equal((sat_vapor_pressure(24.5) + sat_vapor_pressure(15)) / 2, 2.39, tolerance = 0.01)
  expect_equal(slope_vapor_pressure(25), 0.189, tolerance = 0.01)
  expect_equal(actual_vapor_pressure(25, 18, rh_max = 82, rh_min = 54), 1.70, tolerance = 0.01)
  expect_equal(actual_vapor_pressure(25, 18), sat_vapor_pressure(18))
  expect_equal(vpd(25, 18, rh_max = 82, rh_min = 54),
               (sat_vapor_pressure(25) + sat_vapor_pressure(18)) / 2 - 1.70, tolerance = 0.01)
  expect_equal(wind_at_2m(3.2, 10), 2.4, tolerance = 0.01)
  expect_equal(wind_at_2m(1.5), 1.5, tolerance = 0.001)
  expect_equal(net_radiation(14.5, 18.8, 25.1, 19.1, 2.1), 7.6, tolerance = 0.01)
})

test_that("dew point inverts saturation vapour pressure", {
  expect_equal(dew_point(c(10, 25), 100), c(10, 25))
  td = dew_point(20, 60)
  expect_equal(sat_vapor_pressure(td), 0.6 * sat_vapor_pressure(20))
})
