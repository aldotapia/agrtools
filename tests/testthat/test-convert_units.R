test_that("same-unit conversion returns the value", {
  expect_equal(convert_units(5, "length", "m", "m"), 5)
})

test_that("known conversions", {
  expect_equal(convert_units(0, "temperature", "celsius", "fahrenheit"), 32)
  expect_equal(convert_units(1, "pressure", "atm", "kPa"), 101.325)
  expect_equal(convert_units(1, "daily radiation", "cal/cm2/day", "MJ/m2/day"), 0.041868)
  expect_equal(convert_units(1, "daily radiation", "W/m2/day", "MJ/m2/day"), 0.0864)
  expect_equal(convert_units(1, "length", "mi", "ft"), 5280)
  expect_equal(convert_units(1, "density", "lb/ft3", "lb/yd3"), 27)
  expect_equal(convert_units(36, "speed", "km/h", "m/s"), 10)
})

test_that("every pair round-trips", {
  units = list(
    "temperature" = c("celsius", "fahrenheit", "kelvin"),
    "pressure" = c("atm", "mmHg", "kPa", "bar", "mbar", "psi", "mH2O"),
    "density" = c("kg/m3", "g/cm3", "lb/ft3", "lb/gal", "ton/yd3"),
    "conductivity" = c("dS/m", "uS/cm", "tdi mg/L", "Meq/l")
  )
  for (what in names(units)) for (a in units[[what]]) for (b in units[[what]]) {
    expect_equal(convert_units(convert_units(12.3, what, a, b), what, b, a), 12.3)
  }
})

test_that("invalid units error", {
  expect_error(convert_units(1, "length", "m", "kg"), "'to' should be one of")
})
