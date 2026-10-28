#' @title Convert units
#'
#' @description
#' Convert units of a value to another unit, conversions are based on the type of value
#'
#' @param value numeric. Value to be converted
#' @param what character. What to convert
#' @param from character. Unit of the value
#' @param to character. Unit to convert to
#'
#' @return numeric. Converted value
#'
#' @details
#' This function converts units of a value to another unit, conversions are based on the type of value:
#'
#' \tabular{ll}{
#'   \strong{Type}   \tab \strong{Units} \cr
#'   temperature     \tab celsius, fahrenheit, kelvin\cr
#'   pressure        \tab atm, mmHg, kPa, bar, mbar, psi, mH2O\cr
#'   mass            \tab kg, g, mg, lb, oz\cr
#'   velocity (or speed) \tab km/h, m/s, mph\cr
#'   radiation       \tab J/m2, MJ/m2, Wh/m2, cal/cm2\cr
#'   daily radiation \tab MJ/m2/day, W/m2/day, cal/cm2/day\cr
#'   time            \tab s, min, h, d\cr
#'   length          \tab m, cm, mm, km, in, ft, yd, mi\cr
#'   area            \tab m2, cm2, mm2, km2, in2, ft2, yd2, mi2\cr
#'   volume          \tab m3, cm3, mm3, l, in3, ft3, yd3, gal\cr
#'   angle           \tab deg, rad, grad, turn\cr
#'   conductivity    \tab dS/m, mS/m, uS/m, mS/cm, uS/cm, tdi mg/L, Meq/l\cr
#'   density         \tab kg/m3, g/cm3, g/m3, mg/m3, lb/ft3, lb/in3, lb/gal, lb/yd3, ton/yd3
#' }
#'
#' "radiation" is energy per unit area (radiant exposure). "W/m2/day" in
#' "daily radiation" means the daily mean irradiance in W/m2. Calories are
#' international table calories (4.1868 J), as in FAO-56. "tdi mg/L" (total
#' dissolved ions) and "Meq/l" are the usual empirical approximations
#' (1 dS/m = 667 mg/L = 10 meq/L). "gal" is the US gallon and "ton" the short
#' ton (2000 lb).
#'
#' @examples
#' convert_units(1, "pressure", "atm", "kPa")
#' convert_units(101.325, "pressure", "kPa", "atm")
#' convert_units(1, "pressure", "atm", "bar")
#' convert_units(1.01325, "pressure", "bar", "atm")
#' convert_units(1, "pressure", "atm", "psi")
#' convert_units(14.6959, "pressure", "psi", "atm")
#' convert_units(1, "pressure", "atm", "mH2O")
#'
#' @export
convert_units = function(value, what, from, to){
  if (!is.numeric(value)) {
    stop("value should be numeric")
  }
  if (!is.character(what)) {
    stop("what should be character")
  }
  if (!is.character(from)) {
    stop("from should be character")
  }
  if (!is.character(to)) {
    stop("to should be character")
  }
  if (what == "speed") {
    what = "velocity"
  }

  # factor of each unit to the first unit of its type
  in2 = 0.0254
  ft = 0.3048
  yd = 0.9144
  mi = 1609.344
  factors = list(
    "temperature" = c("celsius" = NA, "fahrenheit" = NA, "kelvin" = NA),
    "pressure" = c("kPa" = 1, "atm" = 101.325, "mmHg" = 0.133322, "bar" = 100,
                   "mbar" = 0.1, "psi" = 6.894757, "mH2O" = 9.80665),
    "mass" = c("kg" = 1, "g" = 1e-3, "mg" = 1e-6, "lb" = 0.45359237,
               "oz" = 0.028349523125),
    "velocity" = c("m/s" = 1, "km/h" = 1 / 3.6, "mph" = 0.44704),
    "radiation" = c("J/m2" = 1, "MJ/m2" = 1e6, "Wh/m2" = 3600,
                    "cal/cm2" = 41868),
    "daily radiation" = c("MJ/m2/day" = 1, "W/m2/day" = 0.0864,
                          "cal/cm2/day" = 0.041868),
    "time" = c("s" = 1, "min" = 60, "h" = 3600, "d" = 86400),
    "length" = c("m" = 1, "cm" = 0.01, "mm" = 0.001, "km" = 1000, "in" = in2,
                 "ft" = ft, "yd" = yd, "mi" = mi),
    "area" = c("m2" = 1, "cm2" = 1e-4, "mm2" = 1e-6, "km2" = 1e6,
               "in2" = in2^2, "ft2" = ft^2, "yd2" = yd^2, "mi2" = mi^2),
    "volume" = c("m3" = 1, "cm3" = 1e-6, "mm3" = 1e-9, "l" = 1e-3,
                 "in3" = in2^3, "ft3" = ft^3, "yd3" = yd^3,
                 "gal" = 0.003785411784),
    "angle" = c("rad" = 1, "deg" = pi / 180, "grad" = pi / 200, "turn" = 2 * pi),
    "conductivity" = c("dS/m" = 1, "mS/m" = 0.01, "uS/m" = 1e-5, "mS/cm" = 1,
                       "uS/cm" = 1e-3, "tdi mg/L" = 1 / 667, "Meq/l" = 0.1),
    "density" = c("kg/m3" = 1, "g/cm3" = 1000, "g/m3" = 1e-3, "mg/m3" = 1e-6,
                  "lb/ft3" = 0.45359237 / ft^3, "lb/in3" = 0.45359237 / in2^3,
                  "lb/gal" = 0.45359237 / 0.003785411784,
                  "lb/yd3" = 0.45359237 / yd^3, "ton/yd3" = 907.18474 / yd^3)
  )

  if (!what %in% names(factors)) {
    stop(paste("'what' should be one of", paste(names(factors), collapse = ", ")))
  }
  units = names(factors[[what]])
  if (!from %in% units) {
    stop(paste("'from' should be one of", paste(units, collapse = ", ")))
  }
  if (!to %in% units) {
    stop(paste("'to' should be one of", paste(units, collapse = ", ")))
  }

  if (what == "temperature") {
    kelvin = switch(from,
                    "celsius" = value + 273.15,
                    "fahrenheit" = (value + 459.67) * 5 / 9,
                    "kelvin" = value)
    return(switch(to,
                  "celsius" = kelvin - 273.15,
                  "fahrenheit" = kelvin * 9 / 5 - 459.67,
                  "kelvin" = kelvin))
  }
  unname(value * factors[[what]][[from]] / factors[[what]][[to]])
}
