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
#' | Type            | Units                                                              |
#' |-----------------|--------------------------------------------------------------------|
#' | temperature     | celsius, fahrenheit, kelvin                                        |
#' | pressure        | atm, mmHg, kPa, bar, mbar, psi, mH2O                               |
#' | mass            | kg, g, mg, lb, oz                                                  |
#' | velocity        | km/h, m/s, mph                                                     |
#' | radiation       | W/m2, cal/cm2, MJ/m2                                               |
#' | daily radiation | MJ/m2/day, W/m2/day, cal/cm2/day                                   |
#' | time            | s, min, h, d                                                       |
#' | length          | m, cm, mm, km, in, ft, yd, mi                                      |
#' | area            | m2, cm2, mm2, km2, in2, ft2, yd2, mi2                              |
#' | volume          | m3, cm3, mm3, l, in3, ft3, yd3, gal                                |
#' | angle           | deg, rad, grad, turn                                               |
#' | conductivity    | dS/m, mS/m, uS/m, mS/cm, uS/cm, tdi mg/L, Meq/l                    |
#' | density         | kg/m3, g/cm3, g/m3, mg/m3, lb/ft3, lb/in3, lb/gal, lb/yd3, ton/yd3 |
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
  # function to convert units useful in agricultural process
  # value: numeric value to be converted
  # what: what to convert
  # from: unit of the value
  # to: unit to convert to
  # returns: converted value
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

  what_list = c("temperature", "pressure", "mass", "velocity", "radiation",
                "daily radiation", "time", "length", "area", "volume", "angle",
                "conductivity", "density")

  temperature_list = c("celsius", "fahrenheit", "kelvin")
  pressure_list = c("atm", "mmHg", "kPa", "bar", "mbar", "psi", "mH2O")
  mass_list = c("kg", "g", "mg", "lb", "oz")
  velocity_list = c("km/h", "m/s", "mph")
  radiation_list = c("W/m2", "cal/cm2", "MJ/m2")
  daily_radiation_list = c("MJ/m2/day", "W/m2/day", "cal/cm2/day")
  time_list = c("s", "min", "h", "d")
  length_list = c("m", "cm", "mm", "km", "in", "ft", "yd", "mi")
  area_list = c("m2", "cm2", "mm2", "km2", "in2", "ft2", "yd2", "mi2")
  volume_list = c("m3", "cm3", "mm3", "l", "in3", "ft3", "yd3", "gal")
  angle_list = c("deg", "rad", "grad", "turn")
  conductivity_list = c("dS/m", "mS/m", "uS/m", "mS/cm", "uS/cm", "tdi mg/L", "Meq/l")
  density_list = c("kg/m3", "g/cm3", "g/m3", "mg/m3", "lb/ft3", "lb/in3", "lb/gal", "lb/yd3", "ton/yd3")

  if (!what %in% what_list) {
    stop(paste("'what' should be one of", paste(what_list, collapse = ", ")))
  }

  if (what == what_list[1]) {
    if (!from %in% temperature_list) {
      stop(paste("'from' should be one of", paste(temperature_list, collapse = ", ")))
    }
    if (!to %in% temperature_list) {
      stop(paste("'to' should be one of", paste(temperature_list, collapse = ", ")))
    }
  }
  if (what == what_list[2]) {
    if (!from %in% pressure_list) {
      stop(paste("'from' should be one of", paste(pressure_list, collapse = ", ")))
    }
    if (!to %in% pressure_list) {
      stop(paste("'to' should be one of", paste(pressure_list, collapse = ", ")))
    }
  }

  if (what == what_list[3]) {
    if (!from %in% mass_list) {
      stop(paste("'from' should be one of", paste(mass_list, collapse = ", ")))
    }
    if (!to %in% mass_list) {
      stop(paste("'to' should be one of", paste(mass_list, collapse = ", ")))
    }
  }

  if (what == what_list[4]) {
    if (!from %in% velocity_list) {
      stop(paste("'from' should be one of", paste(velocity_list, collapse = ", ")))
    }
    if (!to %in% velocity_list) {
      stop(paste("'to' should be one of", paste(velocity_list, collapse = ", ")))
    }
  }

  if (what == what_list[5]) {
    if (!from %in% radiation_list) {
      stop(paste("'from' should be one of", paste(radiation_list, collapse = ", ")))
    }
    if (!to %in% radiation_list) {
      stop(paste("'to' should be one of", paste(radiation_list, collapse = ", ")))
    }
  }

  if (what == what_list[6]) {
    if (!from %in% daily_radiation_list) {
      stop(paste("'from' should be one of", paste(daily_radiation_list, collapse = ", ")))
    }
    if (!to %in% daily_radiation_list) {
      stop(paste("'to' should be one of", paste(daily_radiation_list, collapse = ", ")))
    }
  }

  if (what == what_list[7]) {
    if (!from %in% time_list) {
      stop(paste("'from' should be one of", paste(time_list, collapse = ", ")))
    }
    if (!to %in% time_list) {
      stop(paste("'to' should be one of", paste(time_list, collapse = ", ")))
    }
  }

  if (what == what_list[8]) {
    if (!from %in% length_list) {
      stop(paste("'from' should be one of", paste(length_list, collapse = ", ")))
    }
    if (!to %in% length_list) {
      stop(paste("'to' should be one of", paste(length_list, collapse = ", ")))
    }
  }

  if (what == what_list[9]) {
    if (!from %in% area_list) {
      stop(paste("'from' should be one of", paste(area_list, collapse = ", ")))
    }
    if (!to %in% area_list) {
      stop(paste("'to' should be one of", paste(area_list, collapse = ", ")))
    }
  }

  if (what == what_list[10]) {
    if (!from %in% volume_list) {
      stop(paste("'from' should be one of", paste(volume_list, collapse = ", ")))
    }
    if (!to %in% volume_list) {
      stop(paste("'to' should be one of", paste(volume_list, collapse = ", ")))
    }
  }

  if (what == what_list[11]) {
    if (!from %in% angle_list) {
      stop(paste("'from' should be one of", paste(angle_list, collapse = ", ")))
    }
    if (!to %in% angle_list) {
      stop(paste("'to' should be one of", paste(angle_list, collapse = ", ")))
    }
  }

  if (what == what_list[12]) {
    if (!from %in% conductivity_list) {
      stop(paste("'from' should be one of", paste(conductivity_list, collapse = ", ")))
    }
    if (!to %in% conductivity_list) {
      stop(paste("'to' should be one of", paste(conductivity_list, collapse = ", ")))
    }
  }

  if (what == what_list[13]) {
    if (!from %in% density_list) {
      stop(paste("'from' should be one of", paste(density_list, collapse = ", ")))
    }
    if (!to %in% density_list) {
      stop(paste("'to' should be one of", paste(density_list, collapse = ", ")))
    }
  }


  result = switch(
    what,
    "temperature" = switch(
      from,
      "celsius" = switch(
        to,
        "fahrenheit" = (value * 9 / 5) + 32,
        "kelvin" = value + 273.15
      ),
      "fahrenheit" = switch(
        to,
        "celsius" = (value - 32) * 5 / 9,
        "kelvin" = (value + 459.67) * 5 / 9
      ),
      "kelvin" = switch(
        to,
        "celsius" = value - 273.15,
        "fahrenheit" = (value * 9 / 5) - 459.67
      )
    ),
    "pressure" = switch(
      from,
      "atm" = switch(
        to,
        "mmHg" = value * 760,
        "kPa" = value * 101.325,
        "bar" = value * 1.01325,
        "mbar" = value * 1013.25,
        "psi" = value * 14.6959,
        "mH2O" = value * 10.3322
    ),
    "mmHg" = switch(
      to,
      "atm" = value / 760,
      "kPa" = value * 0.133322,
      "bar" = value * 0.00133322,
      "mbar" = value * 1.33322,
      "psi" = value * 0.0193368,
      "mH2O" = value * 0.0135951
    ),
    "kPa" = switch(
      to,
      "atm" = value / 101.325,
      "mmHg" = value / 0.133322,
      "bar" = value * 0.01,
      "mbar" = value * 10,
      "psi" = value * 0.145038,
      "mH2O" = value * 0.101972
    ),
    "bar" = switch(
      to,
      "atm" = value / 1.01325,
      "mmHg" = value / 1.33322,
      "kPa" = value * 100,
      "mbar" = value * 1000,
      "psi" = value * 14.5038,
      "mH2O" = value * 10.1972
    ),
    "mbar" = switch(
      to,
      "atm" = value / 1013.25,
      "mmHg" = value / 1.33322,
      "kPa" = value * 0.1,
      "bar" = value * 0.001,
      "psi" = value * 0.0145038,
      "mH2O" = value * 0.0101972
    ),
    "psi" = switch(
      to,
      "atm" = value / 14.6959,
      "mmHg" = value / 0.0193368,
      "kPa" = value / 0.145038,
      "bar" = value / 14.5038,
      "mbar" = value / 0.0145038,
      "mH2O" = value * 0.703069
    ),
    "mH2O" = switch(
      to,
      "atm" = value / 10.3322,
      "mmHg" = value / 0.0135951,
      "kPa" = value / 0.101972,
      "bar" = value / 10.1972,
      "mbar" = value / 0.0101972,
      "psi" = value / 0.703069
    )
  ),
    "mass" = switch(
      from,
      "kg" = switch(
        to,
        "g" = value * 1000,
        "mg" = value * 1e6,
        "lb" = value * 2.20462,
        "oz" = value * 35.274
      ),
      "g" = switch(
        to,
        "kg" = value / 1000,
        "mg" = value * 1000,
        "lb" = value * 0.00220462,
        "oz" = value * 0.035274
      ),
      "mg" = switch(
        to,
        "kg" = value / 1e6,
        "g" = value / 1000,
        "lb" = value * 2.20462e-6,
        "oz" = value * 3.5274e-5
      ),
      "lb" = switch(
        to,
        "kg" = value * 0.453592,
        "g" = value * 453.592,
        "mg" = value * 453592,
        "oz" = value * 16
      ),
      "oz" = switch(
        to,
        "kg" = value * 0.0283495,
        "g" = value * 28.3495,
        "mg" = value * 28349.5,
        "lb" = value / 16
      )
    ),
    "velocity" = switch(
      from,
      "km/h" = switch(to,
                      "m/s" = value / 3.6,
                      "mph" = value / 1.60934),
      "m/s" = switch(to,
                     "km/h" = value * 3.6,
                     "mph" = value * 2.23694),
      "mph" = switch(to,
                     "km/h" = value * 1.60934,
                     "m/s" = value / 2.23694)
    ),
    "radiation" = switch(
      from,
      "W/m2" = switch(
        to,
        "cal/cm2" = value * 0.000239006,
        "MJ/m2" = value * 0.0000036
      ),
      "cal/cm2" = switch(to,
                         "W/m2" = value * 4184,
                         "MJ/m2" = value * 0.014418),
      "MJ/m2" = switch(to,
                       "W/m2" = value * 277.778,
                       "cal/cm2" = value * 69.419)
    ),
    "daily radiation" = switch(
      from,
      "MJ/m2/day" = switch(
        to,
        "W/m2/day" = value * 11.574,
        "cal/cm2/day" = value * 0.041868
      ),
      "W/m2/day" = switch(
        to,
        "MJ/m2/day" = value * 0.0864,
        "cal/cm2/day" = value * 0.0036
      ),
      "cal/cm2/day" = switch(
        to,
        "MJ/m2/day" = value * 0.697,
        "W/m2/day" = value * 277.778
      )
    ),
    "time" = switch(
      from,
      "s" = switch(
        to,
        "min" = value / 60,
        "h" = value / 3600,
        "d" = value / 86400
      ),
      "min" = switch(
        to,
        "s" = value * 60,
        "h" = value / 60,
        "d" = value / 1440
      ),
      "h" = switch(
        to,
        "s" = value * 3600,
        "min" = value * 60,
        "d" = value / 24
      ),
      "d" = switch(
        to,
        "s" = value * 86400,
        "min" = value * 1440,
        "h" = value * 24
      )
    ),
    "length" = switch(
      from,
      "m" = switch(
        to,
        "cm" = value * 100,
        "mm" = value * 1000,
        "km" = value / 1000,
        "in" = value * 39.3701,
        "ft" = value * 3.28084,
        "yd" = value * 1.09361,
        "mi" = value / 1609.34
      ),
      "cm" = switch(
        to,
        "m" = value / 100,
        "mm" = value * 10,
        "km" = value / 100000,
        "in" = value * 0.393701,
        "ft" = value * 0.0328084,
        "yd" = value * 0.0109361,
        "mi" = value / 160934
      ),
      "mm" = switch(
        to,
        "m" = value / 1000,
        "cm" = value / 10,
        "km" = value / 1e6,
        "in" = value * 0.0393701,
        "ft" = value * 0.00328084,
        "yd" = value * 0.00109361,
        "mi" = value / 1.609e6
      ),
      "km" = switch(
        to,
        "m" = value * 1000,
        "cm" = value * 100000,
        "mm" = value * 1e6,
        "in" = value * 39370.1,
        "ft" = value * 3280.84,
        "yd" = value * 1093.61,
        "mi" = value / 1.60934
      ),
      "in" = switch(
        to,
        "m" = value * 0.0254,
        "cm" = value * 2.54,
        "mm" = value * 25.4,
        "km" = value / 39370.1,
        "ft" = value * 0.0833333,
        "yd" = value * 0.0277778,
        "mi" = value / 63360
      ),
      "ft" = switch(
        to,
        "m" = value * 0.3048,
        "cm" = value * 30.48,
        "mm" = value * 304.8,
        "km" = value / 3280.84,
        "in" = value * 12,
        "yd" = value * 0.333333,
        "mi" = value / 5280
      ),
      "yd" = switch(
        to,
        "m" = value * 0.9144,
        "cm" = value * 91.44,
        "mm" = value * 914.4,
        "km" = value / 1093.61,
        "in" = value * 36,
        "ft" = value * 3,
        "mi" = value / 1760
      ),
      "mi" = switch(
        to,
        "m" = value * 1609.34,
        "cm" = value * 160934,
        "mm" = value * 1.609e6,
        "km" = value * 1.60934,
        "in" = value * 63360,
        "ft" = value * 5280,
        "yd" = value * 1760
      )
    ),
    "area" = switch(
      from,
      "m2" = switch(
        to,
        "cm2" = value * 10000,
        "mm2" = value * 1e6,
        "km2" = value / 1e6,
        "in2" = value * 1550,
        "ft2" = value * 10.7639,
        "yd2" = value * 1.19599,
        "mi2" = value / 2.59e6
      ),
      "cm2" = switch(
        to,
        "m2" = value / 10000,
        "mm2" = value * 100,
        "km2" = value / 1e8,
        "in2" = value / 6.4516,
        "ft2" = value / 929.03,
        "yd2" = value / 8361.27,
        "mi2" = value / 2.59e10
      ),
      "mm2" = switch(
        to,
        "m2" = value / 1e6,
        "cm2" = value / 100,
        "km2" = value / 1e12,
        "in2" = value / 645.16,
        "ft2" = value / 92903,
        "yd2" = value / 836127,
        "mi2" = value / 2.59e12
      ),
      "km2" = switch(
        to,
        "m2" = value * 1e6,
        "cm2" = value * 1e8,
        "mm2" = value * 1e12,
        "in2" = value * 1.55e9,
        "ft2" = value * 1.076e7,
        "yd2" = value * 1.196e6,
        "mi2" = value / 2.59
      ),
      "in2" = switch(
        to,
        "m2" = value / 1550,
        "cm2" = value * 6.4516,
        "mm2" = value * 645.16,
        "km2" = value / 1.55e9,
        "ft2" = value / 144,
        "yd2" = value / 1296,
        "mi2" = value / 4e6
      ),
      "ft2" = switch(
        to,
        "m2" = value / 10.7639,
        "cm2" = value * 929.03,
        "mm2" = value * 92903,
        "km2" = value / 1.076e7,
        "in2" = value * 144,
        "yd2" = value / 9,
        "mi2" = value / 2.788e7
      ),
      "yd2" = switch(
        to,
        "m2" = value / 1.19599,
        "cm2" = value * 8361.27,
        "mm2" = value * 836127,
        "km2" = value / 1.196e6,
        "in2" = value * 1296,
        "ft2" = value * 9,
        "mi2" = value / 3.098e6
      ),
      "mi2" = switch(
        to,
        "m2" = value * 2.59e6,
        "cm2" = value * 2.59e10,
        "mm2" = value * 2.59e12,
        "km2" = value * 2.59,
        "in2" = value * 4e6,
        "ft2" = value * 2.788e7,
        "yd2" = value * 3.098e6
      )
    ),
    "volume" = switch(
      from,
      "m3" = switch(
        to,
        "cm3" = value * 1e6,
        "mm3" = value * 1e9,
        "l" = value * 1000,
        "in3" = value * 61024,
        "ft3" = value * 35.3147,
        "yd3" = value * 1.30795,
        "gal" = value * 264.172
      ),
      "cm3" = switch(
        to,
        "m3" = value / 1e6,
        "mm3" = value * 1000,
        "l" = value / 1000,
        "in3" = value / 16.3871,
        "ft3" = value / 28316.8,
        "yd3" = value / 764554.9,
        "gal" = value / 3785.41
      ),
      "mm3" = switch(
        to,
        "m3" = value / 1e9,
        "cm3" = value / 1000,
        "l" = value / 1e6,
        "in3" = value / 16387.1,
        "ft3" = value / 2.8317e7,
        "yd3" = value / 7.6455e9,
        "gal" = value / 3.7854e6
      ),
      "l" = switch(
        to,
        "m3" = value / 1000,
        "cm3" = value * 1000,
        "mm3" = value * 1e6,
        "in3" = value * 61.024,
        "ft3" = value / 28.3168,
        "yd3" = value / 764.555,
        "gal" = value / 3.78541
      ),
      "in3" = switch(
        to,
        "m3" = value / 61024,
        "cm3" = value * 16.3871,
        "mm3" = value * 16387.1,
        "l" = value / 61.024,
        "ft3" = value / 1728,
        "yd3" = value / 46656,
        "gal" = value / 231
      ),
      "ft3" = switch(
        to,
        "m3" = value / 35.3147,
        "cm3" = value * 28316.8,
        "mm3" = value * 2.8317e7,
        "l" = value * 28.3168,
        "in3" = value * 1728,
        "yd3" = value / 27,
        "gal" = value * 7.48052
      ),
      "yd3" = switch(
        to,
        "m3" = value / 1.30795,
        "cm3" = value * 764554.9,
        "mm3" = value * 7.6455e9,
        "l" = value * 764.555,
        "in3" = value * 46656,
        "ft3" = value * 27,
        "gal" = value * 201.974
      ),
      "gal" = switch(
        to,
        "m3" = value / 264.172,
        "cm3" = value * 3785.41,
        "mm3" = value * 3.7854e6,
        "l" = value * 3.78541,
        "in3" = value * 231,
        "ft3" = value / 7.48052,
        "yd3" = value / 201.974
      )
    ),
    "angle" = switch(
      from,
      "deg" = switch(
        to,
        "rad" = value * pi / 180,
        "grad" = value * 10 / 9,
        "turn" = value / 360
      ),
      "rad" = switch(
        to,
        "deg" = value * 180 / pi,
        "grad" = value * 200 / pi,
        "turn" = value / (2 * pi)
      ),
      "grad" = switch(
        to,
        "deg" = value * 9 / 10,
        "rad" = value * pi / 200,
        "turn" = value / 400
      ),
      "turn" = switch(
        to,
        "deg" = value * 360,
        "rad" = value * 2 * pi,
        "grad" = value * 400
      )
    ),
    "conductivity" =
      switch(
        from,
        "dS/m" = switch(
          to,
          "mS/m" = value * 100,
          "uS/m" = value * 100000,
          "mS/cm" = value,
          "uS/cm" = value * 1000,
          "tdi mg/L" = value * 667,
          "Meq/l" = value * 10
        ),
        "mS/m" = switch(
          to,
          "dS/m" = value / 100,
          "uS/m" = value * 1000,
          "mS/cm" = value / 100,
          "uS/cm" = value * 10,
          "tdi mg/L" = value * 6.67,
          "Meq/l" = value / 10
        ),
        "uS/m" = switch(
          to,
          "dS/m" = value / 100000,
          "mS/m" = value / 1000,
          "mS/cm" = value / 100000,
          "uS/cm" = value * 0.01,
          "tdi mg/L" = value * 0.00667,
          "Meq/l" = value / 10000
        ),
        "mS/cm" = switch(
          to,
          "dS/m" = value,
          "mS/m" = value * 100,
          "uS/m" = value * 100000,
          "uS/cm" = value * 1000,
          "tdi mg/L" = value * 667,
          "Meq/l" = value * 10
        ),
        "uS/cm" = switch(
          to,
          "dS/m" = value / 1000,
          "mS/m" = value / 10,
          "uS/m" = value * 100,
          "mS/cm" = value / 1000,
          "tdi mg/L" = value * 0.667,
          "Meq/l" = value / 100
        ),
        "tdi mg/L" = switch(
          to,
          "dS/m" = value / 667,
          "mS/m" = value / 6.67,
          "uS/m" = value / 0.00667,
          "mS/cm" = value / 667,
          "uS/cm" = value / 0.667,
          "Meq/l" = value / 66.7,
        ),
        "Meq/l" = switch(
          to,
          "dS/m" = value / 10,
          "mS/m" = value * 10,
          "uS/m" = value * 10000,
          "mS/cm" = value * 0.1,
          "uS/cm" = value * 100,
          "tdi mg/L" = value * 66.7
        )
      ),
    "density" = switch(
      from,
      "kg/m3" = switch(
        to,
        "g/cm3" = value / 1000,
        "g/m3" = value * 1000,
        "mg/m3" = value * 1e6,
        "lb/ft3" = value / 16.0185,
        "lb/in3" = value / 27679.9,
        "lb/gal" = value / 119.826,
        "lb/yd3" = value / 0.478,
        "ton/yd3" = value / 119.826
      ),
      "g/cm3" = switch(
        to,
        "kg/m3" = value * 1000,
        "g/m3" = value * 1e6,
        "mg/m3" = value * 1e9,
        "lb/ft3" = value / 0.0160185,
        "lb/in3" = value / 27.6799,
        "lb/gal" = value / 0.119826,
        "lb/yd3" = value * 0.478,
        "ton/yd3" = value / 0.119826
      ),
      "g/m3" = switch(
        to,
        "kg/m3" = value / 1000,
        "g/cm3" = value / 1e6,
        "mg/m3" = value * 1000,
        "lb/ft3" = value / 16018.5,
        "lb/in3" = value / 27679900,
        "lb/gal" = value / 119826,
        "lb/yd3" = value / 0.478,
        "ton/yd3" = value / 119826
      ),
      "mg/m3" = switch(
        to,
        "kg/m3" = value / 1e6,
        "g/cm3" = value / 1e9,
        "g/m3" = value / 1000,
        "lb/ft3" = value / 16018500,
        "lb/in3" = value / 27679900000,
        "lb/gal" = value / 119826000,
        "lb/yd3" = value / 478,
        "ton/yd3" = value / 119826000
      ),
      "lb/ft3" = switch(
        to,
        "kg/m3" = value * 16.0185,
        "g/cm3" = value * 0.0160185,
        "g/m3" = value * 16018.5,
        "mg/m3" = value * 16018500,
        "lb/in3" = value / 1728,
        "lb/gal" = value / 7.48052,
        "lb/yd3" = value / 27,
        "ton/yd3" = value / 1504.56
      ),
      "lb/in3" = switch(
        to,
        "kg/m3" = value * 27679.9,
        "g/cm3" = value * 0.0276799,
        "g/m3" = value * 27679.9,
        "mg/m3" = value * 27679900,
        "lb/ft3" = value * 1728,
        "lb/gal" = value / 231,
        "lb/yd3" = value / 46656,
        "ton/yd3" = value / 2767999
      ),
      "lb/gal" = switch(
        to,
        "kg/m3" = value * 119.826,
        "g/cm3" = value * 0.119826,
        "g/m3" = value * 119826,
        "mg/m3" = value * 119826000,
        "lb/ft3" = value * 7.48052,
        "lb/in3" = value * 231,
        "lb/yd3" = value / 201.974,
        "ton/yd3" = value / 119826
      ),
      "lb/yd3" = switch(
        to,
        "kg/m3" = value * 0.478,
        "g/cm3" = value * 0.000478,
        "g/m3" = value * 478,
        "mg/m3" = value * 478000,
        "lb/ft3" = value * 27,
        "lb/in3" = value * 46656,
        "lb/gal" = value * 201.974,
        "ton/yd3" = value / 2000
      ),
      "ton/yd3" = switch(
        to,
        "kg/m3" = value * 119.826,
        "g/cm3" = value * 0.119826,
        "g/m3" = value * 119826,
        "mg/m3" = value * 119826000,
        "lb/ft3" = value * 1504.56,
        "lb/in3" = value * 2767999,
        "lb/gal" = value * 119826,
        "lb/yd3" = value * 2000
      )
    )
  )
  return(result)
}
