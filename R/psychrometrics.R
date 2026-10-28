#' Psychrometric and atmospheric helpers (FAO-56)
#'
#' Building blocks of the FAO-56 Penman-Monteith equation (Allen et al., 1998).
#' Inputs are in FAO-56 units: temperature in Celsius, relative humidity in %,
#' elevation and height in m, wind speed in m/s, pressure in kPa. Use
#' `convert_units()` for other units.
#'
#' @param t numeric. Air temperature (Celsius).
#' @param t_max numeric. Maximum temperature (Celsius).
#' @param t_min numeric. Minimum temperature (Celsius).
#' @param rh numeric. Relative humidity (%).
#' @param rh_max numeric or NULL. Maximum relative humidity (%).
#' @param rh_min numeric or NULL. Minimum relative humidity (%).
#' @param rh_mean numeric or NULL. Mean relative humidity (%).
#' @param elev numeric. Elevation above sea level (m).
#' @param p numeric. Atmospheric pressure (kPa).
#' @param u_h numeric. Wind speed measured at height `h` (m/s).
#' @param h numeric. Height of the wind measurement (m).
#'
#' @details
#' - `sat_vapor_pressure()`: saturation vapour pressure, e°(T) (eq. 11).
#' - `slope_vapor_pressure()`: slope of the saturation vapour pressure curve (eq. 13).
#' - `actual_vapor_pressure()`: actual vapour pressure from RH max & min
#'   (eq. 17), RH max only (eq. 18), RH mean (eq. 19), or, with no RH, from
#'   `t_min` as dew point (eq. 48).
#' - `vpd()`: vapour pressure deficit, es - ea, with es the mean of e°(t_max)
#'   and e°(t_min) (eq. 12).
#' - `dew_point()`: dew point temperature, the inverse of eq. 11 applied to e°(t) * rh / 100.
#' - `atm_pressure()`: atmospheric pressure from elevation (eq. 7).
#' - `psychrometric_constant()`: psychrometric constant (eq. 8).
#' - `wind_at_2m()`: wind speed adjusted to 2 m with the logarithmic profile (eq. 47).
#'
#' @return numeric. Pressures in kPa, slope in kPa/ºC, psychrometric constant
#'   in kPa/ºC, dew point in Celsius, wind speed in m/s.
#'
#' @examples
#' sat_vapor_pressure(24.5)
#' actual_vapor_pressure(t_max = 25, t_min = 18, rh_max = 82, rh_min = 54)
#' vpd(t_max = 25, t_min = 18, rh_max = 82, rh_min = 54)
#' dew_point(20, 60)
#' psychrometric_constant(atm_pressure(1800))
#' wind_at_2m(3.2, h = 10)
#'
#' @name psychrometrics
NULL

#' @rdname psychrometrics
#' @export
sat_vapor_pressure = function(t) {
  0.6108 * exp((17.27 * t) / (t + 237.3))
}

#' @rdname psychrometrics
#' @export
slope_vapor_pressure = function(t) {
  4098 * sat_vapor_pressure(t) / ((t + 237.3)^2)
}

#' @rdname psychrometrics
#' @export
actual_vapor_pressure = function(t_max, t_min, rh_max = NULL, rh_min = NULL,
                                 rh_mean = NULL) {
  e_tmax = sat_vapor_pressure(t_max)
  e_tmin = sat_vapor_pressure(t_min)
  if (!is.null(rh_max) && !is.null(rh_min)) {
    return((e_tmin * rh_max / 100 + e_tmax * rh_min / 100) / 2)
  }
  if (!is.null(rh_max)) {
    return(e_tmin * rh_max / 100)
  }
  if (!is.null(rh_min)) {
    stop("rh_min alone is not enough, also give rh_max, or use rh_mean")
  }
  if (!is.null(rh_mean)) {
    return(rh_mean / 100 * (e_tmax + e_tmin) / 2)
  }
  e_tmin
}

#' @rdname psychrometrics
#' @export
vpd = function(t_max, t_min, rh_max = NULL, rh_min = NULL, rh_mean = NULL) {
  es = (sat_vapor_pressure(t_max) + sat_vapor_pressure(t_min)) / 2
  es - actual_vapor_pressure(t_max, t_min, rh_max, rh_min, rh_mean)
}

#' @rdname psychrometrics
#' @export
dew_point = function(t, rh) {
  x = log(sat_vapor_pressure(t) * rh / 100 / 0.6108)
  237.3 * x / (17.27 - x)
}

#' @rdname psychrometrics
#' @export
atm_pressure = function(elev) {
  101.3 * ((293 - 0.0065 * elev) / 293)^5.26
}

#' @rdname psychrometrics
#' @export
psychrometric_constant = function(p) {
  0.000665 * p
}

#' @rdname psychrometrics
#' @export
wind_at_2m = function(u_h, h = 2) {
  u_h * 4.87 / log(67.8 * h - 5.42)
}

#' Net radiation (FAO-56)
#'
#' Daily net radiation as net shortwave minus net longwave radiation
#' (Allen et al., 1998, eqs. 38-40).
#'
#' @param r_s numeric. Measured solar radiation (MJ/m2/day).
#' @param rso numeric. Clear sky radiation (MJ/m2/day), see `clearsky_radiation()`.
#' @param t_max numeric. Maximum temperature (Celsius).
#' @param t_min numeric. Minimum temperature (Celsius).
#' @param ea numeric. Actual vapour pressure (kPa), see `actual_vapor_pressure()`.
#' @param albedo numeric. Surface albedo. Default is 0.23 (grass reference).
#'
#' @return numeric. Net radiation in MJ/m2/day.
#'
#' @examples
#' # FAO-56 examples 11 and 12: about 7.6 MJ/m2/day
#' net_radiation(r_s = 14.5, rso = 18.8, t_max = 25.1, t_min = 19.1, ea = 2.1)
#'
#' @export
net_radiation = function(r_s, rso, t_max, t_min, ea, albedo = 0.23) {
  rns = (1 - albedo) * r_s
  rnl = 4.903e-9 * ((t_max + 273.16)^4 + (t_min + 273.16)^4) / 2 *
    (0.34 - 0.14 * sqrt(ea)) * (1.35 * pmin(r_s / rso, 1) - 0.35)
  rns - rnl
}
