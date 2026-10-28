#' @title Compute reference evapotranspiration using Penman-Monteith method
#'
#' @description
#' Compute daily reference evapotranspiration using Penman-Monteith method.
#' This function also allows different units for the input variables. For
#' unit conversion see `convert_units()` function.
#'
#' @param date character or Date object. Date of the observation.
#' @param lat numeric. Latitude of the location in decimal degrees by default.
#' @param elev numeric. Elevation of the location in meters by default.
#' @param t_max numeric. Maximum temperature in Celsius by default.
#' @param t_min numeric. Minimum temperature in Celsius by default.
#' @param u_h numeric. Wind speed at 2 meters in m/s by default.
#' @param r_s numeric. Solar radiation in W/m2 (daily mean irradiance) by default.
#' @param rh_max numeric. Maximum relative humidity in % by default.
#' @param rh_min numeric. Minimum relative humidity in % by default.
#' @param rh_mean numeric. Mean relative humidity in % by default.
#' @param h numeric. Height of the wind speed measurement in meters by default.
#' @param lat_u character. Unit of the latitude. Default is "deg".
#' @param elev_u character. Unit of the elevation. Default is "m".
#' @param t_max_u character. Unit of the maximum temperature. Default is "celsius".
#' @param t_min_u character. Unit of the minimum temperature. Default is "celsius".
#' @param u_h_u character. Unit of the wind speed. Default is "m/s".
#' @param r_s_u character. Unit of the solar radiation. Default is "W/m2/day".
#' @param rh_max_u character. Unit of the maximum relative humidity. Default is "%" ("%" or "fraction").
#' @param rh_min_u character. Unit of the minimum relative humidity. Default is "%" ("%" or "fraction").
#' @param rh_mean_u character. Unit of the mean relative humidity. Default is "%" ("%" or "fraction").
#' @param h_u character. Unit of the height of the wind speed measurement. Default is "m".
#'
#' @return A numeric value of the reference evapotranspiration in mm/day.
#'
#' @details
#' Calculate the reference evapotranspiration (ETo) using the FAO-56 Penman-Monteith method. This functions requires latitude, elevation, date, maximum temperature, minimum temperature, mean wind speed and mean solar radiation. There are 4 different methods to calculate actual vapor pressure, which are:
#' - 1st method: using maximum and minimum Relative Humidity (%).
#' - 2nd method: using maximum Relative Humidity (%).
#' - 3rd method: using mean Relative Humidity (%).
#' - 4th method: using minimum temperature, asssuming when temperature is close to t_min, the air is (or close to) saturated.
#'
#' @examples
#' et0pm(date='2024-03-09',lat=-30.631261,elev=80,t_min=7.91,
#'       t_max=27.69,rh_min=37.71,rh_max=93.2,
#'       u_h=1.364,r_s=272.824)
#'
#' et0pm(date='2024-03-09',lat=-30.631261,elev=80,t_min=281.06,
#'       t_max=300.84,rh_min=37.71,rh_max=93.2,
#'       u_h=1.364,r_s=272.824, t_min_u = "kelvin",
#'       t_max_u = "kelvin")
#'
#' @export
et0pm = function(date,
                 lat,
                 elev,
                 t_max,
                 t_min,
                 u_h,
                 r_s,
                 rh_max = NULL,
                 rh_min = NULL,
                 rh_mean = NULL,
                 h = 2,
                 # time to define units
                 lat_u = "deg",
                 elev_u = "m",
                 t_max_u = "celsius",
                 t_min_u = "celsius",
                 u_h_u = "m/s",
                 r_s_u = "W/m2/day",
                 rh_max_u = "%",
                 rh_min_u = "%",
                 rh_mean_u = "%",
                 h_u = "m"
                 ) {
  if (!all(sapply(list(lat, elev, t_max, t_min,  u_h, r_s), is.numeric))) {
    stop("lat, elev, t_max, t_min, u_h, r_s should be numeric")
  }
  date = as_date_vec(date)

  # convert units
  if(lat_u != "rad"){
    tryCatch({
      lat = convert_units(lat, "angle", lat_u, "rad")
    }, error = function(e) {
      stop("lat_u unit is not valid")
    })
  }
  if(elev_u != "m"){
    tryCatch({
      elev = convert_units(elev, "length", elev_u, "m")
    }, error = function(e) {
      stop("elev_u unit is not valid")
    })
  }
  if(t_max_u != "celsius"){
    tryCatch({
      t_max = convert_units(t_max, "temperature", t_max_u, "celsius")
    }, error = function(e) {
      stop("t_max_u unit is not valid")
    })
  }
  if(t_min_u != "celsius"){
    tryCatch({
      t_min = convert_units(t_min, "temperature", t_min_u, "celsius")
    }, error = function(e) {
      stop("t_min_u unit is not valid")
    })
  }
  if(u_h_u != "m/s"){
    tryCatch({
      u_h = convert_units(u_h, "velocity", u_h_u, "m/s")
    }, error = function(e) {
      stop("u_h_u unit is not valid")
    })
  }
  if(r_s_u != "MJ/m2/day"){
    tryCatch({
      r_s = convert_units(r_s, "daily radiation", r_s_u, "MJ/m2/day")
    }, error = function(e) {
      stop("r_s_u unit is not valid")
    })
  }
  if(h_u != "m"){
    tryCatch({
      h = convert_units(h, "length", h_u, "m")
    }, error = function(e) {
      stop("h_u unit is not valid")
    })
  }

  for (rh in list(rh_max, rh_min, rh_mean)) {
    if (!is.null(rh) && !is.numeric(rh)) {
      stop("rh_max, rh_min and rh_mean should be numeric")
    }
  }

  rh_units = c("%", "fraction")
  if (!all(c(rh_max_u, rh_min_u, rh_mean_u) %in% rh_units)) {
    stop("rh units should be one of %, fraction")
  }
  if (!is.null(rh_max) && rh_max_u == "fraction") rh_max = rh_max * 100
  if (!is.null(rh_min) && rh_min_u == "fraction") rh_min = rh_min * 100
  if (!is.null(rh_mean) && rh_mean_u == "fraction") rh_mean = rh_mean * 100

  t_mean = (t_max + t_min) / 2
  u2 = wind_at_2m(u_h, h)
  delta = slope_vapor_pressure(t_mean)
  gamma = psychrometric_constant(atm_pressure(elev))
  e_s = (sat_vapor_pressure(t_max) + sat_vapor_pressure(t_min)) / 2
  ea = actual_vapor_pressure(t_max, t_min, rh_max, rh_min, rh_mean)
  j = as.numeric(format(date, format = "%j"))
  rso = clearsky_radiation(extraterrestrial_radiation(j, lat), elev)
  rn = net_radiation(r_s, rso, t_max, t_min, ea)

  denom = delta + gamma * (1 + 0.34 * u2)
  etrad = delta / denom * 0.408 * rn
  etwind = gamma / denom * (900 / (t_mean + 273)) * u2 * (e_s - ea)
  et0 = etrad + etwind
  return(et0)
}
