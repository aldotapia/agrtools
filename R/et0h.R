#' @title Compute reference evapotranspiration using Hargreaves method
#'
#' @description
#' Compute daily reference evapotranspiration using Hargreaves method.
#' This function also allows different units for the input variables. For
#' unit conversion see `convert_units()` function.
#'
#' @param date character or Date object. Date of the observation.
#' @param t_max numeric. Maximum temperature in Celsius by default.
#' @param t_min numeric. Minimum temperature in Celsius by default.
#' @param lat numeric. Latitude of the location in decimal degrees by default.
#' @param t_max_u character. Unit of the maximum temperature. Default is "celsius".
#' @param t_min_u character. Unit of the minimum temperature. Default is "celsius".
#' @param lat_u character. Unit of the latitude. Default is "deg".
#'
#' @return A numeric value of the reference evapotranspiration in mm/day.
#'
#' @examples
#' et0h(date='2024-03-09',t_max=27.69,t_min=7.91,lat=-30.631261)
#'
#' @export
et0h = function(date,
                t_max,
                t_min,
                lat,
                t_max_u = "celsius",
                t_min_u = "celsius",
                lat_u = "rad"){
  if (!all(sapply(list(lat, t_max, t_min), is.numeric))) {
    stop("lat, t_max, t_min should be numeric")
  }
  if (!lubridate::is.Date(date)) {
    if (is.character(date)) {
      if (grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = date)) {
        date = as.Date(date)
      } else{
        stop("date should be in YYYY-MM-DD format")
      }
    }
  }
  if(lat_u != "rad"){
    tryCatch({
      lat = convert_units(lat, "angle", lat_u, "rad")
    }, error = function(e) {
      stop("lat_u unit is not valid")
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

  t_mean = (t_max + t_min) / 2
  j = as.numeric(format(date, format = "%j"))

  et0 = 0.0135 * extraterrestrial_radiation(j, lat) * 0.408  * (t_mean + 17.8)
  return(et0)
}
