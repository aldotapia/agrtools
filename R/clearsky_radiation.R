#' Calculate the clear sky radiation.
#'
#' This function computes the clear sky radiation using the formula proposed by Allen et al. (1998).
#'
#' @param j numeric. Day of the year.
#' @param lat numeric. Latitude in radians.
#' @param elev numeric. Elevation in meters.
#'
#' @return A numeric value of the clear sky radiation in W/m2/day.
#'
#' @examples
#' # date='2024-03-09'
#' # lat=-30.631261
#' # elev=80
#'
#' lat = convert_units(-30.631261, "angle", "deg", "rad")
#' clearsky_radiation(j=69, lat=lat, elev=80)
#'
#' @export
clearsky_radiation = function(j, lat, elev){
  dr = 1 + 0.033 * cos(2 * pi * j / 365)
  delta_s = 0.409 * sin((2 * pi * j / 365) - 1.39)
  ws = acos(-tan(lat) * tan(delta_s))
  ra = (24 * 60 / pi) * 0.0820 * dr * (ws * sin(lat) * sin(delta_s) +
                                         cos(lat) * cos(delta_s) * sin(ws))
  rso = (0.75 + 0.00002 * elev) * ra
  return(rso)
}
