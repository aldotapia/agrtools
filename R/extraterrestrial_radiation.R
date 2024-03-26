#' Calculate the extraterrestrial radiation.
#'
#' This function computes the extraterrestrial radiation using the formula proposed by Allen et al. (1998).
#'
#' @param j numeric. Day of the year.
#' @param lat numeric. Latitude in radians.
#'
#' @return A numeric value of the clear sky radiation in MJ/m2/day.
#'
#' @examples
#' # date='2024-03-09'
#' # lat=-30.631261
#'
#' lat = convert_units(-30.631261, "angle", "deg", "rad")
#' extraterrestrial_radiation(j=69, lat=lat)
#'
#' @export
extraterrestrial_radiation = function(j, lat){
  dr = 1 + 0.033 * cos(2 * pi * j / 365)
  delta_s = 0.409 * sin((2 * pi * j / 365) - 1.39)
  ws = acos(-tan(lat) * tan(delta_s))
  ra = (24 * 60 / pi) * 0.0820 * dr * (ws * sin(lat) * sin(delta_s) +
                                         cos(lat) * cos(delta_s) * sin(ws))
  return(ra)
}
