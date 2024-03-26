#' Calculate the clear sky radiation.
#'
#' This function computes the clear sky radiation using the formula proposed by Allen et al. (1998).
#'
#' @param ra numeric. Extraterrestrial radiation in MJ/m2/day.
#' @param elev numeric. Elevation in meters.
#'
#' @return A numeric value of the clear sky radiation in MJ/m2/day.
#'
#' @examples
#' clearsky_radiation(ra=35.16739, elev=80)
#'
#' @export
clearsky_radiation = function(ra, elev){
  rso = (0.75 + 0.00002 * elev) * ra
  return(rso)
}
