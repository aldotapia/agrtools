#' Cooling Degree Days (HDD) calculation
#'
#' This function calculates the Cooling Degree Days (HDD) based on the mean temperature. The unit can be in Celsius, Fahrenheit, or Kelvin.
#'
#' @param t_max numeric, maximum temperature
#' @param t_min numeric, minimum temperature
#' @param t_mean numeric, mean temperature
#' @param base numeric, base temperature
#' @param tunit character, temperature unit
#' @param lunit character, length unit
#'
#' @return numeric, Cooling Degree Days
#'
#' @details
#' The Cooling Degree Days (HDD) is a measure of how much (in degrees), and for how long (in days), outside air temperature was higher than a specific base temperature. The base temperature is the temperature below which the building needs to be heated. The HDD is calculated as the difference between the mean temperature and the base temperature. If the mean temperature is lower than the base temperature, the HDD is set to zero.
#' The original formula is calculating t_mean with t_max and t_min, but t_mean option is added in the case you have only t_mean.
#'
#' @examples
#' cdd(t_max = 20, t_min = 10, tunit = 'celsius', lunit = 'celsius')
#' cdd(t_max = 30, t_min = 15, tunit = 'celsius', lunit = 'celsius')
#' cdd(t_max = 35, t_min = 18, tunit = 'celsius', lunit = 'celsius')
#'
#' @export
cdd = function(t_max = NULL,
               t_min = NULL,
               t_mean = NULL,
               base = 18.33,
               tunit = 'celsius',
               lunit = 'celsius') {
  temperature_list = c("celsius", "fahrenheit", "kelvin")
  if (!tunit %in% temperature_list) {
    stop("tunit should be one of celsius, fahrenheit, kelvin")
  }
  if (!lunit %in% temperature_list) {
    stop("lunit should be one of celsius, fahrenheit, kelvin")
  }
  if (tunit != lunit) {
    stop("tunit and lunit should be the same")
  }
  if(is.null(t_max) & is.null(t_mean) & is.null(t_min)) {
    stop("At least t_mean or t_max & t_min should be provided")
  }else{
    if(!is.null(t_max) & !is.null(t_mean)){
      stop("t_max and t_mean cannot be provided at the same time")
    }
    if(!is.null(t_min) & !is.null(t_mean)){
      stop("t_min and t_mean cannot be provided at the same time")
    }
    if(!is.null(t_max) & !is.null(t_min)){
      t_mean = (t_max + t_min) / 2
    }
    cdd = t_mean - base
    cdd[cdd < 0] = 0
  }
  return(round(cdd, 0))
}
