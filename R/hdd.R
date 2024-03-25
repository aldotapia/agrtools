#' Heating Degree Days (HDD) calculation
#'
#' This function calculates the Heating Degree Days (HDD) based on the mean temperature. The unit can be in Celsius, Fahrenheit, or Kelvin.
#'
#' @param t_max numeric, maximum temperature
#' @param t_min numeric, minimum temperature
#' @param t_mean numeric, mean temperature
#' @param base numeric, base temperature
#' @param tunit character, temperature unit
#' @param lunit character, length unit
#'
#' @return numeric, Heating Degree Days
#'
#' @details
#' The Heating Degree Days (HDD) is a measure of how much (in degrees), and for how long (in days), outside air temperature was lower than a specific base temperature. The base temperature is typically 18.33 degrees Celsius (65 degrees Fahrenheit).
#' The original formula is calculating t_mean with t_max and t_min, but t_mean option is added in the case you have only t_mean.
#'
#' @examples
#' hdd(t_max = 20, t_min = 10, tunit = 'celsius', lunit = 'celsius')
#' hdd(t_max = 30, t_min = 15, tunit = 'celsius', lunit = 'celsius')
#' hdd(t_max = 35, t_min = 18, tunit = 'celsius', lunit = 'celsius')
#'
#' @export
hdd = function(t_max = NULL,
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
    hdd = t_mean - base
    hdd[hdd < 0] = 0
  }
  return(round(hdd, 0))
}
