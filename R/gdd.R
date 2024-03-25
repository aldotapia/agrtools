#' Growing Degree Days (HDD) calculation
#'
#' This function calculates the Growing Degree Days (HDD) based on the mean temperature. The unit can be in Celsius, Fahrenheit, or Kelvin.
#'
#' @param t_max numeric, maximum temperature
#' @param t_min numeric, minimum temperature
#' @param t_mean numeric, mean temperature
#' @param base numeric, base temperature
#' @param tunit character, temperature unit
#' @param lunit character, length unit
#'
#' @return numeric, Growing Degree Days
#'
#' @details
#' The Growing Degree Days (HDD) is a measure of heat accumulation used in agriculture to predict the growth and development of plants and insects. The HDD is calculated based on the mean temperature and the base temperature. The base temperature is the minimum temperature at which a plant or insect can grow. The default base temperature is 10ºC.
#'
#' @examples
#' gdd(t_max = 20, t_min = 10, tunit = 'celsius', lunit = 'celsius')
#' gdd(t_max = 30, t_min = 15, tunit = 'celsius', lunit = 'celsius')
#' gdd(t_max = 35, t_min = 18, tunit = 'celsius', lunit = 'celsius')
#'
#' @export
gdd = function(t_max = NULL,
               t_min = NULL,
               t_mean = NULL,
               base = 10,
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
    gdd = t_mean - base
    gdd[gdd < 0] = 0
  }
  return(round(gdd, 2))
}
