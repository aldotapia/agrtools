#' Estimate chilling hours from daily data
#'
#' This functions estimates the daily chilling hours based on the maximum and minimum temperature data. The chilling hours are calculated based on a threshold temperature. The default threshold temperature is 7.2ºC.
#'
#' @param t_max numeric. Maximum temperature.
#' @param t_min numeric. Minimum temperature.
#' @param kf numeric. Coefficient (from 1 to 1.5 recommended)
#' @param threshold numeric. Threshold temperature (default is 7.2)
#' @param t_maxu character. Unit of maximum temperature (celsius, fahrenheit, kelvin)
#' @param t_minu character. Unit of minimum temperature (celsius, fahrenheit, kelvin)
#' @param thresholdunit character. Unit of threshold temperature (celsius, fahrenheit, kelvin)
#' @param convert_to_celsius logical. Convert temperatures to celsius (default is TRUE)
#'
#' @return numeric. Daily chilling hours.
#'
#' @examples
#' dailychillinghours(20, 6, kf = 1, threshold = 7.2)
#'
#' dailychillinghours(20, 42.8, kf = 1, threshold = 45, t_minu = "fahrenheit",
#'                    thresholdunit = "fahrenheit", convert_to_celsius = TRUE)
#'
#' @export
dailychillinghours = function(t_max,
                              t_min,
                              kf = 1,
                              threshold = 7.2,
                              t_maxu = "celsius",
                              t_minu = "celsius",
                              thresholdunit = "celsius",
                              convert_to_celsius = TRUE){
  if (!is.numeric(t_max) | !is.numeric(t_min) | !is.numeric(kf) | !is.numeric(threshold)) {
    stop("t_max, t_min, kf, threshold should be numeric")
  }
  temperature_list = c("celsius", "fahrenheit", "kelvin")
  if (!t_maxu %in% temperature_list) {
    stop("tunit should be one of celsius, fahrenheit, kelvin")
  }
  if (!t_minu %in% temperature_list) {
    stop("tunit should be one of celsius, fahrenheit, kelvin")
  }
  if (!thresholdunit %in% temperature_list) {
    stop("thresholdunit should be one of celsius, fahrenheit, kelvin")
  }
  if (convert_to_celsius) {
    if(t_maxu != 'celsius'){
      t_max = convert_units(t_max, "temperature", t_maxu, "celsius")
    }
    if(t_minu != 'celsius'){
      t_min = convert_units(t_min, "temperature", t_minu, "celsius")
    }
    if(thresholdunit != 'celsius'){
      threshold = convert_units(threshold, "temperature", thresholdunit, "celsius")
    }
  }

  result = 24 * kf *((threshold - t_min) / (t_max - t_min))
  result[result < 0] = 0
  return(round(result,2))
}
