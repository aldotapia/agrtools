#' Compute chilling hours
#'
#' Function to compute chilling hours based on the daily temperature data.
#'
#' @param t_h numeric. A vector of daily temperature data.
#' @param threshold numeric. A vector of two values representing the lower and upper threshold temperatures.
#' @param tunit character. The unit of the temperature data. Default is "celsius".
#' @param thresholdunit character. The unit of the threshold temperature. Default is "celsius".
#' @param method character. The method to compute chilling hours. Default is "classic".
#' @param output character. The type of output, only valid for method = "classic". Default is "chillhours".
#' @param na.rm logical. Should missing values be removed? Default is TRUE.
#'
#' @return A numeric value representing the chilling hours or chilling units.
#'
#' @examples
#' chillinghours(c(5, 6, 7, 8, 15, 18))
#' chillinghours(c(5, 6, 7, 8, 15, 18), method = "utah")
#' chillinghours(c(5, 6, 7, 8, 15, 18), method = "dpcu")
#'
#' @export
chillinghours = function(t_h, threshold = c(0,7.2),
                         tunit = "celsius",
                         thresholdunit = "celsius",
                         method = c("classic","utah","dpcu"),
                         output = c("chillhours", "chillunits"),
                         na.rm = TRUE) {
  temperature_list = c("celsius", "fahrenheit", "kelvin")
  method_list = c("classic", "utah", "dpcu")
  rc = data.frame('temperature' = c(1.4,2.4,9.1,12.4,15.9,17.9),
                  'unit' = c(0.5, 1, 0.5, 0, -0.5, -1))
  rc2 = data.frame('temperature' = c(1.4,2.4,9.1,12.4,15.9,17.9),
                  'unit' = c(0.5, 1, 0.5,0, -0.5, 0))
  if(na.rm){
    t_h = t_h[!is.na(t_h)]
  }
  if(!is.numeric(threshold)){
    stop("threshold should be numeric")
  }
  if(length(threshold) != 2){
    stop("threshold should have two values")
  }
  if (!tunit %in% temperature_list) {
    stop("tunit should be one of celsius, fahrenheit, kelvin")
  }
  if (!thresholdunit %in% temperature_list) {
    stop("thresholdunit should be one of celsius, fahrenheit, kelvin")
  }
  if (tunit != thresholdunit) {
    stop("tunit and thresholdunit should be the same")
  }
  method = match.arg(method)
  if (!method %in% method_list) {
    stop("method should be one of classic, utah")
  }
  if(method == "classic"){
    output = match.arg(output)
    if(sum(output %in% c("chillhours", "chillunits"))==0){
      stop("output should be one of chillhours, chillunits")
    }
    if(output == "chillhours"){
      t_h = sum((t_h < threshold[2]) & (t_h > threshold[1]))
    }else{
      t_h = sum(threshold[2] - t_h[(t_h < threshold[2]) & (t_h > threshold[1])])
      }
  } else if(method == "utah"){
    if(tunit == "celsius"){
      t_h = cut(t_h, breaks = c(-Inf, rc$temperature, Inf),
                labels = c(0, rc$unit))
      t_h = as.numeric(as.character(t_h))
      t_h = sum(t_h)
    }
  }else if(method == "dpcu"){
    t_h = cut(t_h, breaks = c(-Inf, rc2$temperature, Inf),
              labels = c(0, rc2$unit))
    t_h = as.numeric(as.character(t_h))
    t_h = sum(t_h)
  }else{
    stop("For utah method, the temperature should be in celsius")
  }
  return(t_h)
}
