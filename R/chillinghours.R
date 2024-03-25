#' Compute chilling hours
#'
#' Function to compute chilling hours based on the daily temperature data.
#'
#' @param t_h numeric. A vector of daily temperature data.
#' @param threshold numeric. A vector of two values representing the lower and upper threshold temperatures.
#' @param tunit character. The unit of the temperature data. Default is "celsius".
#' @param thresholdunit character. The unit of the threshold temperature. Default is "celsius".
#' @param output character. The type of output. Default is "chillhours".
#' @param na.rm logical. Should missing values be removed? Default is TRUE.
#'
#' @return A numeric value representing the chilling hours or chilling units.
#'
#' @examples
#' chillinghours(c(5, 6, 7, 8, 9, 10))
#'
#' @export
chillinghours = function(t_h, threshold = c(0,7.2),
                         tunit = "celsius",
                         thresholdunit = "celsius",
                         output = c("chillhours", "chillunits"),
                         na.rm = TRUE) {
  if(na.rm){
    t_h = t_h[!is.na(t_h)]
  }
  if(!is.numeric(threshold)){
    stop("threshold should be numeric")
  }
  if(length(threshold) != 2){
    stop("threshold should have two values")
  }
  temperature_list = c("celsius", "fahrenheit", "kelvin")
  if (!tunit %in% temperature_list) {
    stop("tunit should be one of celsius, fahrenheit, kelvin")
  }
  if (!thresholdunit %in% temperature_list) {
    stop("thresholdunit should be one of celsius, fahrenheit, kelvin")
  }
  if (tunit != thresholdunit) {
    stop("tunit and thresholdunit should be the same")
  }

  output = match.arg(output)
  if(sum(output %in% c("chillhours", "chillunits"))==0){
    stop("output should be one of chillhours, chillunits")
  }

  switch (output,
    'chillhours' = sum((t_h < threshold[2]) & (t_h > threshold[1])),
    'chillunits' = sum(threshold[2] - t_h[(t_h < threshold[2]) &
                                            (t_h > threshold[1])])
  )
}
