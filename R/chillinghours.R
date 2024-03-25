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
