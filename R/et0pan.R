#' @title Compute daily reference evapotranspiration using Pav evaporation method.
#'
#' @description
#' Compute daily reference evapotranspiration using Pav evaporation method from FAO-56
#'
#'
#' @param epan numeric. Pan evaporation in mm/day.
#' @param kp numeric or NULL. Coefficient of the pan. Default is NULL.
#' @param calculate_kp logical. If TRUE, kp will be calculated using the formula proposed by Allen et al. (1998). Default is FALSE.
#' @param u2 numeric or NULL. Wind speed at 2 meters in m/s. Default is NULL.
#' @param rh_mean numeric or NULL. Mean relative humidity in %. Default is NULL.
#' @param fet numeric or NULL. Fetch length in meters. Default is NULL.
#' @param class character. Class of the pan. Default is "class a".
#' @param fetch character. Fetch of the pan. Default is "green".
#'
#' @return A numeric value of the reference evapotranspiration in mm/day.
#'
#' @details
#' Measure water evaporated from a Pan to estimate reference evapotranspiration (ETo) using the FAO-56 Pan evaporation method. This function requires pan evaporation and pan coefficient (Kp). Kp can be estimated using mean wind speed at 2 meters, mean relative humidity, fetch length, class of the pan and fetch of the pan. There are 2 different classes of pan:
#' Class A and Class B. Fetch of the pan can be green or dry. The fetch is the ground cover around the pan.
#'
#' @examples
#' # FAO-56 data example:
#' # pan evaporation: 7.9 mm/day
#' # Fetch = 1000 m
#' # wind speed at 2 meters: 1.9 m/s
#' # mean relative humidity: 73%
#' # Solution (in order): 6.6, 4.8, 7.7 ad 5.4 mm/day
#' et0pan(epan = 7.9,
#'        calculate_kp = TRUE,
#'        u2 = 1.9,
#'        fet = 1000,
#'        rh_mean = 73,
#'        class = 'class a',
#'        fetch = 'green')
#'
#' et0pan(epan = 7.9,
#'        calculate_kp = TRUE,
#'        u2 = 1.9,
#'        fet = 1000,
#'        rh_mean = 73,
#'        class = 'class a',
#'        fetch = 'dry')
#'
#' et0pan(epan = 7.9,
#'        calculate_kp = TRUE,
#'        u2 = 1.9,
#'        fet = 1000,
#'        rh_mean = 73,
#'        class = 'colorado',
#'        fetch = 'green')
#'
#' et0pan(epan = 7.9,
#'        calculate_kp = TRUE,
#'        u2 = 1.9,
#'        fet = 1000,
#'        rh_mean = 73,
#'        class = 'colorado',
#'        fetch = 'dry')
#'
#' @export
et0pan = function(epan,
                  kp = NULL,
                  calculate_kp = FALSE,
                  u2 = NULL,
                  rh_mean = NULL,
                  fet = NULL,
                  class = c('class a', 'colorado'),
                  fetch = c('green','dry')){

  class_list = c('class a', 'colorado')
  fetch_list = c('green','dry')

  if(!is.numeric(epan)){
    stop("epan should be numeric")
  }
  if(!is.logical(calculate_kp)){
    stop("calculate_kp should be logical")
  }
  if(calculate_kp == FALSE){
    if(!is.numeric(kp)){
      stop("kp should be numeric")
    }else{
      if(kp < 0.35 | kp > 0.85){
        stop("kp should be between 0.35 and 0.85")
      }
    }
  }else{
    if(class %in% class_list){
      if(fetch %in% fetch_list){
        if(!is.numeric(u2)){
          stop("u2 should be numeric")
        }
        if(!is.numeric(fet)){
          stop("fet should be numeric")
        }
        if(!is.numeric(rh_mean)){
          stop("rh_mean should be numeric")
        }
        if(class == class_list[1]){
          if(fetch == fetch_list[1]){
            kp = 0.108 - 0.0286 * u2 + 0.0422 * log(fet) +  0.1434 * log(rh_mean) -
              0.000631 * (log(fet)^2) * log(rh_mean)
          }else{
            kp = 0.61 + 0.00341 * rh_mean - 0.000162 * u2 * rh_mean -
              0.00000959 * u2 * fet + 0.00327 * u2 * log(fet) -
              0.00289 * u2 * log(86.4 * u2) - 0.0106 * log(86.4 * u2) * log(fet) +
              0.00063 * (log(fet)^2)*log(86.4*u2)
          }
        }else{
          if(fetch == fetch_list[1]){
            kp = 0.87 + 0.119 * log(fet) - 0.0157 * (log(86.4*u2)^2) -
              0.0019 * (log(fet)^2) * log(86.4*u2) + 0.013 * log(86.4*u2) *
              log(rh_mean) - 0.000053 * log(86.4*u2) * log(fet) * rh_mean
          }else{
            kp = 1.145 - 0.080 * u2 + 0.000903 * (u2^2) * log(rh_mean) -
              0.0964 * log(fet) + 0.0031 * u2 * log(fet) +
              0.0015 * (log(fet)^2)*log(rh_mean)
          }
        }
      }else{
        stop("fetch should be either green or dry")
      }
    }else{
      stop("class should be either class a or colorado")
    }
  }

  et0 = kp * epan
  return(et0)
}
