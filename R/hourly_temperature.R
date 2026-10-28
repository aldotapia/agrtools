#' Hourly temperature from daily maximum and minimum
#'
#' Builds an hourly temperature curve from daily maximum and minimum
#' temperature with the Linvill (1990) method. During the day temperature
#' follows a sine curve from the minimum at sunrise, peaking about two hours
#' after solar noon. After sunset it decays logarithmically towards the next
#' day's minimum. Sunrise and sunset come from the day length at the given
#' latitude. Hours are in solar time.
#'
#' Use it to run hourly chill models (`chillinghours()`, `chill_portions()`)
#' on daily data. Chill computed from reconstructed temperatures is an
#' approximation: for the `piscoelqui` station, hourly RMSE is about 1.7 ºC and
#' the 2023 chill portions are about 30% lower than with observed temperatures.
#'
#' @param date Date or character (YYYY-MM-DD). Consecutive days.
#' @param t_max numeric. Daily maximum temperature.
#' @param t_min numeric. Daily minimum temperature.
#' @param lat numeric. Latitude, in decimal degrees by default.
#' @param lat_u character. Unit of the latitude. Default is "deg".
#'
#' @return data.frame with columns `date`, `hour` (0-23) and `temp`, in the
#'   same unit as `t_max` and `t_min`.
#'
#' @references
#' Linvill, D.E. (1990). Calculating chilling hours and chill units from daily
#' maximum and minimum temperature observations. HortScience 25(1), 14-16.
#'
#' @examples
#' hourly_temperature(c("2023-07-01", "2023-07-02"), t_max = c(15, 17),
#'                    t_min = c(2, 4), lat = -30.13)
#'
#' @export
hourly_temperature = function(date, t_max, t_min, lat, lat_u = "deg") {
  date = as_date_vec(date)
  n = length(date)
  if (length(t_max) != n || length(t_min) != n) {
    stop("date, t_max and t_min should have the same length")
  }
  if (n > 1 && any(diff(date) != 1)) {
    stop("date should be consecutive days in increasing order")
  }
  if (lat_u != "rad") {
    lat = convert_units(lat, "angle", lat_u, "rad")
  }

  j = as.numeric(format(date, "%j"))
  ws = sunset_hour_angle(j, lat)$ws
  # ponytail: day length clamped to 1-23 h so polar days/nights don't break the curve
  dl = pmin(pmax(24 * ws / pi, 1), 23)
  sunrise = 12 - dl / 2
  sunset = 12 + dl / 2

  day_temp = function(h, i) {
    t_min[i] + (t_max[i] - t_min[i]) * sin(pi * (h - sunrise[i]) / (dl[i] + 4))
  }
  # temperature from the sunset of day i towards the minimum of day k
  night_temp = function(hours_after_sunset, i, k) {
    t_ss = day_temp(sunset[i], i)
    night = 24 - dl[i]
    t_ss - (t_ss - t_min[k]) * log(1 + pmax(hours_after_sunset, 0)) / log(1 + night)
  }

  hour = 0:23
  temp = unlist(lapply(seq_len(n), function(i) {
    prev = max(i - 1, 1)
    nxt = min(i + 1, n)
    ifelse(hour < sunrise[i],
           night_temp(hour + 24 - sunset[prev], prev, i),
           ifelse(hour > sunset[i],
                  night_temp(hour - sunset[i], i, nxt),
                  day_temp(hour, i)))
  }))

  data.frame(date = rep(date, each = 24), hour = rep(hour, n), temp = temp)
}
