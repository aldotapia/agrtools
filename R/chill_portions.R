#' Compute chill portions (Dynamic model)
#'
#' Computes chill portions with the Dynamic model (Fishman et al., 1987;
#' Erez et al., 1990). Chill builds up as an intermediate product that high
#' temperatures can destroy. Once the intermediate product reaches a critical
#' level, a fraction of it becomes a permanent chill portion. The result
#' depends on the order of temperatures, so `t_h` must be a continuous hourly
#' series in time order.
#'
#' @param t_h numeric. Hourly temperatures, in time order.
#' @param tunit character. The unit of the temperature data. Default is "celsius".
#' @param cumulative logical. If TRUE, return the accumulated chill portions
#'   for every hour instead of the total. Default is FALSE.
#' @param na.rm logical. Should missing values be removed? Default is TRUE.
#'   Removing them joins the hours before and after the gap. If FALSE, any
#'   missing value gives `NA`.
#'
#' @return numeric. Total chill portions, or the accumulated chill portions for
#'   every hour if `cumulative = TRUE`.
#'
#' @references
#' Fishman, S., Erez, A., Couvillon, G.A. (1987). The temperature dependence of
#' dormancy breaking in plants: mathematical analysis of a two-step model
#' involving a cooperative transition. Journal of Theoretical Biology 124, 473-483.
#'
#' @examples
#' chill_portions(rep(6, 240))
#' chill_portions(rep(c(4, 8, 12, 16, 12, 8), 40), cumulative = TRUE)
#'
#' @export
chill_portions = function(t_h, tunit = "celsius", cumulative = FALSE,
                          na.rm = TRUE) {
  if (!is.numeric(t_h)) {
    stop("t_h should be numeric")
  }
  if (na.rm) {
    t_h = t_h[!is.na(t_h)]
  } else if (anyNA(t_h)) {
    return(if (cumulative) rep(NA_real_, length(t_h)) else NA_real_)
  }
  # the model parameters were fitted with T + 273, not 273.15
  tk = convert_units(t_h, "temperature", tunit, "celsius") + 273

  # Fishman et al. (1987) parameters
  e0 = 4153.5
  e1 = 12888.8
  a0 = 139500
  a1 = 2.567e18
  slp = 1.6
  tetmlt = 277

  sr = exp(slp * tetmlt * (tk - tetmlt) / tk)
  xi = sr / (1 + sr)
  xs = a0 / a1 * exp((e1 - e0) / tk)
  ak1 = a1 * exp(-e1 / tk)

  portion = numeric(length(tk))
  s = 0
  for (i in seq_along(tk)) {
    x = xs[i] - (xs[i] - s) * exp(-ak1[i])
    if (x < 1) {
      s = x
    } else {
      portion[i] = xi[i] * x
      s = x * (1 - xi[i])
    }
  }

  if (cumulative) cumsum(portion) else sum(portion)
}
