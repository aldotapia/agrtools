#' Crop coefficient curve (FAO-56 single Kc)
#'
#' Builds the daily crop coefficient from the four growth stages of FAO-56
#' (Allen et al., 1998, chapter 6): Kc is constant at `kc_ini` during the
#' initial stage, rises linearly to `kc_mid` during development, stays at
#' `kc_mid` during mid-season and falls linearly to `kc_end` during late season
#' (eq. 66).
#'
#' @param days numeric. Days after planting (1 is the planting day).
#' @param l_ini,l_dev,l_mid,l_late numeric. Length of the initial, development,
#'   mid-season and late-season stages (days). See FAO-56 Table 11.
#' @param kc_ini,kc_mid,kc_end numeric. Crop coefficients for the initial stage,
#'   mid-season and end of the season. See FAO-56 Table 12 and `kc_adjust()`.
#'
#' @return numeric. Kc for each day, `NA` outside the growing season.
#'
#' @examples
#' # maize-like crop planted on day 1
#' kc = kc_curve(1:150, 30, 40, 50, 30, kc_ini = 0.3, kc_mid = 1.2, kc_end = 0.6)
#' plot(kc, type = "l")
#'
#' @export
kc_curve = function(days, l_ini, l_dev, l_mid, l_late,
                    kc_ini, kc_mid, kc_end) {
  ends = cumsum(c(l_ini, l_dev, l_mid, l_late))
  # fraction of a stage completed; a zero-length stage is a step
  frac = function(start, len) {
    if (len == 0) return(as.numeric(days > start))
    pmin(pmax((days - start) / len, 0), 1)
  }
  kc = kc_ini + (kc_mid - kc_ini) * frac(ends[1], l_dev) +
    (kc_end - kc_mid) * frac(ends[3], l_late)
  kc[days < 1 | days > ends[4]] = NA
  kc
}

#' Climatic adjustment of Kc mid and Kc end (FAO-56)
#'
#' Adjusts tabulated `kc_mid` (or `kc_end`) values to climates where the mean
#' minimum relative humidity differs from 45% or the mean wind speed at 2 m
#' differs from 2 m/s (Allen et al., 1998, eqs. 62 and 65).
#'
#' @param kc numeric. Tabulated Kc mid or Kc end (FAO-56 Table 12).
#' @param u2 numeric. Mean wind speed at 2 m during the stage (m/s), between 1 and 6.
#' @param rh_min numeric. Mean minimum relative humidity during the stage (%), between 20 and 80.
#' @param h numeric. Mean plant height during the stage (m), between 0.1 and 10.
#'
#' @return numeric. Adjusted Kc. Values of `kc` below 0.45 are returned unchanged,
#'   as FAO-56 recommends for Kc end.
#'
#' @examples
#' kc_adjust(1.2, u2 = 3, rh_min = 30, h = 2)
#'
#' @export
kc_adjust = function(kc, u2, rh_min, h) {
  u2 = pmin(pmax(u2, 1), 6)
  rh_min = pmin(pmax(rh_min, 20), 80)
  h = pmin(pmax(h, 0.1), 10)
  adjusted = kc + (0.04 * (u2 - 2) - 0.004 * (rh_min - 45)) * (h / 3)^0.3
  ifelse(kc < 0.45, kc, adjusted)
}

#' Crop evapotranspiration
#'
#' Crop evapotranspiration, ETc = Ks * Kc * ET0 (FAO-56 eqs. 56 and 80).
#'
#' @param et0 numeric. Reference evapotranspiration (mm/day).
#' @param kc numeric. Crop coefficient, see `kc_curve()`.
#' @param ks numeric. Water stress coefficient (0-1). Default is 1 (no stress).
#'   See `water_balance()` to compute it.
#'
#' @return numeric. Crop evapotranspiration in mm/day.
#'
#' @examples
#' etc(5, 1.15)
#'
#' @export
etc = function(et0, kc, ks = 1) {
  ks * kc * et0
}

#' Daily root zone soil water balance (FAO-56)
#'
#' Computes the daily root zone depletion and water stress coefficient
#' following FAO-56 chapter 8 (Allen et al., 1998, eqs. 82-88). Runoff and
#' capillary rise are assumed to be zero, so pass effective precipitation in
#' `rain` if runoff matters.
#'
#' @param et0 numeric. Daily reference evapotranspiration (mm/day).
#' @param kc numeric. Daily crop coefficient, see `kc_curve()`.
#' @param rain numeric. Daily (effective) precipitation (mm). Default is 0.
#' @param irrigation numeric. Daily net irrigation applied (mm). Default is 0.
#' @param theta_fc numeric. Soil water content at field capacity (m3/m3).
#' @param theta_wp numeric. Soil water content at wilting point (m3/m3).
#' @param zr numeric. Rooting depth (m). A single value or one value per day.
#' @param p numeric. Fraction of TAW that can be depleted before stress
#'   (FAO-56 Table 22). A single value or one value per day. Default is 0.5.
#' @param dr0 numeric. Root zone depletion at the start (mm). Default is 0
#'   (soil at field capacity).
#' @param auto_irrigate logical. If TRUE, the root zone is refilled to field
#'   capacity whenever depletion reaches RAW, and the applied depth is added to
#'   the `irrigation` column. Default is FALSE.
#'
#' @return data.frame with one row per day and the columns `taw` and `raw`
#'   (total and readily available water, mm), `ks` (water stress coefficient),
#'   `etc` and `etc_adj` (crop evapotranspiration without and with stress,
#'   mm), `irrigation` (mm), `dp` (deep percolation, mm) and `dr` (root zone
#'   depletion at the end of the day, mm).
#'
#' @examples
#' et0 = rep(5, 60)
#' kc = kc_curve(1:60, 10, 20, 20, 10, 0.3, 1.15, 0.5)
#' wb = water_balance(et0, kc, theta_fc = 0.3, theta_wp = 0.15, zr = 0.6,
#'                    auto_irrigate = TRUE)
#' sum(wb$irrigation)
#'
#' @export
water_balance = function(et0, kc, rain = 0, irrigation = 0,
                         theta_fc, theta_wp, zr, p = 0.5, dr0 = 0,
                         auto_irrigate = FALSE) {
  n = length(et0)
  if (length(kc) != n) {
    stop("et0 and kc should have the same length")
  }
  if (any(theta_fc <= theta_wp)) {
    stop("theta_fc should be greater than theta_wp")
  }
  if (anyNA(et0) || anyNA(kc)) {
    stop("et0 and kc should not contain NA, fill the gaps first (kc_curve() returns NA outside the season)")
  }
  rain = rep_len(rain, n)
  irrigation = rep_len(irrigation, n)
  rain[is.na(rain)] = 0
  irrigation[is.na(irrigation)] = 0

  taw = rep_len(1000 * (theta_fc - theta_wp) * zr, n)
  raw = rep_len(p, n) * taw
  ks = etc_adj = dp = dr = numeric(n)
  prev = dr0
  for (i in seq_len(n)) {
    ks[i] = if (prev > raw[i]) max(0, (taw[i] - prev) / (taw[i] - raw[i])) else 1
    etc_adj[i] = ks[i] * kc[i] * et0[i]
    d = prev - rain[i] - irrigation[i] + etc_adj[i]
    dp[i] = max(0, -d)
    d = max(d, 0)
    if (d > taw[i]) {
      # the soil cannot dry past wilting point, so ETc is limited to what is left
      etc_adj[i] = etc_adj[i] - (d - taw[i])
      d = taw[i]
    }
    if (auto_irrigate && d >= raw[i]) {
      irrigation[i] = irrigation[i] + d
      d = 0
    }
    dr[i] = d
    prev = d
  }

  data.frame(taw = taw, raw = raw, ks = ks, etc = kc * et0,
             etc_adj = etc_adj, irrigation = irrigation, dp = dp, dr = dr)
}
