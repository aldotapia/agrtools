# Coerce character, Date or POSIXt input to a Date vector
as_date_vec = function(date) {
  if (is.character(date)) {
    out = as.Date(date, format = "%Y-%m-%d")
  } else if (inherits(date, "POSIXt")) {
    # keep the local calendar day, as.Date() would use UTC
    out = as.Date(format(date, "%Y-%m-%d"))
  } else {
    out = tryCatch(as.Date(date), error = function(e) NULL)
  }
  if (is.null(out) || any(is.na(out) & !is.na(date))) {
    stop("date should be a Date or in YYYY-MM-DD format")
  }
  out
}

# Solar declination and sunset hour angle (FAO-56 eqs. 24-25), lat in radians
sunset_hour_angle = function(j, lat) {
  delta_s = 0.409 * sin((2 * pi * j / 365) - 1.39)
  # clamp for polar day/night
  list(delta_s = delta_s,
       ws = acos(pmax(-1, pmin(1, -tan(lat) * tan(delta_s)))))
}
