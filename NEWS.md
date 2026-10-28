# agrtools 0.0.0.9007

## New functions

* Psychrometric helpers from FAO-56: `sat_vapor_pressure()`, `slope_vapor_pressure()`, `actual_vapor_pressure()`, `vpd()`, `dew_point()`, `atm_pressure()`, `psychrometric_constant()` and `wind_at_2m()`.
* `net_radiation()`: daily net radiation (net shortwave minus net longwave).
* `et0pm()` now uses these helpers internally. Its results are unchanged.

* Crop water tools: `kc_curve()`, `kc_adjust()`, `etc()` and `water_balance()` (FAO-56 chapters 6 and 8). The vignette has a new section showing them.

* `chill_portions()`: chill portions with the Dynamic model (Fishman et al., 1987). Results match `chillR::Dynamic_Model()`, except that each portion is counted in the hour it forms instead of the hour after.
* `hourly_temperature()`: hourly temperatures from daily maximum and minimum (Linvill, 1990), so the hourly chill models can run on daily data.

## Bug fixes

* `convert_units()` was rewritten with factor tables. Converting a unit to itself now works (it used to return `NULL`). Wrong factors were corrected in "daily radiation" (cal/cm2/day), "radiation" and "density" (lb/yd3, ton/yd3, lb/ft3 to lb/yd3). `"speed"` is accepted as an alias of `"velocity"`.
* The "radiation" category is now energy per area only: J/m2, MJ/m2, Wh/m2, cal/cm2. `"W/m2"` was dropped because it is a power unit; use "daily radiation" with `"W/m2/day"` for daily mean irradiance.
* `et0pm()`: `u_h_u` no longer always errors, `rh_*_u` units (`"%"` or `"fraction"`) are now applied, the no-RH case uses `t_min` for actual vapour pressure (FAO-56), and Rs/Rso is capped at 1.
* `et0h()`, `et0hs()`: `lat_u` now defaults to `"deg"`, as documented. Code that passed latitude in radians without setting `lat_u = "rad"` must now set it.
* `et0h()`, `et0hs()`, `et0pm()` accept vectors of character dates.
* `gdd()` is vectorized.
* `et0pan()` works with the default `class`/`fetch`, and a supplied kp may go up to 1.1 (Colorado pans exceed 0.85).
* `dailychillinghours()` is capped at `24 * kf` hours and returns `NA` when `t_max == t_min`.
* `extraterrestrial_radiation()` no longer returns `NaN` at polar latitudes.
* `hourly_to_daily()` uses `datecol` throughout, accepts POSIXct dates, implements the `"n"` suffix, and no longer depends on dplyr.

* `kc_curve()` handles zero-length growth stages. `water_balance()` gives a clear error for NA `et0`/`kc`, and limits ETc when depletion reaches TAW so the balance always closes.
* `hourly_to_daily()` accepts a function for `func` again, as in earlier versions.
* `chill_portions(na.rm = FALSE)` returns `NA` instead of failing.
* Date-times passed to the ET0 functions and `hourly_temperature()` keep their local calendar day instead of being converted in UTC.
* `actual_vapor_pressure()` and `et0pm()` give an error when only `rh_min` is given; previously it was silently ignored.
* `hourly_temperature()` checks that dates are consecutive.

## Other

* Added unit tests based on FAO-56 worked examples.
* dplyr and scales moved to Suggests.
