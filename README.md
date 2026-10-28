# `agrtools`

## Description

`agrtools` is an R package that provides a collection of tools for working with agricultural data. The package is under development and new functions will be added in the future.

## Functions

Example data:
 - `piscoelqui`: hourly weather data from the Pisco Elqui weather station in the Coquimbo region, Chile.

General functions:
 - `convert_units()`: convert units of different variables and units.
 - `hourly_to_daily()`: convert hourly data to daily data based on the suffix of the column names for applying the aggregation function.

Weather data functions:
 - `cdd()`: calculate the cooling degree days.
 - `hdd()`: calculate the heating degree days.
 - `gdd()`: calculate the growing degree days.
 - `extraterrestrial_radiation()`: calculate the extraterrestrial radiation.
 - `clearsky_radiation()`: calculate the clear sky radiation.
 - `chillinghours()`: calculate the chilling hours.
 - `chill_portions()`: calculate chill portions with the Dynamic model.
 - `hourly_temperature()`: build hourly temperatures from daily maximum and minimum (Linvill method).
 - `dailychillinghours()`: estimate the chilling hours based on daily data.
 - `sat_vapor_pressure()`, `slope_vapor_pressure()`, `actual_vapor_pressure()`, `vpd()`, `dew_point()`: vapour pressure helpers (FAO-56).
 - `atm_pressure()`, `psychrometric_constant()`, `wind_at_2m()`: atmospheric pressure, psychrometric constant and wind speed adjustment to 2 m.
 - `net_radiation()`: daily net radiation.
 - `et0pm()`: calculate the reference evapotranspiration using the Penman-Monteith method.
 - `et0h()`: calculate the reference evapotranspiration using the Hargreaves method.
 - `et0hs()`: calculate the reference evapotranspiration using the Hargreaves-Samani method.
 - `et0pan()`: calculate the reference evapotranspiration using the Pan evaporation method. It also includes the option to calculate pan coefficient (Kp).

Crop water functions:
 - `kc_curve()`: daily crop coefficient from the four FAO-56 growth stages.
 - `kc_adjust()`: climatic adjustment of Kc mid and Kc end.
 - `etc()`: crop evapotranspiration.
 - `water_balance()`: daily root zone soil water balance with water stress coefficient and optional automatic irrigation.

## Installation

You can install the development version of `agrtools` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("aldotapia/agrtools", build_vignettes = TRUE)
```

## Examples

see the vignettes for examples of how to use the functions.

``` r
browseVignettes("agrtools")
```