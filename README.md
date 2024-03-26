# `agrtools`

## Description

`agrtools` is an R package made for provide a collection of tools for working with agricultural data. The package is under development and new functions will be added in the future.

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
 - `dailychillinghours()`: calculate the chilling hours based on daily data.
 - `et0pm()`: calculate the potential evapotranspiration using the Penman-Monteith method.
 - `et0h()`: calculate the potential evapotranspiration using the Hargreaves method.
  - `et0hs()`: calculate the potential evapotranspiration using the Hargreaves-Samani method.
  - `et0pan()`: calculate the potential evapotranspiration using the Pan evaporation method. It also includes the option to calculate pan coefficient (Kp)

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