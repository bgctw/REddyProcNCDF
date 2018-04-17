
<!-- 
README.md is generated from README.Rmd. Please edit that file
#knitr::knit("README.Rmd") 
rmarkdown::render("README.Rmd") 
maybe clear cache before
-->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/REddyProcNCDF)](http://cran.r-project.org/package=REddyProcNCDF) [![Travis-CI Build Status](https://travis-ci.org/bgctw/REddyProcNCDF.svg?branch=master)](https://travis-ci.org/bgctw/REddyProcNCDF)

Overview
--------

This is an extension of the [`REddyProc` package](https://github.com/bgctw/REddyProc), which supports processing (half)hourly data from Eddy-Covariance sensors.

It adds functionality of reading half-hourly Net Ecosystem Exchange (NEE) data from NetCDF files.

Installation
------------

``` r
# Release stable version from CRAN
#not yet: install.packages("REddyProcNCDF")

# The development version from GitHub using devtools:
# install.packages("devtools")
install.packages("REddyProc")
devtools::install_github("bgctw/REddyProcNCDF")
```

Usage
-----

There is an example file acceseed from the REddyProc repository that stores half-hourly Net-Ecosystem-Exchange (NEE) data, that is read and displayed.

``` r
library(REddyProcNCDF)
?REddyProcNCDF

#+++ Input data from NetCDF file (example needs to be downloaded)
examplePath <- getExamplePath('Example_DE-Tha.1996.1998.hourly_selVars.nc'
                              , isTryDownload = TRUE)
if (length(examplePath)) {
  EddyData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), examplePath)
} else {
  stop(
      "Could not find example text data file."
      ," In order to execute this example code,"
      ," please, allow downloading it from github. " 
      ," Type '?getExamplePath' for more information.")
}
#> Converted time format 'YMDH' to POSIX with column name 'DateTime'.
#> character(0)
#> Loaded BGI Fluxnet NC file: /tmp/Rtmp9nslD2/REddyProcExamples/Example_DE-Tha.1996.1998.hourly_selVars.nc with the following headers:
#>  *** DateTime(POSIXDate Time) year(-) month(-) day(-) hour(-) NEE(umol_m-2_s-1) Rg(W_m-2) NEE_f(umol_m-2_s-1)
head(EddyData.F)
#>              DateTime year month day hour NEE Rg NEE_f
#> 1 1996-01-01 00:30:00 1996     1   1  0.5  NA  0    NA
#> 2 1996-01-01 01:00:00 1996     1   1  1.0  NA  0    NA
#> 3 1996-01-01 01:30:00 1996     1   1  1.5  NA  0    NA
#> 4 1996-01-01 02:00:00 1996     1   1  2.0  NA  0    NA
#> 5 1996-01-01 02:30:00 1996     1   1  2.5  NA  0    NA
#> 6 1996-01-01 03:00:00 1996     1   1  3.0  NA  0    NA
```
