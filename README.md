
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

### NetCDF system libraries

Reading NetCDF files requires several system libraries to be installed.

Debian based OS users (Ubuntu, Docker): do from a shell:

    sudo apt-get update
    sudo apt-get install libnetcdf-dev
    sudo apt-get install libudunits2-dev

Windows users, see [ncdf home page](http://cirrus.ucsd.edu/~pierce/ncdf/)

Mac users: TODO

### R-packages

``` r
# Release stable version from CRAN
#not yet: install.packages("REddyProcNCDF")

# The development version from GitHub using devtools:
# install.packages("devtools")
install.packages("RNetCDF")
devtools::install_github("bgctw/REddyProcNCDF")
```

Alternatively to the RNetCDF package dependency, REddyProcNCDF also works if the ncdf4 package has been installed.

Usage
-----

There is an example file acceseed from the REddyProc repository that stores half-hourly Net-Ecosystem-Exchange (NEE) data, that is read and displayed.

``` r
library(REddyProcNCDF)
?REddyProcNCDF

examplePath <- system.file(
  file.path('examples','Example_DE-Tha.1996.1998.hourly_selVars.nc')
  , package = "REddyProcNCDF")
EddyData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), examplePath)
#> Loading required namespace: RNetCDF
#> Converted time format 'YMDH' to POSIX with column name 'DateTime'.
#> character(0)
#> Loaded BGI Fluxnet NC file: /home/twutz/R/x86_64-pc-linux-gnu-library/3.4/REddyProcNCDF/examples/Example_DE-Tha.1996.1998.hourly_selVars.nc with the following headers:
#>  *** DateTime(POSIXDate Time) year(-) month(-) day(-) hour(-) NEE(umol_m-2_s-1) Rg(W_m-2) NEE_f(umol_m-2_s-1)
head(EddyData.F)
#>              DateTime year month day hour NEE Rg NEE_f
#> 1 1996-01-01 00:30:00 1996     1   1  0.5  NA  0    NA
#> 2 1996-01-01 01:00:00 1996     1   1  1.0  NA  0    NA
#> 3 1996-01-01 01:30:00 1996     1   1  1.5  NA  0    NA
#> 4 1996-01-01 02:00:00 1996     1   1  2.0  NA  0    NA
#> 5 1996-01-01 02:30:00 1996     1   1  2.5  NA  0    NA
#> 6 1996-01-01 03:00:00 1996     1   1  3.0  NA  0    NA
# EddyData.F can now be used to initialize EddyProc R5 class 
```
