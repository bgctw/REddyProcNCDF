#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load NetCDF data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fLoadFluxNCIntoDataframe <- function(
  ##title<<
  ## Load data from a NetCDF file
  ##description<<
  ## Load specified variables and time stamp information from NetCDF file in
  ## Fluxnet BGI format.
  ## The time stamp information needs to be provided as variables
  ## 'year', 'month', 'day', 'hour'.
  VarList.V.s         ##<< String vector of variables to be read in
  , FileName.s        ##<< File name as a string
  , Dir.s = ''        ##<< Directory as a string
  , NcPackage.s = 'ncdf4'  ##<< Name of R NetCDF package
    ## (implemented for 'RNetCDF' and 'ncdf4') as a string
  , fReadTime = fReadTimeSeveralCols	##<< function that reads time columns
    ## It must append columns year (from 0AD), month, day, and hour (fractional)
    ## See \code{\link{fReadTimeSeveralCols}}
  , ...		##<< further arguments to var.get.nc or ncvar_get, such as start and count
) {
  ##author<<AMM, KS, TW
  # Check for R NetCDF packages
  if (!((NcPackage.s == 'ncdf4' && suppressWarnings(requireNamespace("ncdf4")) )
        || (NcPackage.s == 'RNetCDF' && suppressWarnings(requireNamespace("RNetCDF")) )) )
    stop('fLoadFluxNCIntoDataframe::: Required package \'', NcPackage.s
         , '\' could not be loaded!')

  # Read in time variables
  Data.F <- fReadTime(
    NULL, FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe', ...)
  # Convert time format to POSIX
  # !Attention: Use YMDH time format because julday and hour time stamps
  # inconsistent at end of year
  Data.F <- fConvertTimeToPosix(
    Data.F, 'YMDH', Year.s = 'year', Month.s = 'month'
    , Day.s = 'day', Hour.s = 'hour')
  # Read in variables from a given list of needed variables
  # capture both stdErr and stdOut from fAddNCVar, so that messages can be suppressed
  msgFromfAddNCFVar <- capture.output(capture.output(
		  Data.F <- fAddNCFVar(
		    Data.F, setdiff(VarList.V.s, colnames(Data.F)), FileName.s, Dir.s, NcPackage.s
		    , 'fLoadFluxNCIntoDataframe', ...)
		  , type = c("message")))
  message(msgFromfAddNCFVar)
  message('Loaded BGI Fluxnet NC file: ', FileName.s, ' with the following headers:')
  message(' *** ', paste(colnames(Data.F), '(', as.character(lapply(
    Data.F, attr, which = 'units')), ')', collapse = ' ', sep = ''))
  Data.F
  ##value<<
  ## Data frame with data from nc file.
  ##examples<<
  ## \donttest{
  ## EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f')
  ##     , getExamplePath('Example_DE-Tha.1996.1998.hourly_selVars.nc', TRUE))
  ## }
}

.tmp.f <- function(){
  FileName.s <- 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
  Dir.s <- '../REddyProc/examples';
  VarList.V.s <- c('NEE', 'Rg', 'rH', 'Tair', 'NEE_f')
  NcPackage.s <- 'ncdf4'
  str(tmp <- fLoadFluxNCIntoDataframe(
    c('NEE', 'Rg'), 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
    , Dir.s = Dir.s, count = c(1L, 1L, 4000L) ))
}

#' @export
fReadTimeSeveralCols <- function(
		### Constructing time from columnns 'year',...,'hour'
		Data.F                ##<< Data frame
		, FileName.s          ##<< NetCDF file name as a string
		, Dir.s               ##<< Directory as a string
		, NcPackage.s         ##<< Name (string) of R NetCDF package
		  ## (implemented for 'RNetCDF' and 'ncdf4')
		, CallFunction.s = '' ##<< Name (string) of function called from
		, colYear = 'year'	  ##<< Name (string) of variable holding the year
		, colMonth = 'month'	##<< Name (string) of variable holding the month
		, colDay = 'day'		  ##<< Name (string) of variable holding the day
		, colHour = 'hour'	  ##<< Name (string) of variable holding the hour
		, defaultHour = 0	  	##<< (numeric) default that is used when colHour = NA
		  ## , when only days are specified
		, ...				          ##<< further arguments to var.get.nc or ncvar_get
		  ## , such as start and count
) {
  ##details<<
  ## Time may be stored in different formats, and \code{\link{fLoadFluxNCIntoDataframe}}
  ## is parameterized by a argument \code{fReadTime}.
  ## The following functions are provided to construct time from different formats:
  ##   These functions help with the preparation of your data for the analysis:
  ##   \itemize{
  ##     \item{ from columnns 'year',...,'hour': fReadTimeSeveralCols (this function) }
  ##     \item{ from column in ISODate integer format: \code{\link{fReadTimeBerkeley}} }
  ##   }
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
  if (!length(colHour) ) {
		Data.F <- fAddNCFVar(
		  Data.F, c(colYear, colMonth, colDay), FileName.s, Dir.s, NcPackage.s
		  , CallFunction.s, VarNew.s = c("year", "month", "day"), ...)
		Data.F$hour <- defaultHour
	} else {
		Data.F <- fAddNCFVar(
		  Data.F, c(colYear, colMonth, colDay, colHour), FileName.s, Dir.s, NcPackage.s
		  , CallFunction.s, VarNew.s = c("year", "month", "day", "hour"), ...)
	}
	Data.F
}

#' @export
fReadTimeBerkeley <- function(
		### Reads time columns (year, month, day, hour) from column in ISODate integer format
		Data.F                ##<< Data frame
		, FileName.s           ##<< NetCDF file name as a string
		, Dir.s                ##<< Directory as a string
		, NcPackage.s          ##<< Name (string) of R NetCDF package
		  ## (implemented for 'RNetCDF' and 'ncdf4')
		, CallFunction.s = ''    ##<< Name (string) of function called from
		, colTime = 'TIMESTAMP_END'	##<< the column name (string) holding time with
		  ## format described in details
		, ...					##<< further arguments to var.get.nc or ncvar_get, such as
		  ## start and count
) {
	##details<<
	## In the Berkeley-Release of the fluxnet data, the time is stored as an integer
	## with base10-digits representing YYYYMMddhhmm
	#
	##seealso<< \code{\link{fReadTimeSeveralCols}}
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
	Data.F <- fAddNCFVar(Data.F, colTime, FileName.s, Dir.s, NcPackage.s
	                     , CallFunction.s, ...)
	timeStampChar <- as.character(Data.F[[colTime]])
	Data.F <- cbind(Data.F, data.frame(
					year = as.integer(substr(timeStampChar, 1, 4))
					, month = as.integer(substr(timeStampChar, 5, 6))
					, day = as.integer(substr(timeStampChar, 7, 8))
					, hour = as.integer(substr(timeStampChar, 9, 10)) +
						as.integer(substr(timeStampChar, 11, 12)) / 60
	))
	#str(Data.F)
	Data.F
}

.tmp.f <- function() {
	# testing if baseDate is stored in nc-file
	# http://stackoverflow.com/questions/18819112/use-r-to-extract-time-series-from-netcdf-data
	InputNCF.s <- REddyProc:::fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
	NCFile.C <- open.nc(InputNCF.s)
	baseDate <- att.get.nc(NCFile.C, "NC_GLOBAL", "base_date")
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fAddNCFVar <- function(
  ##description<<
  ## Add variable from NetCDF file to data frame
  Data.F                ##<< Data frame
  , Var.s                ##<< Variable name or names (vector of strings)
  , FileName.s           ##<< NetCDF file name as a string
  , Dir.s                ##<< Directory as a string
  , NcPackage.s          ##<< Name (string) of R NetCDF package
    ## (implemented for 'RNetCDF' and 'ncdf4')
  , CallFunction.s = ''    ##<< Name (string) of function called from
  , VarNew.s = Var.s		 ##<< Name (string) of the variable in Data.F, offer renaming
  , ...					##<< further arguments to var.get.nc or ncvar_get
    ## , such as start and count
) {
  ##author<< AMM, KS, TW
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
  if (length(VarNew.s) != length(Var.s) ) stop(
    "VarNew.s must have the same length as Var.s")
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
  if (NcPackage.s == 'RNetCDF') {
	  fOpen <- RNetCDF::open.nc
	  fReadVar <- RNetCDF::var.get.nc
	  fClose <- RNetCDF::close.nc
	  #fInqVar <- var.inq.nc
	  fGetAtt <- RNetCDF::att.get.nc
  } else if (NcPackage.s == 'ncdf4') {
	  fOpen <- ncdf4::nc_open
	  fReadVar <- ncdf4::ncvar_get
	  fClose <- ncdf4::nc_close
	  fGetAtt <- function(...) { ncdf4::ncatt_get(...)$value }
  } else {
	  stop(CallFunction.s, ':::fAddNCFVar::: NC file ', InputNCF.s
	     , ' could not be opened!')
  }
  #
	NCFile.C <- fOpen(InputNCF.s)	# NCFile.C <- nc_open(InputNCF.s)
	tmpFilename <- tempfile(); tmpFile <- file(tmpFilename, open = "wt")
	tryCatch({
				newCols <- lapply(seq_along(Var.s), function(i) {
							newCol <- try(as.vector(fReadVar(NCFile.C, Var.s[i], ...)) )
							if (length(newCol) && !inherits(newCol, "try-error")) {
								attr(newCol, 'varnames') <- VarNew.s[i]
								# to prevent error message, that appears even with
								# try(, silent = TRUE) on non-existing attribute
								sink(tmpFile, type = "message")
								try(attr(newCol, 'units') <- fGetAtt(
								  NCFile.C, Var.s[i], 'units'), silent = TRUE)
								sink(NULL, type = "message")
								newCol
							} else {
								warning("could not read variable ", Var.s[i]
								        , " from netCdf-File ", FileName.s)
								return(NULL)
							}
							#attr(Data.F[[1]], 'units')
						})
				names(newCols) <- VarNew.s
				newCols.F <- as.data.frame(newCols[sapply(newCols, length) != 0L])
				# Use c() instead of cbind() to be able to bind dataframe Data.F
				# even if empty
				# twutz160121: c gives a list instead of error when nRows differ
				# between Data.F and newCol.F (e.g. with different count argument to
				# fReadVar), therefore better use cbind
				if (nrow(newCols.F) )
					Data.F <- if (length(Data.F)) cbind(Data.F, newCols.F) else newCols.F
			},
			finally = {
				fClose(NCFile.C)
				close(tmpFile)
				unlink(tmpFilename)
			}
	)
  Data.F
  ##value<<
  ## Data frame with new nc variable added.
}

.tmp.f <- function(){
  Data.F <- NULL; Var.s <- 'NEE'
  FileName.s <- 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
  Dir.s <- 'inst / examples'
  NcPackage.s <- 'ncdf4'
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fLoadFluxNCInfo <- function(
  ##title<<
  ## Get site information from BGI NetCDF files
  ##description<<
  ## Load site information attributes such as latitude, longitude and others
  ## from BGI NetCDF files
  FileName.s               ##<< NetCDF file name as a string
  , Dir.s                  ##<< Directory as a string
  , NcPackage.s = 'ncdf4'  ##<< Name (string) of R NetCDF package
    ## (implemented for 'RNetCDF' and 'ncdf4')
  , CallFunction.s = ''    ##<< Name (string) of function called from
) {
  ##author<< AMM, TW
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
  # Check for R NetCDF packages
  if (!((NcPackage.s == 'ncdf4' && suppressWarnings(requireNamespace("ncdf4")) )
        || (NcPackage.s == 'RNetCDF' && suppressWarnings(requireNamespace("RNetCDF")) )) )
    stop('fLoadFluxNCIntoDataframe::: Required package \'', NcPackage.s
         , '\' could not be loaded!')
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
  if (NcPackage.s == 'RNetCDF') {
    NCFile.C <- RNetCDF::open.nc(InputNCF.s)
    tryCatch({
      ##details<<
      ## Description of attribute list:
      ##describe<<
      SiteInfo.L <- list(
        ID    = RNetCDF::att.get.nc(                  ##<< SiteID
          NCFile.C, 'NC_GLOBAL', 'Site_ID')
        , DIMS = RNetCDF::dim.inq.nc(                 ##<< Number of data rows
          NCFile.C, 'time')$length
        , LON  = as.numeric(RNetCDF::att.get.nc(      ##<< Longitude
          NCFile.C, 'NC_GLOBAL', 'Longitude'))
        , LAT  = as.numeric(RNetCDF::att.get.nc(      ##<< Latitude
          NCFile.C, 'NC_GLOBAL', 'Latitude'))
        , TZ   = as.numeric(RNetCDF::att.get.nc(      ##<< Time zone
          NCFile.C, 'NC_GLOBAL', 'TimeZone'))
        , ELEV = as.numeric(RNetCDF::att.get.nc(      ##<< Elevation
          NCFile.C, 'NC_GLOBAL', 'Elevation'))
        , IGBP = RNetCDF::att.get.nc(                 ##<< IGBP class
          NCFile.C, 'NC_GLOBAL', 'IGBP_class')
      )
    }, finally = RNetCDF::close.nc(NCFile.C))
  } else if (NcPackage.s == 'ncdf4') {
    NCFile.C <- ncdf4::nc_open(
      InputNCF.s, write = FALSE, readunlim = TRUE, verbose = FALSE)
    tryCatch({
      SiteInfo.L <- list(
        ID    = ncdf4::ncatt_get(NCFile.C, 0, 'Site_ID')$value
        , DIMS = NCFile.C$dim$time$len
        , LON  = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'Longitude')$value)
        , LAT  = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'Latitude')$value)
        , TZ   = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'TimeZone')$value)
        , ELEV = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'Elevation')$value)
        , IGBP = ncdf4::ncatt_get(NCFile.C, 0, 'IGBP_class')$value
      )
    }, finally = ncdf4::nc_close(NCFile.C))
  } else {
    stop(CallFunction.s, ':::fLoadFluxNCInfo::: NC file ', InputNCF.s
         , ' could not be opened!')
  }
  ##value<< Attibute list
  SiteInfo.L
}

.tmp.f <- function(){
  FileName.s <- 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
  Dir.s <- '../REddyProc/examples'
  NcPackage.s <- 'ncdf4'
  fLoadFluxNCInfo('Example_DE-Tha.1996.1998.hourly_selVars.nc', Dir.s, 'ncdf4')
}
