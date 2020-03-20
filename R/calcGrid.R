#' Calculation of indices over gridded datasets
#'
#' @description Calculates any of the agroclimatic indices over a multidimensional dataset
#' @param mxnc a path to a NetCDF file(s) with maximum temperature data.
#' @param mnnc a path to a NetCDF file(s) with minimum temperature data.
#' @param varnametx a character string specifying the name of the variable to extract in maximum temperature NetCDF file.
#' @param varnametn a character string specifying the name of the variable to extract in minimum temperature NetCDF file.
#' @param fun name of the function to apply over the netcdf file.
#' @param inienddays character vector indicating the initial and ending dates in which the input data must be splitted. Dates must be provided in format "YYY-MM-DD".
#' @param dates vector of dates corresponding with daily temprature series.
#' @param ncpu number of parallel processes to spawn for the use for parallel computation in multiple cores.
#' @param out name of the NetCDF file where results will be saved (extension ".nc" must be included).
#' @param ... specific arguments matching the arguments of the target funcion.
#' @details The "inienddays" argument only works if the input daily temperature ncdf file includes the time dimension in the format of "days since YYY-MM-DD". Otherwise, the function will return an error. All the arguments of the target function (function to be applied over the ncdf) must be stated ("...") or an unespecified error will appear. The function uses the argument "ncpu" to split the tasks into different cores taking advantage of the hardware. 
#' @return A NetCDF file is created in the path specified in the "out" parameter.
#' @examples
#'\donttest{
#'  calcGrid(mxnc = NULL, mnnc = 'tmin.nc', varnametn = 'tn', fun = 'frostProb', 
#'           inienddays = NULL, dates = dates, ncpu = 4,  
#'           out = 'fd_prob.nc', iniday = '07-01', endday = '06-30', 
#'           type = 'doy', thres = 0, prob = 0.10)
#'}
#' @import ncdf4
#' @import multiApply
#' @import easyNCDF
#' @import tools
#' @export


calcGrid <- function(mxnc = NULL, mnnc = NULL, varnametx = NULL, varnametn = NULL, fun, 
                     inienddays, dates, ncpu = 2, out = 'test.nc', ...){
  
  # Parameters dependency based on function
  params <- list(...)
  if(any(as.character(fun) == c('bedd', 'gdd', 'gst', 'hi'))){
    str <- paste0("Apply(data = list(mx, mn, latitudes), 
                      margins = c(1, 2), fun = ", fun, 
                  ", dates = dates, ncores = ncpu)[[1]]" )
    varunit <-'c'
    
  } else if(any(as.character(fun) == c('coldMonth', 'firstFrost', 
                                       'frostDays', 'frostProb', 'lastFrost',
                                       'ehe', 'firstTemp', 'tempDayprob', 
                                       'tempProb', 'warmMonth'))){
    str <- paste0("Apply(data = mn, margins = c(1, 2), fun =", 
                  fun, ", dates = dates, ncores = ncpu,")
    if(length(params) > 0){
      pars <- paste0(names(params)[1], '=', deparse(params[[1]]), ',')
      if(length(params) > 1){
        for(p in 2:length(params)){
          if(p == length(params)){
            pars <- paste0(pars, names(params)[p], '=', deparse(params[[p]]))
          } else{
            pars <- paste0(pars, names(params)[p], '=', deparse(params[[p]]), ',')
          }
        }
      }
    } else{
      stop(paste0("Additional parameters are required to compute ", fun, "()"))
    }
    str <- paste0(str, pars, ")[[1]]")
    
    if(any(as.character(fun) == c('coldMonth', 'warmMonth'))){
      varunit <- 'c'
    } else if(any(as.character(fun) == c('firstFrost', 'frostDays', 'frostProb', 'lastFrost',
                                         'firstTemp', 'tempDayprob', 'tempProb'))){
      varunit <- 'doy'
    } else if(as.character(fun) == 'ehe'){
      varunit <- ''
    }
  }
  
  # maximum temperature
  # check for file type
  if(!is.null(mxnc)){
    if(file_ext(mxnc) == 'nc'){
      ncfile <- NcOpen(mxnc)
      ncfile_dims <- names(NcReadDims(ncfile))

      # check whether name of variable exists
      if(is.null(varnametx)){
        stop("'varnametx' must be defined")
      } else if(length(varnametx) > 1){
        stop("'varname' must be of length 1")
      } else if(length(which(varnametx == NcReadVarNames(ncfile))) == 0) {
        stop("'varname' must be the name of one variable in the ncdf file")
      }

      # variable to array
      message("Retrieving maximum temperature data from NetCDF file")
      # constrain by dates?
      if(is.null(inienddays)){
        var <- NcToArray(ncfile, vars_to_read = varnametx)
        var <- drop(var)
      } else{
        if(is.character(inienddays)){
          # time dimension
          timedim <- ncfile_dims[match('time', tolower(ncfile_dims))]
          nc_time <- ncvar_get(ncfile, timedim)
          tunits <- ncatt_get(ncfile, timedim, "units")$value
          if(grep('days since', tunits) > 0){
            dates <- as.Date(nc_time, origin = gsub('days since ', '',
                                                    tunits))
            m <- match(inienddays, as.character(dates))
            if(length(which(is.na(m))) > 0){
              stop("dates provided in 'inienddays' don't coincide
                   with dates of data")
            } else {
              var <- NcToArray(ncfile, vars_to_read = varnametx)
              var <- drop(var)
              var <- var[, , m[1]:m[2]]
            }
          } else {
            stop("time dimension is not in the format of
                 'days since YYY-MM-DD'")
          }
        } else{
          stop("'inienddays' must be a character vector of 2 elements")
        }
      }

      # coordinates
      londim <- ncfile_dims[match('lon', tolower(ncfile_dims))]
      latdim <- ncfile_dims[match('lat', tolower(ncfile_dims))]
      nc_lons <- ncvar_get(ncfile, londim)
      nc_lats <- ncvar_get(ncfile, latdim)

        # layer of latitudes
        latitudes <- matrix(nc_lats, nrow = length(nc_lons),
                            ncol = length(nc_lats), byrow = T)

      NcClose(ncfile)

      # tmax
      mx <- var
      rm(var)

    } else{
      stop("'mxnc' must be a path to a NetCDF file")
      }
  }
    # minimum temperature
    # check for file type
    if(!is.null(mnnc)){
      if(file_ext(mnnc) == 'nc'){
        ncfile <- NcOpen(mnnc)
        ncfile_dims <- names(NcReadDims(ncfile))

        # check whether name of variable exists
        if(is.null(varnametn)){
          stop("'varnametn' must be defined")
        } else if(length(varnametn) > 1){
          stop("'varname' must be of length 1")
        } else if(length(which(varnametn == NcReadVarNames(ncfile))) == 0) {
          stop("'varname' must be the name of one variable in the ncdf file")
        }

        # variable to array
        message("Retrieving minimum temperature data from NetCDF file")
        # constrain by dates?
        if(is.null(inienddays)){
          var <- NcToArray(ncfile, vars_to_read = varnametn)
          var <- drop(var)
        } else{
          if(is.character(inienddays)){
            # time dimension
            timedim <- ncfile_dims[match('time', tolower(ncfile_dims))]
            nc_time <- ncvar_get(ncfile, timedim)
            tunits <- ncatt_get(ncfile, timedim, "units")$value
            if(grep('days since', tunits) > 0){
              dates <- as.Date(nc_time, origin = gsub('days since ', '',
                                                      tunits))
              m <- match(inienddays, as.character(dates))
              if(length(which(is.na(m))) > 0){
                stop("dates provided in 'inienddays' don't
                     coincide with dates of data")
              } else {
                var <- NcToArray(ncfile, vars_to_read = varnametn)
                var <- drop(var)
                var <- var[, , m[1]:m[2]]
              }
            } else {
              stop("time dimension is not in the format of
                   'days since YYY-MM-DD'")
            }
          } else{
            stop("'inienddays' must be a character vector of 2 elements")
          }
        }

        # coordinates
        londim <- ncfile_dims[match('lon', tolower(ncfile_dims))]
        latdim <- ncfile_dims[match('lat', tolower(ncfile_dims))]
        nc_lons <- ncvar_get(ncfile, londim)
        nc_lats <- ncvar_get(ncfile, latdim)

        if(!exists("latitudes")){
        # layer of latitudes
        latitudes <- matrix(nc_lats, nrow = length(nc_lons),
                            ncol = length(nc_lats), byrow = T)
        }

        NcClose(ncfile)

        # tmin
        mn <- var
        rm(var)

      } else{
        stop("'mnnc' must be a path to a NetCDF file")
      }
  }


      message("Computing indicator")
      res <- eval(parse(text = str))

      if(length(dim(res)) == 3){
        res <- aperm(res, c(2, 3, 1))
        createNc(arr = res, varname = fun, lons = nc_lons,
                  lats = nc_lats, out = out, varunit = varunit, longname = '')
      } else if(length(dim(res)) == 2){
        res <- replicate(1, res)
        createNc(arr = res, varname = fun, lons = nc_lons,
                  lats = nc_lats, out = out, varunit = varunit, longname = '')
      }
      
}
