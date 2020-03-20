#' Creation of NetCDF from an array
#'
#' @description Creates a NetCDF file from an array with values.
#' @param arr array containing the values to export in netcdf.
#' @param varunit character. units in which variable is measured.
#' @param varname character. name of the variable
#' @param longname character. long name of the variable
#' @param lons numeric. vector of longitudes.
#' @param lats numeric. vector of latitudes.
#' @param inidate date. initial day of observations.
#' @param scale character. temporal units for the variable (usually "days", "months" or "years")
#' @param out character. path to the output file.
#' @return A NetCDF file is created in the path specified in the "out" parameter.
#' @examples
#'
#' \donttest{
#' createNc(arr = hi,
#'           varunit = 'ºC',
#'           varname = 'hi',
#'           longname = 'Huglin Index',
#'           lons = lon,
#'           lats = lat,
#'           inidate = '1980-01-01',
#'           scale = 'years',
#'           out = 'hi.nc')
#'  }
#'
#' @export

createNc <- function(arr, varunit, varname, longname, lons, lats, inidate = NULL, scale = NULL, out){

  #netcdf creation
  londim <- ncdim_def("lon", "degrees_east", as.double(lons))
  latdim <- ncdim_def("lat", "degrees_north", as.double(lats))
  if(!is.null(scale)){
    if(!is.null(inidate)){
      timedim <- ncdim_def( "time", paste0(scale, " since ",inidate), 1:dim(arr)[3], unlim = TRUE)
    } else {
      stop("inidate must be stated")
    }
  } else{
    timedim <- ncdim_def( "time", "timeframe was not defined", 1:dim(arr)[3], unlim = TRUE)
  }

  fillvalue <- 1e32
  tmp_def <- ncvar_def(varname, units = varunit, list(londim, latdim, timedim),
                       fillvalue, longname, prec = "single",
                       compression = 9)

  #file structure creation and input data
  ncout <- nc_create(filename = out, vars = list(tmp_def), force_v4 = TRUE)

  # assignment of the variables
  ncvar_put(ncout, tmp_def, arr)

  # assignment of the additional attributes
  ncatt_put(ncout, "lon", "axis", "X")
  ncatt_put(ncout, "lat", "axis", "Y")
  ncatt_put(ncout, "time", "axis", "T")

  #close the file
  nc_close(ncout)
}
