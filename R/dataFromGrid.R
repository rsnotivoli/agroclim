#' Extract data from a gridded dataset
#'
#' @description This function extracts data from a gridded dataset based on given coordinates.
#' @param grid a RasterBrick or RasterStack object or a path to a NetCDF file.
#' @param varname a character string specifying the name of the variable to extract in NetCDF file.
#' @param lon numeric vector with longitude(s) of target location(s).
#' @param lat numeric vector with latitude(s) of target location(s).
#' @param times logical. If TRUE, time dimension will be extracted from data and the returned values will be in form of a zoo object. Only works for NetCDF and RasterBrick.
#' @details The argument time searches the time dimension and its units in the gridded dataset (NetCDF or RasterBrick). If it can't be found, only the data will be returned.
#'
#' Arguments lon and lat define the coordinates that must be in the same projection as the gridded dataset. If there is no spatial coincidence, returned object will be empty.
#'
#' @return A matrix with same columns as pairs of coordinates defined in lon and lat arguments. If times is TRUE, the matrix will be a zoo object.
#'
#' @examples
#'
#'\donttest{
#'  dataFromGrid(grid = "../data/tmax.nc",
#'                  varname = 'tn',
#'                  lon = c(41.39, 42),
#'                  lat = c(2.17, 2.5),
#'                  times = FALSE)
#'}
#' @import ncdf4
#' @import easyNCDF
#' @import sp
#' @importFrom raster extract getZ
#' @importFrom tools file_ext
#' @import zoo
#' @export


dataFromGrid <- function(grid, varname, lon, lat, times = FALSE){
  if(length(lon) != length(lat)){
    stop("'lon' and 'lat' must have same length")
  }
  # check for file type
  if(is.character(grid)){
    if(file_ext(grid) == 'nc'){
      ncfile <- NcOpen(grid)
      ncfile_dims <- names(NcReadDims(ncfile))

      # check whether name of variable exists
      if(is.null(varname)){
        # varname <- NcReadVarNames(ncfile)
        # m <- match(varname, names(ncfile_dims))
        # wna <- which(is.na(m))
        # if(length(wna) > 0) m <- m[-c(wna)]
        # if(length(m) > 0) varname <- varname[-c(m)]
        stop("'varname' must be defined")
      } else if(length(varname) > 1){
        stop("'varname' must be of length 1")
      } else if(length(which(varname == NcReadVarNames(ncfile))) == 0) {
        stop("'varname' must be the name of one variable in the ncdf file")
      }

      # variable to array
      message("Retrieving data from NetCDF file")
      var <- NcToArray(ncfile, vars_to_read = varname)
      var <- drop(var)

      # coordinates
      londim <- ncfile_dims[match('lon', tolower(ncfile_dims))]
      latdim <- ncfile_dims[match('lat', tolower(ncfile_dims))]
      nc_lons <- ncvar_get(ncfile, londim)
      nc_lats <- ncvar_get(ncfile, latdim)

      # extract data
      comp_coords <- expand.grid(nc_lons, nc_lats)
      lt <- sapply(1:length(lat), function(i){
        which(comp_coords[which.min(abs(comp_coords[, 2] - lat[i])),
                          2] == nc_lats)})
      ln <- sapply(1:length(lon), function(i){
        which(comp_coords[which.min(abs(comp_coords[, 1] - lon[i])),
                          1] == nc_lons)})
      ext_coords <- data.frame(lon = ln, lat = lt)
      if(times){
        # time dimension
        timedim <- ncfile_dims[match('time', tolower(ncfile_dims))]
        nc_time <- ncvar_get(ncfile, timedim)
        tunits <- ncatt_get(ncfile, timedim, "units")$value
        if(grep('days since', tunits) > 0){
          dates <- as.Date(nc_time, origin = gsub('days since ', '', tunits))
          NcClose(ncfile)
          res <- apply(ext_coords, 1, function(x) var[x[1],x[2], ])
          return(zoo(res, dates))
        } else {
          warning("time dimension is not in the format of 'days since YYY-MM-DD'")
          res <- apply(ext_coords, 1, function(x) var[x[1],x[2], ])
          return(res)
        }
      } else{
          NcClose(ncfile)
          res <- apply(ext_coords, 1, function(x) var[x[1],x[2], ])
          return(res)
        }


    } else {
      stop("'grid' must be a path to a NetCDF file or a Raster object")
    }
  } else if(!any(c('RasterBrick', 'RasterStack') %in% class(grid))){
    stop("'grid' must be a path to a NetCDF file or a Raster object")
    } else{
      df <- data.frame(lon = lon, lat = lat)
      coordinates(df) <- c("lon", "lat")
      message("Retrieving data from raster")
      res <- extract(grid, df)
      res <- t(res)

      if(times){
        if(class(grid) == 'RasterBrick'){
          zoo(res, getZ(grid))
        } else{
            return(res)
          }
      } else {
        return(var)
        }

      }
}
