#' Warmest month of the year
#'
#' @description Calculates the mean temperature of the warmest month of the year
#' @param mx vector of daily maximum temperature series.
#' @param dates vector of dates corresponding with daily temperature series
#' @param type type of output. It can be "temp" for temperature or "month" for the number of the warmest month.
#' @param ... any other argument. It is expected to be "na.rm" in case NA values are required to be removed.
#' @return Depending on argument type, the output will be a numeric value with the month (type = "month") or with temperature (type = "temp").
#'
#' @examples
#'
#'
#' warmMonth(mx = daily_tmax,
#'            dates = seq.Date(as.Date('1981-01-01'),
#'             as.Date('2010-12-31'), by ='day'),
#'            type = 'temp',
#'            na.rm = TRUE)
#'
#' @export


warmMonth <- function(mx, dates, type = 'temp', ...){
  if(all(is.na(mx))){
    z <- NA
  } else{
      ag <- aggregate(mx, by = list(substr(dates, 1, 7)), FUN = 'mean', ...)
      ag <- aggregate(ag[, 2], by = list(substr(ag[, 1], 6, 7)), FUN = 'mean', ...)
      if(type == 'temp') z <- max(ag[, 2])
      if(type == 'month') z <- which.max(ag[, 2])
    }
  return(z)
}
