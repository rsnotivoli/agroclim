#' Coldest month of the year
#'
#' @description Calculates the mean temperature of the coldest month of the year
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temperature series
#' @param type type of output. It can be "temp" for temperature or "month" for the number of the coldest month.
#' @param ... any other argument. It is expected to be "na.rm" in case NA values are required to be removed.
#' @return Depending on argument type, the output will be a numeric value with the month (type = "month") or with temperature (type = "temp").
#' @examples
#'
#'
#' coldMonth(mn = daily_tmin,
#'            dates = seq.Date(as.Date('1981-01-01'),
#'             as.Date('2010-12-31'), by ='day'),
#'            type = 'temp',
#'            na.rm = TRUE)
#'
#' @export

coldMonth <- function(mn, dates, type = 'temp', ...){
  if(all(is.na(mn))){
    z <- NA
  } else{
      ag <- aggregate(mn, by = list(substr(dates, 1, 7)), FUN = 'mean', ...)
      ag <- aggregate(ag[, 2], by = list(substr(ag[, 1], 6, 7)), FUN = 'mean', ...)
      if(type == 'temp') z <- min(ag[, 2])
      if(type == 'month') z <- which.min(ag[, 2])
    }
  return(z)
}
