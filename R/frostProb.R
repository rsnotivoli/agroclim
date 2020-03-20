#' First day in the year where P(tmin<0) <= prob
#'
#' @description Calculates the first day in the year where the probability if frost is under a threshold.
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param iniday first day of the year ("dd-mm") when frost occurrence will be considered.
#' @param endday last day of the year ("dd-mm") when frost occurrence will be considered.
#' @param type type of output. It can be "doy" for day of the year (julian day) or "date" for data format ("dd-mm").
#' @param thres temperature threshold considered to trigger frost occurrence (0 by default).
#' @param prob numeric. Threshold indicating the probability (0.10 by default)
#' @return A numeric vector with the annual number of frost probability is returned.
#' @details The function is able to span over years. If the argument "enddate" is a date earlier than "inidate", the considered period will cover from "inidate" to the end of the year and from the beginning of the next year to "enddate".#' @return depending on argument type, the output will be a numeric vector of julian days (type = "doy") or a vector of characters with dates (type = "date").
#'
#' @examples
#'
#' frostProb(mn = daily_tmin,
#'   dates = seq.Date(as.Date('1981-01-01'),
#'                              as.Date('2010-12-31'), by ='day'),
#'   iniday = '07-01',
#'   endday = '06-30',
#'   type = 'date',
#'   prob = 0.10)
#'
#' @import zoo
#' @export

frostProb <- function(mn, dates, iniday = '07-01', endday = '06-30', type = 'doy', thres = 0, prob = 0.10) {

  prob <- 1 - prob
  z <- lastFrost(mn, dates, iniday, endday, type = 'doy', thres)
  p <- quantile(z, prob, na.rm = T)
  if(type == 'doy'){
    return(p)
  } else if(type == 'date'){
    ycomp <- seq.Date(as.Date('1900-01-01'), as.Date('1900-12-31'), by ='day')
    return(substr(ycomp[p], 6, 10))
  }
}
