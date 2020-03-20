#' Mean first frost day
#'
#' @description Calculates the first frost day within a predefined period.
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param iniday first day of the year ("dd-mm") when frost occurrence will be considered.
#' @param endday last day of the year ("dd-mm") when frost occurrence will be considered.
#' @param type type of output. It can be "doy" for day of the year (julian day) or "date" for data format ("dd-mm").
#' @param thres temperature threshold considered to trigger frost occurrence (0 by default).
#' @details The function is able to span over years. If the argument "enddate" is a date earlier than "inidate", the considered period will cover from "inidate" to the end of the year and from the beginning of the next year to "enddate".
#' @return Depending on argument type, the output will be a numeric vector of julian days (type = "doy") or a vector of characters with dates (type = "date").
#'
#' @examples
#'
#'
#' firstFrost(mn = daily_tmin,
#'             dates = seq.Date(as.Date('1981-01-01'),
#'                              as.Date('2010-12-31'), by ='day'),
#'             iniday = '07-01',
#'             endday = '06-30',
#'             type = 'doy')
#' @import zoo
#' @export


firstFrost <- function(mn, dates, iniday = '07-01', endday = '06-30', type = 'doy', thres = 0) {
      
      x <- zoo(mn, dates)
      wini <- which(as.character(time(x), format = '%m-%d') == iniday)
      wend <- which(as.character(time(x), format = '%m-%d') == endday)
      if(wend[1] < wini[1]){
        wend <- wend[2:length(wend)]
      }
      z <- rep(NA,length(wini))
      for (i in 1:(length(wini))) {
        st <- wini[i]
        if(i == length(wini)){
          en <- length(x)
        } else {
            en <- wend[i]
          }
        f <- which(as.numeric(x)[st:en] < thres)
        if(length(f) == 0) next
        if(type == 'doy'){
          z[i] <- min(f)
        } else if(type == 'date'){
          z[i] <- substr(index(x[st:en])[min(f)], 6, 10)
        }
      }
  return(z)
}
