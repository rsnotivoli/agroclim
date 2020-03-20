#' Number of frost days
#'
#' @description Calculates the number of frost days within a predefined period.
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param iniday first day of the year ("dd-mm") when frost occurrence will be considered.
#' @param endday last day of the year ("dd-mm") when frost occurrence will be considered.
#' @param thres temperature threshold considered to trigger frost occurrence (0 by default).
#' @details Despite the logical threshold of temperature is 0 ºC to determine frost occurrence, the argument "thres" is open to change in case of different units of temperature.
#' @return A numeric vector with the annual number of frost days is returned.
#' @examples
#'
#'
#' frostDays(mn = daily_tmin,
#'            dates = seq.Date(as.Date('1981-01-01'),
#'                              as.Date('2010-12-31'), by ='day'),
#'            iniday = '07-01',
#'            endday = '06-30')
#'
#' @import zoo
#' @export


frostDays <- function(mn, dates, iniday = '07-01', endday = '06-30', thres = 0) {

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
        z[i] <- length(which(as.numeric(x)[st:en] < thres))
      }
  return(z)
}
