#' Growing Season Temperature (GST)
#'
#' @description Growing Season Temperature (GST). Mean daily temperature in growing season.
#' @param mx vector of daily maximum temperature series.
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param lati numeric value indicating the latitude of location.
#' @return A numeric vector with annual values is returned.
#' @details Depending on the latitude, the function detects the hemisphere and considers growing season from 1st April to 31st October (northern hemisphere) or from 1st October to 30rd April (southern hemisphere).

#' @references Jones G, Duff A, Hall A, Myers J (2010) Spatial Analysis of Climate in Winegrape Growing Regions in the Western United States. Am. J. Enol. Vitic. 61:3.
#' @examples
#'
#'   gst(mx = daily_tmax,
#'     mn = daily_tmin,
#'     dates = seq.Date(as.Date('1981-01-01'),
#'             as.Date('2010-12-31'), by ='day'),
#'             lati = 42)
#'
#' @export

gst <- function(mx, mn, lati, dates){

  mm <- (mx + mn) / 2

  if(!is.numeric(lati)){
    stop("'lati' must be a numeric value indicating latitude")
  }

  # definition of growing dates depending on hemisphere
  if(lati >= 0){ # northern: apr to oct
    iniday <- '04-01'
    endday <- '10-31'} else{ # southern: oct to mar
      iniday <- '10-01'
      endday <- '04-30'
    }
  days <- dates[which(as.numeric(substr(dates, 6,7))
                       %in%
                         substr(iniday, 1, 2):substr(endday, 1, 2))]
  years <- unique(substr(days, 1, 4))
  gstres <- numeric()
  for(i in 1:length(years)){
    xx <- mm[match(days[which(substr(days, 1, 4) == years[i])], dates)]
    gstres[i] <- mean(xx, na.rm = T)
  }
  return(round(gstres, 2))
}
