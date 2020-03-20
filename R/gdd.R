#' Growing Degree Days
#'
#' @description Growing Degree Day (GDD) or Winkler index. Useful as a zoning tool to differentiate between grape varieties and climate (Winkler et al. 1974).
#' @param mx vector of daily maximum temperature series.
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param lati (optional) numeric value indicating the latitude of location.
#' @param iniday initial date in text format ("YYYY-mm-dd"). If "lati" is defined, "iniday" and "endday" are automatically set.
#' @param endday last date in text format ("YYYY-mm-dd"). If "lati" is defined, "iniday" and "endday" are automatically set.
#' @return The sum of growing-degree-days is returned as a numeric value.
#' @details Depending on the latitude, the function detects the hemisphere and considers growing season from 1st April to 31st October (northern hemisphere) or from 1st October to 30rd April (southern hemisphere).

#' @references Winkler AJ, Cook JA, Kliwer WM, Lider LA (1974) General viticulture. University of California Press, Berkeley, CA
#' @examples
#'
#'   gdd(mx = daily_tmax,
#'     mn = daily_tmin,
#'     dates = seq.Date(as.Date('1981-01-01'),
#'             as.Date('2010-12-31'), by ='day'),
#'             lati = 42)
#'
#' @export

gdd <- function(mx, mn, dates, lati = NULL, iniday, endday){

  if(!is.null(lati)){
    if(!is.numeric(lati)){
      stop("'lati' must be a numeric value indicating latitude")
    }
  
    # definition of growing dates depending on hemisphere
    if(lati >= 0){ # northern: apr to sept
      iniday <- '04-01'
      endday <- '10-31'} else{ # southern: oct to mar
        iniday <- '10-01'
        endday <- '04-30'
      }
  }
  days <- dates[which(as.numeric(substr(dates, 6,7))
                      %in%
                        substr(iniday, 1, 2):substr(endday, 1, 2))]

  ff <- function(x){
    round(
      sum(
        apply(x, 1, function(temps){
          max(((temps[1] + temps[2]) / 2) - 10, 0)
        })
      )
    )
  }

  x <- cbind(mx, mn)
  years <- unique(substr(days, 1, 4))
  gddres <- numeric()
  for(i in 1:length(years)){
    xx <- x[match(days[which(substr(days, 1, 4) == years[i])], dates), ]
    gddres[i] <- ff(xx)
  }
  return(gddres)
}
