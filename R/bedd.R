#' Biologically effective degree-days (BEDD)
#'
#' @description Biologically effective degree-days (BEDD). Gladstones, J. (1992).
#' @param mx vector of daily maximum temperature series.
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param lati numeric value indicating the latitude of location.
#' @details Depending on the latitude, the function detects the hemisphere and considers growing season from 1st April to 31st October (northern hemisphere) or from 1st October to 30rd April (southern hemisphere).
#' @return The sum of degree-days (BEDD) is returned as a numeric value.

#' @references Gladstones, J. (1992) Viticulture and environment (Winetitles: Adelaide).
#' @examples
#'
#'   bedd(mx = daily_tmax,
#'     mn = daily_tmin,
#'     dates = seq.Date(as.Date('1981-01-01'),
#'             as.Date('2010-12-31'), by ='day'),
#'             lati = 42)
#'
#' @export

bedd <- function(mx, mn, lati, dates){

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


  ff <- function(xx, lati){
    if(lati < 40 | lati > 50) K = 1 else{
      int <- approx(40:50, seq(1.0,1.045, (1.045-1.0)/10), n = 200)
      K <- int$y[which(abs(int$x - lati) == min(abs(int$x - lati)))]
    }
    if(is.na(sum(xx[,1:2]))) return(NA) else{
      round(
        sum(
          apply(xx, 1, function(temps, K){
            if(temps[3] > 13){
              dtr_adj <- 0.25 * (temps[3] - 13)
            } else if(temps[3] >= 10 & temps[3] <= 13){
              dtr_adj <- 0
            } else if(temps[3] < 10){
              dtr_adj <- 0.25 * (temps[3] - 10)
            }
            min(((max(((temps[1] + temps[2])/2) - 10, 0)* K) + dtr_adj), 9)
          }, K)
        )
      )
    }
  }

  x <- cbind(mx, mn, mx-mn)

  years <- unique(substr(days, 1, 4))
  beddres <- numeric()
  for(i in 1:length(years)){
    yeardays <- days[which(substr(days, 1, 4) == years[i])]
    xx <- x[match(yeardays, dates), ]
    beddres[i] <- ff(xx, lati)
  }
  return(beddres)
}
