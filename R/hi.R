#' Huglin Heliothermal Index (HI)
#'
#' @description Huglin Heliothermal Index (HI). Useful as a zoning tool (Huglin 1978).
#' @param mx vector of daily maximum temperature series.
#' @param mn vector of daily minimum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param lati numeric value indicating the latitude of location.
#' @return A numeric vector with annual values is returned.
#' @details Depending on the latitude, the function detects the hemisphere and considers growing season from 1st April to 30rd September (northern hemisphere) or from 1st October to 31st March (southern hemisphere).

#' @references Huglin P. (1978) Nouveau mode d'evaluation des possibilities heliothermiques d'un milieu viticole. CR Acad Agr 64: 1117–1126
#' @examples
#'
#'   hi(mx = daily_tmax,
#'     mn = daily_tmin,
#'     dates = seq.Date(as.Date('1981-01-01'),
#'             as.Date('2010-12-31'), by ='day'),
#'             lati = 42)
#'
#' @export

hi <- function(mx, mn, dates, lati){

  if(!is.numeric(lati)){
    stop("'lati' must be a numeric value indicating latitude")
  }

  # definition of growing dates depending on hemisphere
  if(lati >= 0){ # northern: apr to sept
    iniday <- '04-01'
    endday <- '09-30'} else{ # southern: oct to mar
      iniday <- '10-01'
      endday <- '03-31'
    }
  days <- dates[which(as.numeric(substr(dates, 6,7))
                       %in%
                         substr(iniday, 1, 2):substr(endday, 1, 2))]

  # K calculation
  ## this is a correction of the original K value proposed by (Hall and Jones, 2010)
  ## Hall, A., and G.V. Jones. 2010. Spatial analysis of climate in wine-grape growing regions in Australia. Aust. J. Grape Wine Res. doi: 10.1111/j.1755-0238.2010.00100.x
  ## daylength (Glarner, 2006). http://www.gandraxa.com/length_of_day.xml
  # axis <- 0.409 # obliquity of the ecliptic in radians. Constant value since it slowly changes through thousands of years.
  # if(lat >= 0) inijday <- '12-21' else inijday <- '06-21' # initial julian day depending on hemisphere (winter solstice)
  # jdays <- numeric()
  # years <- as.numeric(unique(substr(days, 1, 4)))
  # for(i in 1:length(years)){
  #   sub <- days[which(substr(days, 1, 4) == years[i])]
  #   seldates <- seq.Date(as.Date(paste(years[i]-1,inijday, sep = '-')), as.Date(sub[length(sub)]), by = 'day')
  #   jdays <- c(jdays, match(sub, seldates))
  # }
  # m_lat_day <- 1 - (tan(lat) * tan(axis * cos((pi * jdays) / 182.625)))
  # daylength <- (acos(1 - m_lat_day) / pi) * 24
  # totalseasondaylength <- aggregate(daylength, by = list(substr(days, 1, 4)), FUN = 'sum')[, 2]
  # # K calculation (modified)
  # K <- (0.00028311 * totalseasondaylength) + 0.30834

  ff <- function(x, lati){
    if(lati < 40 | lati > 50) K = 1 else{
      int <- approx(40:50, seq(1.02,1.06, (1.06-1.02)/10), n = 200)
      K <- int$y[which(abs(int$x - lati) == min(abs(int$x - lati)))]
    }
    round(
      sum(
        apply(xx, 1, function(temps, K){
          max(((temps[1] - 10) + (temps[2] - 10))/2, 0) * K
        }, K)
      )
    )
  }

  x <- cbind(((mx + mn) / 2), mx)
  years <- unique(substr(days, 1, 4))
  hires <- numeric()
  for(i in 1:length(years)){
    xx <- x[match(days[which(substr(days, 1, 4) == years[i])], dates), ]
    hires[i] <- ff(xx, lati)
  }
  return(hires)
}
