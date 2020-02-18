#' Extreme Heat Exposure (EHE)
#'
#' @description Extreme Heat Exposure (EHE). Useful for climatic risks assessement on wheat and barley.
#' @param mx vector of daily maximum temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param op character. Indicates whether the output will be in date or numeric format.
#' @details Adapted from Trnka et al. (2014). Event is triggered when the Tmax is above +35°C for at least three days during the period from five days after anthesis (supposed to be May-1st) to maturity (suposed to be July-31st). The minimum daily temperature is usually measured 2 m above ground; thus, the actual crop temperature might be even lower.
#' @return If op = "first", the function returns the first day (date format) when the first event is triggered. If op =='doy', julian day is returned. If op = "number", the funciton returns the number of events occurred in the year.

#' @references Trnka M, Rotter RP, Ruiz-Ramos M, Kersebaum KC, Olesen JE, Zalud Z, Semenov MA (2014) Adverse weather conditions for European wheat production will become more frequent with climate change. Nature Climate Change volume 4, pages 637–643.
#' @examples
#'
#'             ehe(mx = daily_tmax,
#'             dates = seq.Date(as.Date('1981-01-01'),
#'                     as.Date('2010-12-31'), by ='day'),
#'             op = 'first')
#'
#' @import zoo
#' @export

ehe <- function(mx, dates, op = 'first'){

  ff <- function(xx, dd, op){
    if(is.na(sum(xx))) daythres <- NA else{
      xx <- zoo(xx, dd)
      wini <- which(as.character(time(xx), format = '%d-%m') == '01-05')
      wend <- which(as.character(time(xx), format = '%d-%m') == '31-07')
      f <- rle(as.numeric(xx)[wini:wend] > 35)
      w <- which(which(f$values) > 3)
      if(length(w) > 0){
        daythres <- numeric()
        for(i in 1:length(w)){
          daythres[i] <- as.character(time(xx[wini:wend][sum(f$lengths[1:(w[i] - 1)])+1]), format = '%d-%m')
        }
      } else {daythres <- NULL}
    }
    if(op == 'first'){
      if(is.null(daythres)){
        return(NA)
      } else{
          return(daythres[1])
        }
    } else if(op == 'doy'){
      if(is.null(daythres)){
        return(NA)
      } else{
        w <- which(as.Date(paste0('1901-',daythres[1]), '%Y-%d-%m') ==
                seq.Date(as.Date('1901-01-01'), as.Date('1901-12-31'), by ='day'))
        return(w)
      }
    } else if(op == 'number'){
      return(length(daythres))
    }
  }

  years <- unique(substr(dates, 1, 4))
  eheres <- numeric()
  for(i in 1:length(years)){
    dd <- dates[which(substr(dates, 1, 4) == years[i])]
    xx <- mx[match(dd, dates)]
    eheres[i] <- ff(xx, dd, op = op)
  }
  return(eheres)
}
