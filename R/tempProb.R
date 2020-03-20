#' Probability of exceed a predefined temperature value
#'
#' @description Calculates the first day in the year where the probability of temperature over a threshold is higher than a predefined threshold.
#' @param mx vector of daily (usually maximum) temperature series.
#' @param dates vector of dates corresponding with daily temprature series
#' @param thres temperature threshold considered to trigger occurrence.
#' @param month integer between 1 and 12 indicating the month to compute the probability.
#' @return A numeric vector with annual values is returned.
#' @examples
#'
#'   tempProb(mx = daily_tmax,
#'   dates = seq.Date(as.Date('1981-01-01'),
#'                              as.Date('2010-12-31'), by ='day'),
#'   thres = 20,
#'   month = NULL)
#'
#'
#' @export

tempProb <- function(mx, dates, thres = 20, month = NULL) {

  if(!as.numeric(thres)){
    stop("'thres' must be a numeric value")
  }
  x <- zoo(mx, dates)

  if(is.null(month)){
    p <- round(pnorm(thres, mean(x), sd(x), lower.tail = FALSE), 2)
  } else if(as.numeric(month)){
    if(month >=1 & month <= 12){
      w <- which(as.numeric(as.character(time(x), format = '%m')) == month)
      p <- round(pnorm(thres, mean(x[w]), sd(x[w]), lower.tail = FALSE), 2)
    } else{
      stop("'month' must be an integer between 1 and 12")
    }
  } else {
    stop("'month' must be an integer between 1 and 12")
  }
   return(p)
}
