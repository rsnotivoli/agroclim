#' Frequency of frosts
#'
#' @description This function calculates the frequency of frost occurrence, by month.
#' @param mn vector containing the daily minimum temperature series.
#' @param dates sequence of daily dates coinciding with temperature data series.
#' @param thres temperature threshold considered to trigger frost occurrence (0 by default).
#' @param out system path to the generated output file ('.pdf').
#' @param iniday first day of the year ("dd-mm") to consider for first and last occurrence of frost.
#' @param endday last day of the year ("dd-mm") to consider for first and last occurrence of frost.
#' @details Despite the logical threshold of temperature is 0 ºC to determine frost occurrence, the argument "thres" is open to change in case of different units of temperature.
#' @return If the output path is defined, a pdf file is created.
#' @examples
#'\donttest{
#' frostFreqs(mn = daily_tmin, dates = seq.Date(as.Date('1981-01-01'),
#'                              as.Date('2010-12-31'), by ='day'), thres = 0)
#'}
#' @import zoo
#' @import stats
#' @importFrom reshape melt
#' @importFrom grDevices dev.off pdf
#' @export


frostFreqs <- function(mn, dates, thres = 0, out = NULL, iniday = '07-01', endday = '06-30'){
  
  #global variables
  value <- month <- days <- NULL
  x <- zoo(mn, dates)
  nfrosts <- function(x, thres){
    freqs <- matrix(0, ncol = 31, nrow = 12)
    colnames(freqs) <- 1:31
    for(i in 1:12){
      d <- as.numeric(x[which(as.numeric(substr(time(x), 6, 7)) == i)])
      d[d <= thres] <- 'frost'
      d[d != 'frost'] <- 'no frost'
      r <- rle(d)
      r <- r$lengths[which(r$values == 'frost')]
      r <- table(r)
      freqs[i,match(as.numeric(names(r)), 1:31)] <- as.numeric(r)
    }
    freqs
  }

  years <- unique(substr(dates, 1, 4))
  lf <- list()
  for(y in 1:length(years)){
    lf[[y]] <- nfrosts(x = x[which(years[y] == substr(time(x), 1, 4))],
                        thres = thres)
  }
  allf <- abind::abind(lf, along = 3)

  allf <- apply(allf, c(1,2), sum, na.rm = T)
  
  # what percentage represents each day for its corresponding month
  allf <- t(apply(allf, 1, function(x) {x * 100 / sum(x, na.rm = T)}))
  
  allf <- melt(allf)
  names(allf) <- c('month', 'days', 'value')
 
  # frost probability, by month
  pf <- numeric()
  for(i in 1:12){
    w <- which(as.numeric(as.character(time(x), format = '%m')) == i)
    ec <- ecdf(as.numeric(x[w]))
    pf[i] <- ec(thres)
  }
  
  for(i in 1:12){
    w <- which(allf$month == i)
    allf$value[w] <- allf$value[w] * pf[i]
  }

  cols <- rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))
  colfunc <- colorRampPalette(cols)

  allf$days <- factor(allf$days)
  for(i in 2:31) allf$days <- relevel(allf$days, i) #reorder levels
  
  aord <- allf[order(allf$days, decreasing = T), ]
  aord$month <- factor(aord$month, levels = c(7:12, 1:6))
  
  pg <- ggplot(aord, aes(y = value, x = month, fill = days)) +
    # geom_bar(stat = "identity", position = position_nudge(x = 0.5)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "Frost spell length in days (frequency of occurrence)", values=rev(colfunc(31))) +
    scale_x_discrete(name = "", labels = month.abb[c(7:12, 1:6)]) +
    scale_y_continuous(name = "Probability (%)", breaks = seq(0,100, 10), limits = c(0, 100)) +
    geom_vline(xintercept=seq(0.5, 12.5, 1), 
               lwd=0.2, colour="grey") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.spacing.x = unit(-0.1,"cm"),
          legend.key.size = unit(1,"lines"),
          legend.title=element_text(size = 10),
          legend.text=element_text(size = 7),
          axis.title.x=element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.margin=margin(0,0,0,0)) +
    guides(fill=guide_legend(ncol = 31,
                             label.position = 'bottom',
                             label.hjust = 0,
                             title.position = "top",
                             reverse = TRUE))

  ############
  # plot of first and last frost occurrence probability
  
  frostyears <- aggregate(x, by = list(substr(dates, 1, 4)), function(x) length(which(x < thres)))
  frostprob <- length(which(frostyears > 0)) / length(frostyears)
  # first frost probability
  ff <- firstFrost(mn, dates, iniday = iniday, endday = endday, type = 'doy', thres = thres)
  ffun <- sort(unique(ff))
  ec <- ecdf(ff)
  # plot(ec)
  pff <- sapply(ffun, ec) * frostprob

  # last frost probability
  lf <- lastFrost(mn, dates, iniday = iniday, endday = endday, type = 'doy', thres = 0)
  lfun <- sort(unique(lf))
  ec <- ecdf(lf)
  # plot(ec)
  plf <- (1-sapply(lfun, ec)) * frostprob
  
  pfrost <- data.frame(day = seq.Date(as.Date('2019-07-01'), 
                                      as.Date('2020-06-30'), by ='day'), 
                       first = NA, last = NA)
  pfrost$first[ffun] <- pff
  pfrost$last[lfun] <- plf
  pfrost <- melt(pfrost, id = c('day'))
  
  pf <- ggplot(data=pfrost, aes(x=day, y=value, colour=variable)) +
    geom_point() +
    geom_line(data = pfrost[!is.na(pfrost$value),]) +
    theme_void() +
    # theme_bw() +
    scale_color_manual(values = c('blue2', 'darkviolet'), 
                       labels = c("Before", "After"),
                       name="Date of frost probability") +
    ylim(0,1) +
    # adjust x-axis
    xlim(as.Date(c(min(pfrost$day)+15,max(pfrost$day)-15), format="%Y/%m/%d")) +
    theme(legend.position = c(0.17, 0.9),
          legend.title=element_text(size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
  
    guides(colour=guide_legend(ncol=2))
  
  
  gt <- combine_plots(pg, pf)
  
  if(!is.null(out)){
    pdf(out, height = 6, width = 7, useDingbats = F)
      plot(gt)
    dev.off()
  } else{
    plot(gt)
  }
  
}




