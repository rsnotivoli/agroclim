#' Pentadic probabilities
#'
#' @description This function calculates the probability od occurrence of a temperature range in a five-day row (pentad).
#' @param mx vector containing the daily maximum temperature series.
#' @param mn same as mx but with daily minimum temperature series.
#' @param temp_dates sequence of daily dates coinciding with temperature data series.
#' @param out system path to the generated output file ('.pdf').
#' @param min_abs minimum absolute temperature used as threshold to plot data.
#' @param max_abs same as min_abs but with maximum absolute temperature.
#' @param plot_years numeric vector indicating the frequencies of individual years to plot over the averages.
#' @param show_legend logical value indicating wether legend will be shown or not.
#' @return If the output path is defined, a pdf file is created.
#' @details The numer of individual years to plot over the average frequencies is unlimited. However, it is recommended to add only a few due to the difficult of reading a lot of information in the same plot.
#'
#' The colors of the individual years are random, so it is possible to get different colors for same years in two runs of same plot.
#'
#'
#' @examples
#'
#'
#' # basic plot with legend
#'  pentadProbs(mx = daily_tmax, mn = daily_tmin,
#'               temp_dates = seq.Date(as.Date('1981-01-01'),
#'               as.Date('2010-12-31'), by ='day'),
#'               out = NULL, min_abs = -20, max_abs = 44,
#'               plot_years = NULL, show_legend = TRUE)
#'
#' # years 1981 and 2009 are plotted over the average frequencies
#'  pentadProbs(mx = daily_tmax, mn = daily_tmin,
#'               temp_dates = seq.Date(as.Date('1981-01-01'),
#'               as.Date('2010-12-31'), by ='day'),
#'               out = NULL, min_abs = -20, max_abs = 44,
#'               plot_years = c(1981, 2009), show_legend = TRUE)
#'
#' @import ggplot2
#' @import abind
#' @import stats
#' @importFrom  cowplot plot_grid save_plot
#' @importFrom ggpubr get_legend ggarrange
#' @importFrom grDevices colorRampPalette
#' @import ggforce
#' @importFrom reshape melt
#' @export

pentadProbs <- function(mx, mn, temp_dates,
                         out = NULL, min_abs = -20,
                         max_abs = 44, plot_years = NULL,
                         show_legend = TRUE){

  if(sum(diff(sapply(list(mx, mn, temp_dates), length))) != 0){
    stop("'temp_dates', 'tmax' and 'tmin' data series mut have same length")
  }

  #global variables
  ordered_intervals <- pentads <- alphas <- NULL
  freqs <- pos <- val <- NULL
  alp <- x0 <- y0 <- NULL
  r <- cols <- NULL
  
  years <- unique(substr(temp_dates, 1, 4))

  if(!is.null(plot_years)){
    if(!all(plot_years%in%years)){
      stop("'plot_years' must be within the range of recorded years")
    } else {
      plot_indiv <- TRUE
    }
  } else {
    plot_indiv <- FALSE
  }

  # thresholds for max and min temp.
  thresmin <- seq((floor(min(mn, na.rm = T)) + floor(min(mn, na.rm = T)) %% 2),
                  (ceiling(max(mn, na.rm=T)) - ceiling(max(mn, na.rm=T)) %% 2),
                  2)
  thresmax <- seq((floor(min(mx, na.rm = T)) + floor(min(mx, na.rm = T)) %% 2),
                  (ceiling(max(mx, na.rm=T)) - ceiling(max(mx, na.rm=T)) %% 2),
                  2)

  # function to compute temperature occurences
  ff <- function(x, thres){
    pentads <- sort(rep(1:length(seq(1, 365, 5)), 5))
    # if leap a year exists, 1 day is added to pentad 12 (end of February)
    if(length(x) == 366) pentads <- c(pentads[1:55], 12, pentads[56:365])
    mat <- data.frame(temp = x, pentad = pentads)
    return(with(mat, table(cut(temp, breaks = thres), pentads)))
  }

  # compute occurrences for each year

  lmin <- lmax <- list()
  for(i in 1:length(years)){
    lmin[[i]] <- ff(mn[which(temp_dates == paste0(years[i], '-01-01')):
                        which(temp_dates == paste0(years[i], '-12-31'))],
                   thresmin)
    lmax[[i]] <- ff(mx[which(temp_dates == paste0(years[i], '-01-01')):
                           which(temp_dates == paste0(years[i], '-12-31'))],
                    thresmax)
  }
  names(lmin) <- names(lmax) <- years
  allmin <- abind::abind(lmin, along = 3)
  allmax <- abind::abind(lmax, along = 3)
  
  # probabilities
  # fp <- function(x) sum(x) / (length(x)*5)
  fp <- function(x) {1-(length(which(x <= 1)) / length(x))}
  
  mintab <- melt(t(apply(allmin, c(1,2), fp)))
  maxtab <- melt(t(apply(allmax, c(1,2), fp)))
  #############
  
  # colnames(mintab) <- colnames(maxtab) <- c('pentads','intervals','mean', 'sd', 'prob')
  colnames(mintab) <- colnames(maxtab) <- c('pentads','intervals', 'prob')
  mintab[mintab==0] <- NA
  maxtab[maxtab==0] <- NA

  # create color scale based on limits from arguments
  all <- seq((floor(min_abs) + floor(min_abs) %% 2),
             (ceiling(max_abs) - ceiling(max_abs) %% 2),
             2)
  all_scale <- paste0(all[1:length(all)-1],',',all[2:length(all)])
  colfunc <- colorRampPalette(rev(c("red","yellow","green4","royalblue","midnightblue","darkorchid4")))
  # plot(rep(1, n), col = (colfunc(length(all_scale))), pch = 19, cex = 2)
  colscale <- cbind(all_scale, colfunc(length(all_scale)))

  #****************
  # labels of pentads
  pent_labels <- substr(seq.Date(as.Date('1901-01-01'), as.Date('1901-12-31'), by ='day'), 6, 10)
  ini_pent_labels <- pent_labels[seq(1, 365, 5)]
  ini_pent_labels <- paste0(month.abb[as.numeric(substr(ini_pent_labels, 1,2))], substr(ini_pent_labels, 3,5))
  end_pent_labels <- pent_labels[seq(5, 365, 5)]
  end_pent_labels <- paste0(month.abb[as.numeric(substr(end_pent_labels, 1,2))], substr(end_pent_labels, 3,5))
  pent_labels <- paste(ini_pent_labels, end_pent_labels, sep =' to ')
  
  #****************
  #tmin

  # order data
  mintab$pentads <- factor(mintab$pentads)
  ints <- as.character(unique(mintab$intervals))
  for(i in 1:length(ints)){ mintab$ordered_intervals[which(ints[i] == mintab$intervals)] <- i}

  #assign color scale
  colores <- colscale[match(substr(mintab$intervals, 2, (nchar(as.character(mintab$intervals))-1)), colscale[, 1]),2]
  names(colores) <- factor(as.character(mintab$ordered_intervals))

  # create alpha scale
  # mintab$alphas <- NA
  # mintab$alphas[which(mintab$freqs >= 1)] <- 0.9 # more than 1 day per year
  # mintab$alphas[which(mintab$freqs >= 0.5 & mintab$freqs < 1)] <- 0.5 # 1 day in 2 years
  # mintab$alphas[which(mintab$freqs >= 0.25 & mintab$freqs < 0.5)] <- 0.2 # 1 day in 4 years
  # mintab$alphas[which(mintab$freqs >= 0.20 & mintab$freqs < 0.25)] <- 0.05 # 1 day in 5 years
  # mintab$alphas[which(mintab$freqs < 0.20)] <- 0 # less than 1 day in 5 or more years
  mintab$alphas <- mintab$prob
  
  mintab <- mintab[-c(which(is.na(mintab$prob))), ]

  p1 <- ggplot(mintab, aes(x = ordered_intervals, y = pentads, label = factor(ordered_intervals))) +
    geom_tile(aes(fill = factor(ordered_intervals), alpha = alphas), show.legend = FALSE) +
    scale_fill_manual(values = colores) +
    scale_y_discrete(limits = rev(sort(unique(mintab$pentads))), labels = rev(pent_labels)) +
    scale_x_continuous(position = 'top',
                       breaks = unique(mintab$ordered_intervals),
                       labels = unique(as.character(mintab$intervals))) +
    theme(panel.border = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey", size = 0.05),
          title=element_text(size=10),
          axis.text=element_text(size=8),
          axis.title=element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          plot.margin = unit(c(0.6,0.2,0.1,0.2), "cm")
          )

  #****************
  #tmax

  # order data
  maxtab$pentads <- factor(maxtab$pentads)
  ints <- as.character(unique(maxtab$intervals))
  for(i in 1:length(ints)){ maxtab$ordered_intervals[which(ints[i] == maxtab$intervals)] <- i}

  #assign color scale
  colores <- colscale[match(substr(maxtab$intervals, 2, (nchar(as.character(maxtab$intervals))-1)), colscale[, 1]),2]
  names(colores) <- factor(as.character(maxtab$ordered_intervals))

  # create alpha scale
  # maxtab$alphas <- NA
  # maxtab$alphas[which(maxtab$freqs >= 1)] <- 0.9 # more than 1 day per year
  # maxtab$alphas[which(maxtab$freqs >= 0.5 & maxtab$freqs < 1)] <- 0.5 # 1 day in 2 years
  # maxtab$alphas[which(maxtab$freqs >= 0.25 & maxtab$freqs < 0.5)] <- 0.2 # 1 day in 4 years
  # maxtab$alphas[which(maxtab$freqs >= 0.20 & maxtab$freqs < 0.25)] <- 0.05 # 1 day in 5 years
  # maxtab$alphas[which(maxtab$freqs < 0.20)] <- 0 # less than 1 day in 5 or more years
  maxtab$alphas <- maxtab$prob

  maxtab <- maxtab[-c(which(is.na(maxtab$prob))), ]

  p2 <- ggplot(maxtab, aes(x = ordered_intervals, y = pentads, label = factor(ordered_intervals))) +
    geom_tile(aes(fill = factor(ordered_intervals), alpha = alphas), show.legend = FALSE) +
    scale_fill_manual(values = colores) +
    scale_y_discrete(limits = rev(sort(unique(maxtab$pentads))), labels = rev(pent_labels)) +
    scale_x_continuous(position = 'top',
                       breaks = unique(maxtab$ordered_intervals),
                       labels = unique(as.character(maxtab$intervals))) +
    theme(panel.border = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey", size = 0.05),
          title=element_text(size=10),
          axis.text=element_text(size=8),
          axis.title=element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text.y=element_blank(),
          # axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.6,0.2,0.1,0), "cm")
          )


  # adding individual years to the plot (if selected)
  if(plot_indiv){
    colyearsfunc <- colorRampPalette(c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999'))
    cols_to_legend <- vector()
    for(y in 1:length(plot_years)){
      coly <- sample(colyearsfunc(length(years)), 1)
      cols_to_legend <- c(cols_to_legend, coly)
      y_min <- t(lmin[[which(names(lmin) == as.character(plot_years[y]))]])
      y_min <- melt(y_min)
      colnames(y_min) <- c('pentads','intervals','freqs')
      ints <- as.character(unique(y_min$intervals))
      y_min <- y_min[-c(which(y_min$freqs == 0)), ]
      y_min$pentads <- factor(y_min$pentads)
      y_min$ordered_intervals <- NA
      for(i in 1:length(ints)){ y_min$ordered_intervals[which(ints[i] == y_min$intervals)] <- i}
      p1 <- p1 + geom_point(data = y_min,
                      mapping = aes(x = ordered_intervals,
                                    y = pentads, size = freqs),
                      pch=21,
                      colour = coly,
                      show.legend = FALSE) +
        scale_size(range = c(0, 4))

      y_max <- t(lmax[[which(names(lmax) == as.character(plot_years[y]))]])
      y_max <- melt(y_max)
      colnames(y_max) <- c('pentads','intervals','freqs')
      y_max <- y_max[-c(which(y_max$freqs == 0)), ]
      y_max$pentads <- factor(y_max$pentads)
      ints <- as.character(unique(y_max$intervals))
      y_max$ordered_intervals <- NA
      for(i in 1:length(ints)){ y_max$ordered_intervals[which(ints[i] == y_max$intervals)] <- i}
      p2 <- p2 + geom_point(data = y_max,
                            mapping = aes(x = ordered_intervals,
                                          y = pentads, size = freqs),
                            pch=21,
                            colour = coly,
                            show.legend = FALSE) +
        scale_size(range = c(0, 4))
    }

  }

  if(show_legend){
    # df <- data.frame(pos= rep(1:length(all_scale), 5),
    #                  val = sort(rep(1:5, length(all_scale))),
    #                  alp = sort(rep(c(0, 0.05, 0.2, 0.5, 0.9), length(all_scale)))
    las <- length(all_scale)
    df <- data.frame(pos= rep(1:las, 10),
                     val = sort(rep(1:10, las)),
                     alp = sort(rep(seq(0, 0.9, 0.1), las))
    )
    # colfunc <- colorRampPalette(rev(c("red","yellow","green4","royalblue","midnightblue","darkorchid4")))
    colores <- colfunc(las)
    names(colores) <- factor(as.character(1:las))
    
    meanprobs <- data.frame(int = all_scale, min = NA, max = NA, minsd = NA, maxsd = NA)
    ag <- aggregate(mintab$prob, by =list(mintab$intervals), FUN ='mean')
    meanprobs[match(substr(ag$Group.1, 2, nchar(as.character(ag$Group.1))-1), all_scale), 2] <- ag[, 2]
    ag <- aggregate(maxtab$prob, by =list(maxtab$intervals), FUN ='mean')
    meanprobs[match(substr(ag$Group.1, 2, nchar(as.character(ag$Group.1))-1), all_scale), 3] <- ag[, 2]
    ag <- aggregate(mintab$prob, by =list(mintab$intervals), FUN ='sd')
    meanprobs[match(substr(ag$Group.1, 2, nchar(as.character(ag$Group.1))-1), all_scale), 4] <- ag[, 2]
    ag <- aggregate(maxtab$prob, by =list(maxtab$intervals), FUN ='sd')
    meanprobs[match(substr(ag$Group.1, 2, nchar(as.character(ag$Group.1))-1), all_scale), 5] <- ag[, 2]
    
    leg1 <- ggplot(df, aes(x = pos, y = val, fill = pos)) +
      geom_raster(aes(alpha = alp), interpolate = TRUE) +
      scale_fill_gradientn(colours = colores) +
      
      geom_point(data = meanprobs, aes(x = 1:las, y = min*10), color='blue') +
      geom_line(data = meanprobs, aes(x = 1:las, y = min*10), color='blue') +
      geom_ribbon(data = meanprobs, aes(x = 1:las, ymin=(meanprobs$min-meanprobs$minsd)*10, 
                                        ymax=(meanprobs$min+meanprobs$minsd)*10), linetype=2, alpha=0.3, fill = 'blue') +
      geom_point(data = meanprobs, aes(x = 1:las, y = max*10), color='red') +
      geom_line(data = meanprobs, aes(x = 1:las, y = max*10), color='red') +
      geom_ribbon(data = meanprobs, aes(x = 1:las, ymin=(meanprobs$max-meanprobs$maxsd)*10, 
                                        ymax=(meanprobs$max+meanprobs$maxsd)*10), linetype=2, alpha=0.3, fill = 'red') +
      
      scale_x_continuous(breaks = 1:las, labels = all_scale, expand=c(0, 0)) +
      scale_y_continuous(breaks = 1:10, labels = (1:10)/10, expand=c(0, 0)) +
      ylab("Probability") +
      theme(axis.line=element_blank(),
            axis.text=element_text(size=7),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            # axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size = 9),
            axis.line.x = element_line(size = 0.2, linetype = "solid", colour = "black"),
            axis.line.y = element_line(size = 0.2, linetype = "solid", colour = "black"),
            title=element_text(size=10),
            legend.position="none",
            panel.background=element_blank(),
            # panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            plot.margin = unit(c(0.2,-1.3,0.4,1.8), "cm")
      )

      if(plot_indiv){
        # frequencies sizes and colors
        circles <- data.frame(
          x0 = 1:5,
          y0 = rep(1, 5),
          r = seq(0, 0.1, length.out = 5)
        )
        leg2 <- ggplot() +
          geom_circle(aes(x0=x0, y0=y0, r=r), data=circles) +
          coord_fixed() +
          scale_x_continuous(breaks = 1:5,
                             labels = c('1', '2', '3', '4', '5'),
                             limits = c(0, 6)) +
          theme(axis.line=element_blank(),
                axis.text=element_text(size=9),
                # axis.text.x = element_text(angle = 45),
                axis.text.y = element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                title=element_text(size=10),
                legend.position="none",
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank(),
                plot.margin = unit(c(0.4,2,-1,2), "cm")
          ) +
          ylim(0.8,2) +
          annotate(geom = 'text', x = 1, y = 1.5,
                   label = 'Occurrence (number of days)', size = 3,
                   hjust = 0)

        # colors and labels for years
        df_years <- data.frame(pos = rep(1:10, length(plot_years)), cols = cols_to_legend)
        leg3 <- ggplot(df_years) +
          geom_line(aes(x = pos, y = pos, colour = cols)) +
          xlim(0, 10) +
          ylim(0, 100) +
          scale_color_manual(labels = as.character(plot_years), values = cols_to_legend) +
          theme_bw() +
          labs(color='Year') +
          theme(legend.position = 'bottom',
                legend.title = element_text(size =8),
                legend.text = element_text(size =8))
          geom_blank()
        leg3 <- get_legend(leg3)

        # layout
        pg1 <- ggarrange(p1, p2, ncol = 2, nrow = 1, align = 'h',
                         labels = c("Minimum temperature", "Maximum temperature"),
                         font.label = list(face = "plain", size = 11),
                         widths = c(1, 0.76))
        pg2 <- ggarrange(leg2, leg3, ncol = 1, nrow = 2)
        pg3 <- ggarrange(leg1, pg2, ncol =2, nrow =1)
        pg <- ggarrange(pg1, pg3, ncol = 1, nrow = 2, heights = c(1.7, 0.4))

      } else {
        # layout
        pg1 <- ggarrange(p1, p2, ncol = 2, nrow = 1, align = 'h',
                         labels = c("Minimum temperature", "Maximum temperature"),
                         font.label = list(face = "plain", size = 11),
                         widths = c(1, 0.76))
        leg1 <- leg1 + theme(plot.margin = unit(c(0.2,2.3,0.4,2.3), "cm"))
        pg <- ggarrange(pg1, leg1, ncol = 1, nrow = 2, heights = c(1.7, 0.4))
      }
  } else {
    pg <- plot_grid(p1, p2,
                    nrow  = 1,
                    align = 'h',
                    axis  = 'tb',
                    labels = c("Minimum temperature", "Maximum temperature"),
                    label_size = 11,
                    label_fontface = "plain",
                    rel_widths = c(1, 0.76)
    )
  }

  if(!is.null(out)){ # export to pdf
    save_plot(out, pg, nrow = 1, base_asp = 1, base_height = 9)
  } else{
    pg
  }

}





