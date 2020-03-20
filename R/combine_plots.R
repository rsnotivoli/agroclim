#' Combine plots
#'
#' @description This function combines two plots build with ggplot2 and use both legends.
#' @param plot1 First plot to draw.
#' @param plot2 Second plot that will be drawed over the first.
#' @return A gtable object.
#' @examples
#'
#' library(ggplot2)
#' df <- mtcars[, c("disp", "qsec", "cyl")]
#' plot1 <- ggplot(df, aes(x=cyl, y=qsec)) +
#'          geom_point() + theme_bw()
#' plot2 <- ggplot(df, aes(x=disp, y=qsec, color = factor(cyl))) +
#'          geom_line() + theme_void() + theme(legend.position = c(0.9, 0.8))
#' cb <- combine_plots(plot1, plot2)
#' plot(cb)
#'
#' @importFrom gtable gtable_add_grob
#' @export


combine_plots <- function(plot1, plot2) {
  # build plots
  bp1 <- ggplot_build(plot1)
  bp2 <- ggplot_build(plot2)
  # extract grobs
  tablep1 <- ggplot_gtable(bp1)
  tablep2 <- ggplot_gtable(bp2)
  # extract layout info
  tp1ly <- tablep1$layout
  tp2ly <- tablep2$layout
  # extract positions
  margins <- tp1ly[which(tp1ly$name == 'panel'), 1:4]
  # build table from positions of the two plots
  exp <- gtable_add_grob(tablep1, grobs = tablep2$grobs[[which(tp2ly$name == 'panel')]],
                         t = margins$t, l = margins$l, b = margins$b, r = margins$r, name = "1")
  # add legend from plot2
  exp <- gtable_add_grob(exp, grobs = tablep2$grobs[[which(tp2ly$name == 'guide-box')]],
                         t = margins$t, l = margins$l, b = margins$b, r = margins$r, name = "2")
  exp
}
