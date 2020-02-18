#' Overlap plots
#'
#' @description This function overlay two plots build with ggplot2 and use both legends. This function was extracted from: https://stackoverflow.com/questions/40901169/ggplot2-overlay-two-plots-and-use-both-legends
#' @param base_plt base plot.
#' @param over_plt Overlapping plot.
#' @examples
#'
#' # Examples can be found at:
#' # https://stackoverflow.com/questions/40901169/ggplot2-overlay-two-plots-and-use-both-legends
#'
#' @author "Constantine" and "Sandy Muspratt" users from StackOverflow
#' @importFrom gtable gtable_add_grob
#' @importFrom plyr defaults
#' @export


overlap_plots <- function(base_plt, over_plt) {
  
  plot_theme <- function(p) {
    plyr::defaults(p$theme, theme_get())
  }
  
  base_g = ggplot_gtable(ggplot_build(base_plt))
  overlay_g = ggplot_gtable(ggplot_build(over_plt))
  
  plt_panel = c(subset(base_g$layout, name == "panel", se = t:r))
  pnl_ind = which(overlay_g$layout$name == "panel")
  leg_ind = which(overlay_g$layout$name == "guide-box") 
  final_grob = gtable_add_grob(base_g,
                               overlay_g$grobs[[pnl_ind]],
                               plt_panel$t,
                               plt_panel$l,
                               plt_panel$b,
                               plt_panel$r, name = "a")
  
  final_grob = gtable_add_grob(final_grob,
                               overlay_g$grobs[[leg_ind]],
                               plt_panel$t,
                               plt_panel$l,
                               plt_panel$b,
                               plt_panel$r, name = "b")
  return(final_grob)
}
