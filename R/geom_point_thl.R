# Wrappers for plotting points

#' THL wrapper for `geom_point()`
#'
#' Wrapper provides better defaults to match with graphical guidelines. 
#' Changeable default parameters are: \code{fill, shape, linewidth}.
#' Hidden changeable default parameters are: \code{size, stroke}
#'
#' @author Oskari Luomala
#' 
#' @inheritParams ggplot2::geom_point
#' @param fill Geom point fill color.
#' @param shape Shape of points, defaults to filled circle (21).
#' @param linewidth Line width size in pt, defaults to 3 pts. 
#'  Affects both size and stroke parameters.
#' @param size Line width in mm, overwrites linewidth parameter if spesified.
#' @param stroke Line width of points' border, overwrites linewidth parameter 
#'  if spesified.
#'  
#' @export
#' @examples
#' # TODO
geom_point_thl <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ...,na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE,
                           fill = "white", shape = 21, linewidth = 3) {
  lwd <- thlPtsConvert(linewidth)
  size <- 10/3 * lwd
  stroke <- 1.35 * lwd
  
  args <- list(...)
  if (!is.null(args$size)) {
    size <- args$size
  }
  if (!is.null(args$stroke)) {
    stroke <- args$stroke
  }
  
  geom_point(mapping = mapping, data = data, stat = stat,
             position = position, 
             fill = fill, shape = shape, size = size, stroke = stroke,
             ..., na.rm = na.rm, show.legend = show.legend,
             inherit.aes = inherit.aes)
}

