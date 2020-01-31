# Wrappers for lineplots

#' THL wrapper for `geom_line()`
#' 
#' Wrapper provides better defaults to match with graphical guidelines. 
#' Changeable default parameters are \code{linewidth, size}
#'
#' @author Oskari Luomala
#' 
#' @inheritParams ggplot2::geom_line
#' @param linewidth Line width size in pt
#' @param size Line width in mm, overwrites linewidth parameter if not NULL

#' @export
#' @examples
#' # TODO
geom_line_thl <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, 
                          show.legend = NA, inherit.aes = TRUE, ...,
                          linewidth = 3, size = NULL) {
  # Use size parameter instead of linewidth, if size is spesified
  size_p <- thlPtsConvert(linewidth)
  if (!is.null(size)) {
    size_p <- size
  }
  geom_line(mapping = mapping, data = data, stat = stat,
            position = position, na.rm = na.rm, show.legend = show.legend,
            inherit.aes = inherit.aes, size = size_p, ...)
}
