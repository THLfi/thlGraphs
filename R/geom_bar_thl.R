# Wrappers for barplots


#' THL wrapper for `geom_col()`
#'
#' Wrapper provides better defaults to match with graphical guidelines. 
#' Defaults affect to parameters: color and width.
#'
#' Uses white border for and gap width between bars is 42\% of the bar width.
#' Use parameter `size` to change border into smaller.
#'
#' @author Oskari Luomala
#' @inheritParams ggplot2::geom_col
#' @param width Bar width. 
#' The gap should be between 30% and 50% of the bar's width. 
#' Formula: gap_pct = (1 - width) / width, therefore: width = 1 / (1 + gap_pct).
#' Thus width parameter should be between 0.667 and 0.769.
#' @param colour Border colour of bars. Defaults to "white".
#' @seealso [geom_col()] and [geom_bar()]
#' @import ggplot2
#' @export
#'
#' @examples
#' data(yli180)
#' 
#' # Minimal plot built from thlGraphs elements
#' ggplot(yli180, aes(kk, pros)) + 
#'   geom_col_thl(fill = colors_thl("dark.green")) + 
#'   scale_y_continuous_thl(limits = c(0,2)) + 
#'   theme_thl() +
#'   labs(title = "Erikoissairaanhoidosssa yli 180 vrk odottaneet",
#'        y = "osuus (%)", x = "",
#'        caption = c(expression(paste(bold("Lähde:"), " THL"))))
geom_col_thl <- function(mapping = NULL, data = NULL, position = "stack", ...,
                         width = 0.7, na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, colour = "white") {
  geom_col(mapping = mapping, data = data, position = position, ..., 
           colour = colour, width = width,  
           na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
}


#' @rdname geom_col_thl  
geom_bar_thl <- function(mapping = NULL, data = NULL, stat = "count",
                         position = "stack", ..., width = 0.7, na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE) {
  geom_bar(mapping = mapping, data = data, stat = stat, position = position,
           ..., width = width, na.rm = na.rm, show.legend = show.legend, 
           inherit.aes = inherit.aes)
}
