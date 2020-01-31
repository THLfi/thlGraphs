# THL's scale wrappers for ggplot2



#' Make axis breaks prettier than default ggplot
#' 
#' Computes a sequence of equally spaced pretty values which cover range of the values in `x`
#' 
#' 
#' @param x vector which range should be covered by breaks
#' @param n	 number of desired breaks
#' @param ... other \code{\link[base]{pretty}} parameters
#'
#' @export
#' @examples
#' breaks_thl(c(0,140))
#' breaks_thl(c(0,20))
breaks_thl <- function(x, n = 8, ...) {
  pretty(x, n = n, ...)
}


#' Formatting numbers in finnish, swedish and in english format
#' 
#' Formats vector of numbers
#' 
#' @param x Numeric vector to be formatted.
#' @param language Changes big number separator and decimal separator. 
#' Possible values are: "fi", "se", "en".
#' @param ... other \code{\link[base]{format}} parameters
#' 
#' @export
#' @examples
#' vec <- breaks_thl(c(0,2))
#' format_thl(vec, "fi")
#' format_thl(vec, "en")
format_thl <- function(x, language = "fi", ...) {
  sep.big <- switch(language,
                    "fi" = " ",
                    "se" = " ",
                    "en" = ",",
                    " ")
  sep.dec <- switch(language,
                    "fi" = ",",
                    "se" = ",",
                    "en" = ".",
                    ",")
  format(x, big.mark = sep.big, decimal.mark = sep.dec, scientific = FALSE, ...)
}


#' THL wrapper for `scale_x_continuous()`
#' 
#' Removes expanding from lower limit on x-scale.
#' Also makes breaks prettier using more break points than default ggplot
#' 
#' 
#' 
#' @param limits A numeric vector of length two providing limits of the scale. 
#' Use NA to refer to the existing minimum or maximum.
#' @param expand Expansion of limits, vector or see [expand_scale()]
#' @param language Language formatting of breaks.
#' @param labels Text labels for axis breaks.  
#' @param ... other [scale_x_continuous()] parameters
#' @param breaks Breaks on axis. See [breaks_thl()] how the default prettier breaks are made.
#' @seealso stuff
#'
#' @export
#' @examples
#' #TODO
scale_x_continuous_thl <- function(limits = NULL, expand = waiver_thl(), 
                                   language = "fi", labels = waiver_thl(),
                                   ..., breaks = waiver_thl()) {
  if (is.waiverthl(labels)) {
    labels <- function(x) {format_thl(x, language = language)}
  }
  if (is.waiverthl(breaks)) {
    breaks <- breaks_thl
  } 
  scale_x_continuous(breaks = breaks, limits = limits, 
                     expand = expand, labels = labels,...)
}


#' THL wrapper for `scale_x_continuous()`
#' 
#' Removes expanding from lower limit on y-scale.
#' Formats axis break numbers in language spesified way
#' Also makes breaks prettier using more break points than default ggplot
#' 
#' 
#' 
#' @param limits A numeric vector of length two providing limits of the scale. 
#' Use NA to refer to the existing minimum or maximum.
#' @param expand Expansion of limits, vector or see [expand_scale()]
#' Defaults to expanding y upper end by 0.05 multiplier.
#' @param language Language formatting of breaks.
#' @param labels Text labels for axis breaks.
#' @param ... other [scale_y_continuous()] parameters
#' @param breaks Breaks on axis. 
#' See [breaks_thl()] how the default prettier breaks are made.
#'
#' @export
#' @examples
#' #TODO
scale_y_continuous_thl <- function(limits = NULL, expand = waiver_thl(), 
                                   language = "fi", labels = waiver_thl(),
                                   ..., breaks = waiver_thl()) {
  #if (is.null(expand)) {
  if (is.waiverthl(expand)) {
    expand <- expand_scale(mult = c(0, 0.05), add = c(0, 0))
  }
  if (is.waiverthl(labels)) {
    labels <- function(x) {format_thl(x, language = language)}
  }
  if (is.waiverthl(breaks)) {
    breaks <- breaks_thl
  } 
  scale_y_continuous(breaks = breaks, limits = limits, 
                     expand = expand, labels = labels, ...)
}


