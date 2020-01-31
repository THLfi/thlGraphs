# Functions for making THL defaults.

#' A THL waiver object.
#' 
#' THL's default "flag" object similar to `waiver()`.
#' @seealso \code{\link[ggplot2]{waiver}}
#' 
#' @export
waiver_thl <- function() {
  structure(list(), class = c("waiver", "waiverthl"))
}


#' Reports whether x is a waiverthl object.
#' 
#' Used in setting defaults for some parameters.
#' @seealso \code{\link{waiver_thl}}
#' 
#' @param x object to be checked
#' 
#' @export
#' @examples
#' is.waiverthl(waiver_thl())
is.waiverthl <- function(x) {
  inherits(x, "waiverthl")
}
