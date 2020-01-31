#' Transparent or lighter versions of colors
#' 
#' For each color, create a lighter version either by introducing transparency
#' or by just dimming down.
#' 
#' Same thin is used for all components of RGB. Thus \code{thin=0} always
#' results in white. No value checking is done, so either \code{thin} or
#' \code{alpha} outside \[0,1\] most likely results in error.
#' 
#' @param col A color. Eighter a name such as "red" or preferrably a hex string
#' such as "#FF0000"
#' @param alpha Amount of transparency, 1=not transparent, 0=fully transparent.
#' Recycled.
#' @param thin Amount of lightening, 1=none, 0=white.
#' @return A vector of rgba values suitable for using in graphics.
#' @export
#' @note Not all graphics devices support transparency. Parameter \code{thin}
#' may be helpful in these situations.
#' @author Mikko J Virtanen, Petteri Mäntymaa, Tarja Palosaari, Salla Toikkanen
#' @seealso \code{\link{col2rgb}}, \code{\link{rgb}}
#' @keywords Graphics
#' @examples
#' 
#' thlShade("red", alpha = .40)
#' thlShade(thlColors(type = "main"), alpha = .8, thin = .5)
#'
#' 
thlShade <- function(col, alpha = .25, thin = 1) {
  n <- max(length(col), length(alpha), length(thin))
  col <- rep(col, length = n)
  alpha <- rep(alpha, length = n)
  thin <- rep(thin, length = n)
  colmat <- rbind(grDevices::col2rgb(col), alpha = alpha, thin = thin)
  apply(colmat, 2, function(a) {grDevices::rgb(1 - a[5]*(1 - a[1]/255),
                                               1 - a[5]*(1 - a[2]/255),
                                               1 - a[5]*(1 - a[3]/255),
                                               a[4])}
  )
}

