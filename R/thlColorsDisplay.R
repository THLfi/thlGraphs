#' Display THL colours
#' 
#' @param n a numerical value or a vector describing how many colours of the palette should be shown
#' @inheritParams thlColors
#' @import ggplot2
#' @author Salla Toikkanen, Petteri Mäntymaa, Tarja Palosaari
#' @import ggplot2
#' @seealso  \code{\link{thlColors}}, \code{\link{thlShade}}
#' 
#' @examples 
#' ## show 5 colours from the quantitative colour palette S2
#' thlColorsDisplay(n = 5, type = "quanti", name = "S2")
#' 
#' ## show 8 THL main colours with some transparency
#' thlColorsDisplay(n = 8, type = "main", alpha = 0.8)
#' 
#' ## show THL qualitative colours
#' thlColorsDisplay(n = 1:10, type = "quali", name = NULL)
#' 
#' ## THL qualitative colours in the order in which they should be used in line plots
#' thlColorsDisplay(n = 1:6, type = "quali", name = "line")
#' 
#' ## Attempt to generate 11 colours from twoway quantitative palette K1 yields an error
#' \dontrun{
#' thlColorsDisplay(n = 11, type = "twoway", name = "K1")
#' }
#' ## Allowing interpolation 11 colours can be generated, although not recommended
#' thlColorsDisplay(n = c(1,3,5,7,9,11), type = "twoway", name = "K1", allow.interpolation = TRUE)
#' @export

thlColorsDisplay <- function(n = 1:12, type = "quali", name = NULL, 
                                      allow.interpolation = FALSE, 
                             alpha = 1, thin = 1){
  p1 <-sapply(n, function(a) thlColors(n = a, type = type, name = name,
                                               allow.interpolation = allow.interpolation, 
                                               alpha = alpha, thin = thin))
  if(isTRUE(any(unlist(lapply(p1, is.null))))){warning("n not valid for palette ", name , ". Adjust n or allow interpolation")}
  if(length(n)>1){
    maxcol <- max(sapply(p1, length))
    p1 <- lapply(p1, function(a)  c(a, rep(NA, maxcol- length(a))))
    df <- data.frame(palette = n, do.call("rbind", p1))
    colnames(df)[2:ncol(df)] <- paste0("col", 1:c(ncol(df)-1))
    longdf <- reshape(df, direction = "long", varying = list(names(df)[2:ncol(df)]), v.names = "fill", 
                      idvar = c("palette"))
  }
  else{
    longdf <- data.frame(fill = p1, time = 1:n, palette= n )
  }
  
  ggplot(data = longdf) + geom_tile(aes(x = time, y = palette, fill = fill, col = fill), colour="white") + 
    scale_color_identity() +
    scale_fill_identity() + theme_void() +
    ggtitle(paste0("type = \"", type, "\", name = ", ifelse(!is.null(name), name, "NULL"), 
                   ", alpha = ", alpha, ", thin = ", thin)) +
    theme(plot.title = element_text(size=16), axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.text = element_text( hjust = 1, size = 14)) +
    scale_y_continuous(breaks = n, labels = paste0(n, " colours") )
  
}
