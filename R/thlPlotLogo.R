#' Insert THL default logo to a plot
#' @param x x-position of the logo
#' @param y y-position of the logo
#' @param size Logo size
#' @import png
#' @import grid
#' @author Petteri Mäntymaa, Tarja Palosaari,  Salla Toikkanen
#' @examples 
#' \dontrun{
#' ## Generate data for demonstration 
#' year <- 1999:2017
#' set.seed(1234)
#' value1 <- jitter(c(250:(250+length(year)-1)), factor = 100)*1000
#' value2 <- jitter(c(200:(200-length(year)+1)), factor = 300)*1000
#' value3 <- jitter(c(100:(100-length(year)+1)), factor = 400)*1000
#' dat <- data.frame(year = rep(year,3), value = c(value1, value2, value3), 
#'                   group = factor(rep(c("Rabbits", "Magicians", "Playing cards"), 
#'                                      each = length(year))))
#' dat$value <- with(dat, ifelse(year %in% c(2012,2013) & group == "Rabbits", NA, value))
#' dat$value <- with(dat, ifelse(year == 2007 & group == "Magicians", NA, value))
#' 
#' ## Default line plot:
#' thlLinePlot(data = subset(dat, year<2007), xvar = year, yvar = value, 
#'                    groupvar = group, ylimits = c(0,350000))
#' ## Add THL logo to the plot                   
#' thlPlotLogo(0.78, 0.92, 1.7)
#' }
#' @export 

thlPlotLogo <- function(x,y,size) {
  if((!is.numeric(x)|x<0|x>1)|(!is.numeric(y)|y<0|y>1)) {
    stop("x and y must be numeric [0,1]")
  }
  if(!is.numeric(size)|size<0) {
    stop("size must be numeric and non-negative")
  }
  image <- png::readPNG(logopath())
  logo <- grid::rasterGrob(image = image,
                           x = unit(x, "npc"),
                           y = unit(y, "npc"),
                           width = unit(2*size, "cm"),
                           height = unit(size, "cm"),
                           just = c("left", "centre"),
                           interpolate = T)
  grid::grid.draw(logo)
}
