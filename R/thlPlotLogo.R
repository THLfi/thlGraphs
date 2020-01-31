#' Insert THL logo to a plot
#' @param label Label before the logo  
#' @param lang Language of the logo
#' Finnish, swedish and english versions are available.
#' @param x x-position of the logo
#' @param y y-position of the logo
#' @param size Logo size
#' @param fontsize Label fontsize
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
#' thlPlotLogo("Source: ", "en", .71, .02, 0.9, fontsize = 10)
#' }
#' @export 

# Function to draw your statement (TODO:CHANGE FONT & COLOR)
thlPlotLogo <- function(label,
                        lang = "fi",
                        x,
                        y,
                        size,
                        fontsize) {
  lang <- match.arg(lang, choices = c("fi", "sv", "en"))
  if(!lang %in% c("fi", "sv", "en")){
    stop("Argument lang should be one of the following: \"fi\",  \"sv\", \"en\".")
  } else if(lang == "en") {
    logowd <- 6.5
  } else {
    logowd <- 5.5
  }
  image <- png::readPNG(
    system.file("extdata",
                paste0("thl-logo-",lang,".png"),
                package = "thlGraphs",
                mustWork = TRUE))
  lab <- grid::textGrob(label = label,
                        x = unit(x, "npc"),
                        y = unit(y, "npc"),
                        just = c("left", "centre"),
                        gp = grid::gpar(fontsize = fontsize))
  logo <- grid::rasterGrob(image = image,
                           x = unit(x, "npc") + unit(1, "grobwidth", lab),
                           y = unit(y, "npc"),
                           width = unit(logowd*size, "cm"),
                           height = unit(size, "cm"),
                           just = c("left", "centre"),
                           gp = grid::gpar(fontsize = fontsize))
  grid::grid.draw(lab)
  grid::grid.draw(logo)
}