#' THL visual theme using ggplot2 
#' 
#' @author Tarja Palosaari,  Salla Toikkanen, Petteri Mäntymaa
#' 
#' @param show.grid.x Show background vertical grid TRUE/FALSE?
#' @param show.grid.y Show background horizontal grid TRUE/FALSE?
#' @param base.size Base size for elements in PostScript points
#' @param basic.lwd Basic linewidth in PostScript points
#' @param legend.position Position of the legend text
#' @param horizontal Set as TRUE if plotting horizontal bars
#' @param x.axis.title Set as TRUE if x-axis label is desired
#' @return theme() object, to be combined with ggplot object with "+"
#' @import ggplot2
#' @export


thlTheme <- function(show.grid.y = TRUE, show.grid.x = FALSE, base.size = 16,
                      basic.lwd = 3, legend.position = "none", horizontal = FALSE,
                     x.axis.title = FALSE) {
  if (.Platform$OS.type == 'windows') {
    windowsFonts(ArialMT = windowsFont("ArialMT"))
  }
  font.family = "ArialMT"
  
  if(isTRUE(show.grid.y & !horizontal)){ panel.grid.major.y = element_line(colour = "#dcdfe2", size = thlPtsConvert(1) )}
  
  
  else if (!horizontal){panel.grid.major.y = element_blank()}
  if(isTRUE(show.grid.x & !horizontal)){ panel.grid.major.x = element_line(colour = "#dcdfe2", size = thlPtsConvert(.75), linetype = 2 )}
  else if (!horizontal){panel.grid.major.x = element_blank()}
  if(isTRUE(show.grid.y & horizontal)){ panel.grid.major.x = element_line(colour = "#dcdfe2", size = thlPtsConvert(1) )}
  else if(horizontal){panel.grid.major.x = element_blank()}
  if(isTRUE(show.grid.x & horizontal)){ panel.grid.major.y = element_line(colour = "#dcdfe2", size = thlPtsConvert(.75), linetype = 2 )}
  else if (horizontal){panel.grid.major.y = element_blank()}
  
  if(horizontal) {
    axis.title.x = element_text( angle = 0, size = 0.75*base.size, family = font.family,
                  colour = "#606060", hjust=1.0,
                  margin = margin(b=10, t=10))
  } 
  if(isTRUE(x.axis.title)){
    axis.title.x = element_text(size = 0.75*base.size, family = font.family, 
                                colour = "#606060")
  }
  
  else {
    axis.title.x = element_blank()
  }
  if (horizontal) {
    axis.line.y =  element_line(size = thlPtsConvert(2), ## was: size = thlPtsConvert(1.5)
                                colour = "#606060", lineend = "butt")
    axis.line.x = element_blank()
  } else {
     axis.line.y = element_blank()
     axis.line.x = element_line(size = thlPtsConvert(2),  ## was: size = thlPtsConvert(1.5)
                             colour = "#606060", lineend = "butt")
  }

  if (horizontal) {
    axis.text.y = element_text(size = 0.75*base.size, family =font.family, colour = "#606060", face = "bold") ## 10-12pt in instructions
  }
  else {
    axis.text.y = element_text(size = 0.625*base.size, family =font.family, colour = "#606060") ## 10-12pt in instructions
  }
  if (horizontal) {
    axis.text.x = element_text(size = 0.625*base.size, family =font.family, colour = "#606060") ## 12pt in instructions
     }
  else {
    axis.text.x = element_text(size = 0.625*base.size, family =font.family, colour = "#606060", face = "bold")## 12pt in instructions
  }
  if (horizontal) {
    axis.title.y = element_blank()
  }
  else {
    axis.title.y = element_text( angle = 0, size = 0.75*base.size, family =font.family, 
                  colour = "#606060", vjust=1.05, ## 10pt in instructions
                  margin = margin(b=10, t=10)) ##  y title on top, not above the axis the axis!!!!
  }
  
  lwd <- thlPtsConvert(basic.lwd)
  
  
    theme(
      legend.position = legend.position,
      legend.title = element_blank(),
      legend.background = element_rect(fill = "white"),
      legend.key = element_blank(),
      legend.text = element_text(size = 0.625*base.size, colour = "#606060" ),  ## 10pt in instructions
      
      line = element_line(size = basic.lwd),
      
      plot.title = element_text(colour = "#606060", face = 2, size = base.size, hjust = 0),## 16pt in instructions
      plot.subtitle = element_text(colour = "#606060", size =  0.75*base.size, hjust = 0), ## 12pt in instructions
      plot.caption = element_text(colour = "#606060", size = 0.625*base.size, hjust = 1,
                                  vjust = 0, margin = margin(t=10)), ## 10pt in instructions??
      
      axis.title.x = axis.title.x,
      axis.line.x = axis.line.x,
      axis.line.y = axis.line.y,
      axis.title.y = axis.title.y,
      
      axis.text.y = axis.text.y ,
      
      axis.text.x = axis.text.x,
      axis.ticks = element_blank(),
      
      panel.background = element_blank(), ## poistaa boxin
      panel.grid.major.y = panel.grid.major.y,
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = panel.grid.major.x,
      panel.grid.minor.x = element_blank(),
      
      strip.background = element_rect(colour = "white", fill = "white") ,
      strip.placement = "outside",
      strip.text = element_text(size = 0.625*base.size, family =font.family,
                                colour = "#606060"), ## 10pt in instructions??
      text = element_text(family = font.family, colour = "#606060"), ## 8-10pt in instructions
      plot.margin = margin(t = 20, r = 20, b = 20, l = 15, unit = "pt")  ## previously l=5.5 
      
    )  
   
 
}
