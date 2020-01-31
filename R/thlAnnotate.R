#' Add annotations to the plot
#' 
#' @param type Type of annotation wanted: "box" for a text box or "label" for text
#' @param x x-axis location for annotation
#' @param y y-axis location for annotation
#' @param xend location on the x-axis for the tip of the arrow, defaults to x 
#' @param yend location on the x-axis for the tip of the arrow, defaults to y
#' @param label text to be annotated
#' @param arrow.size size of the arrow head in PostScript points
#' @param text.size size of text head in PostScript points
#' @param hjust horizontal justification of the text, 0 = left justified, 0 = right justified. Defaults to 0.5.
#' @param style color style of the annotation ("white" or "dark")
#' @param fill Fill the background of the annotation with color? Defaults to TRUE
#' @param boldFont set TRUE (default) for bolded font
#' @param capitalized set TRUE for upper case letters, default is FALSE
#' @param box.size unused
#' @author Salla Toikkanen, Tarja Palosaari, Petteri Mäntymaa
#' @export

thlAnnotate<-function(type=c("box","label"), x,  y, xend = x, yend = y, label, 
                      arrow.size = 10,
                      text.size = 16, hjust = 0.5, 
                      style = c("white", "dark"), 
                      fill = TRUE, boldFont=TRUE, 
                      capitalized = FALSE, box.size = 0.4){

  tsize <- thlPtsConvert(text.size)
  type <- match.arg(type)
  style <- match.arg(style)
  
  if (boldFont) {
    tbold <- "bold"
  } else {tbold <- "plain"}
  
  if (capitalized) {
    label <- toupper(label)
  } 
  
  if(type == "box"){
    a <- annotate("segment", x = x, xend = xend, y = y, yend = yend, colour = "#606060", 
                  arrow = arrow(length = unit(arrow.size, "points"), type = "closed"), size = .4)
    b <- annotate("label", x = x, y = y, label = label, 
                  color =  ifelse(style == "dark", "white", "#606060"), 
                  fill  = ifelse(!isTRUE(fill), NA, ifelse(style == "dark",  "#606060", "white")),
                  label.padding = unit(0.5, "lines"), label.r = unit(0, "lines"), 
                  label.size = 0.4, size = tsize,
                  hjust = hjust)
    
    li<-list(a,b)
  }
  if(type == "label"){
    li <- annotate("label", x = x, y = y, label = label, 
                   color =  ifelse(style == "dark", "white", "#606060"), 
                   label.padding = unit(0.1 , "lines"), label.r = unit(0, "lines"), label.size = 0, ## was previously label.padding = unit(0.5 , "lines")
                   size = tsize, fontface = tbold, hjust = hjust,
                   fill = ifelse(!isTRUE(fill), NA, ifelse(style == "dark",  "#606060", "white")))
  }
  li
}  
