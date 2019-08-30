#' Control y-axis limits, big marks and decimal marks
#' 
#' @param lang Language: "fi" (default) for Finnish or "en" for English
#' @param limits Specify y-axis limits
#' @param breaks waiver() is the default breaks, see scale_continuous()
#' @param sec.axis specify a secondary axis, see scale_continuous()
#' @param sec.axis.breaks  waiver() is the default breaks for a secondary axis
#' @param sec.axis.labels the labels to be plotted on secondary axis, waiver() is the default, see sec_axis()
#' @return ggplot options, to be combined with ggplot object with "+"
#' @seealso \code{\link{scale_x_continuous}} \code{\link{sec_axis}}
#' @author  Salla Toikkanen, Tarja Palosaari, Petteri Mäntymaa
#' 
#' @export

thlYaxisControl <- function(lang = "fi", limits = NULL, breaks = waiver(), sec.axis = FALSE,
                              sec.axis.breaks = waiver(), sec.axis.labels = waiver()) {
  txt <- "scale_y_continuous(limits = limits, labels = function(x, language=lang) format(x, 
  big.mark = ifelse(language == \"fi\", \" \", \",\"),
  decimal.mark = ifelse(lang == \"en\" , \",\", \".\"),
  scientific = FALSE), expand = c(0,0), breaks = breaks"
  if(isTRUE(sec.axis)){txt<-paste0(txt,
                                   ", sec.axis =  sec_axis(~., breaks = sec.axis.breaks, labels = sec.axis.labels))" )}
  else(txt <- paste0(txt, ")" ))
  
  eval(parse(text = txt))
}
