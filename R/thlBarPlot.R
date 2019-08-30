#' Make THL bar plots, vertical and horizontal
#' @inheritParams thlLinePlot
#' @param xvar variable to be plotted on the x-axis. MUST be character or factor; numeric values cause an error
#' @param xaxis.labels a character vector defining the x-axis labels. To allow automatic calculation, use waiver(). Set NULL to not show at all.
#' @param yaxis.breaks a character vector defining the y-axis breaks and tickmarks. Also affects the y-axis grid when it is set to TRUE. To allow automatic calculation, use waiver()
#' @param barwd bar width, default 0.7
#' @param show.value plot y-values or not (TRUE/FALSE)
#' @param value.box y-values as text boxes (TRUE/FALSE)
#' @param value.digits number of digits when rounding of y-values shown (default is no rounding)
#' @param value.size font size for values 
#' @param value.vjust position of value text or box, default is 0.9
#' @param value.hjust horizontal position of value text or box, default is 0.5
#' @param horizontal set TRUE to get horizontal bars. Default is FALSE (vertical bars)
#' @param stacked set TRUE (default) to get stacked bars for groups, FALSE to dodged (side to side) bars
#' @param yaxis set FALSE to not show y axis labels. Default is TRUE 
#' @param xaxis set FALSE to not show x axis line. Default is TRUE 
#' @import ggplot2
#' @seealso \href{https://terho.thl.fi/wiki01/x/vQM9CQ}{Terho: R ggplot2 graphs with THL visual theme}
#' @return  a ggplot object
#' @author Tarja Palosaari, Petteri Mäntymaa, Salla Toikkanen
#' @examples 
#' data("yli180") 
#' yli180$pros <- as.numeric(as.character(yli180$pros))
#' mycaption <- c(expression(paste(bold("Lähde:"), " THL")))  ## A caption which is partially in bold
#' ## Default, vertical bar plot:
#' thlBarPlot(data = subset(yli180, grepl("2017", kk)), xvar = kk, yvar = pros,
#'                  ylimits = c(0,2), ylabel = "%", title = "Erikoissairaanhoidosssa yli 180 vrk odottaneet", caption = mycaption)
#' ## horizontal bar plot: 
#' thlBarPlot(data = subset(yli180, grepl("2017", kk)), xvar = kk, yvar = pros,
#'                  ylimits = c(0,2), ylabel = "%", horizontal = TRUE)
#' @export 

thlBarPlot<-function(data, 
                     xvar, 
                     yvar, 
                     groupvar = NULL, 
                     ylabel = yvar, 
                     xlabel = NULL,
                     colors = thlColors(n = 5, type = "quali", name ="bar"),
                     title = NULL, 
                     subtitle = NULL, 
                     caption = NULL, 
                     legend.position= "none", 
                     base.size = 16, 
                     show.grid.x = FALSE,
                     show.grid.y = TRUE,
                     lang = "fi", 
                     ylimits = NULL, 
                     xaxis.breaks = waiver(), ## added
                     xaxis.labels = waiver(), ## added
                     yaxis.breaks = waiver(),
                     panels = FALSE, 
                     nrow.panels = 1, 
                     barwd = 0.7, 
                     show.value = FALSE,  
                     value.box = FALSE, 
                     value.digits = NULL, 
                     value.size = thlPtsConvert(9), ##max(c(base.size-12, 1))
                     value.vjust = 0.9,
                     value.hjust = 0.5,
                     horizontal = FALSE,
                     stacked = TRUE,
                     yaxis = TRUE,
                     xaxis = TRUE
) {

  ## to avoid error messages about Arial font on Windows
  if (.Platform$OS.type == 'windows') {
    windowsFonts(Arial = windowsFont("Arial Unicode MS"))
  }
  
  if (stacked) {
    bar.position = position_stack(reverse = TRUE)
    value.position = position_stack(vjust = value.vjust, reverse = TRUE)
    if (horizontal) {value.vjust = 0.5} 
    
  } else {
    bar.position = position_dodge2(width = 0.9, preserve = "single", padding = 0)
    value.position = position_dodge( width = 0.7)
    if (horizontal) {
      value.vjust = 0.4
      value.hjust = 1.1
      } else {
      value.vjust = 1.1}
  }


  ## make a simple bar plot
  gg <- ggplot(data, aes_(x = substitute(factor(xvar)), y = substitute(yvar), 
                          group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), NA),
                          fill = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), "") 
                         
  )) +
    
    geom_bar(stat = "identity", colour="white", size=1.05,  width=barwd,
             position = bar.position) +
      
    {if((!horizontal & stacked) ){guides(fill = guide_legend(reverse = TRUE)) }} +
    {if(horizontal & stacked){guides(fill = guide_legend(reverse = FALSE))  }} +
    
    {if(horizontal & !stacked){guides(fill = guide_legend(reverse = TRUE))  }} +
    
    {if(horizontal){coord_flip() }}  ## horizontal, x <-> y

  
  
  ## assign the rest of the elements to the plot
  gg <- gg +
    ylab(ifelse(deparse(substitute(ylabel))=="yvar", deparse(substitute(yvar)), ylabel)) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    
    ## THL visual theme
    thlTheme(show.grid.y = show.grid.y,
              show.grid.x = show.grid.x, base.size = base.size,
              legend.position = legend.position, horizontal = horizontal,
             x.axis.title =  ifelse(!is.null(xlabel), TRUE, FALSE)) +
    xlab(ifelse(!is.null(xlabel), xlabel, "")) +
    thlYaxisControl(lang = lang, limits = ylimits, breaks = yaxis.breaks,
                      sec.axis.breaks = brks, sec.axis.labels = labsut) +
    
    scale_x_discrete(breaks =  xaxis.breaks, labels = xaxis.labels) + ## NEW, for discrete values
    ## THL colors
    scale_fill_manual(values=colors)  
  
  if (isTRUE(panels)){
    fmla <- as.formula(paste0("~",  substitute(groupvar)))
    gg <- gg + facet_wrap(fmla, scales = "free", nrow = nrow.panels)
  }
  
  
  
  ### values as text boxes, grey font, but also grey border line...
  if (isTRUE(show.value & value.box)){
    ## no rounding and rounding
    if (isTRUE(is.null(value.digits))){
      gg <- gg +  geom_label(aes_(group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), ""),
                                  label =  substitute(format(yvar, big.mark = ifelse(lang == "fi", " ", ","), decimal.mark = ifelse(lang == "fi" , ",", ".")))), fill = "white", size=0.9*value.size, show.legend = FALSE,
                             position = value.position, colour="#606060", vjust = value.vjust, hjust = value.hjust)

    } else {   gg <- gg +  geom_label(aes_(group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), ""),
                                           label =  substitute(format(round(yvar,value.digits), big.mark = ifelse(lang == "fi", " ", ","), decimal.mark = ifelse(lang == "fi" , ",", ".")))), fill = "white", size=0.9*value.size, show.legend = FALSE,
                                      position = value.position, colour="#606060",  vjust = value.vjust, hjust = value.hjust)
    }
    ## vjust = 0.82 was for horizontals
  } else if (isTRUE(show.value)) {
    ## no rounding
    if (isTRUE(is.null(value.digits))){
      gg <- gg +  geom_text(aes_(group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), ""),
                                 label =  substitute(format(yvar, big.mark = ifelse(lang == "fi", " ", ","), decimal.mark = ifelse(lang == "fi" , ",", ".")))), size=value.size, show.legend = FALSE,
                            position = value.position, colour="white", fontface="bold", vjust = value.vjust, hjust = value.hjust)
    } else { ## rounding
      gg <- gg +  geom_text(aes_(group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), ""),
                                 label =  substitute(format(round(yvar,value.digits), big.mark = ifelse(lang == "fi", " ", ","), decimal.mark = ifelse(lang == "fi" , ",", ".")))), size=value.size, show.legend = FALSE,
                            position = value.position, colour="white", fontface="bold" , vjust = value.vjust, hjust = value.hjust)
    }
  }
  ## vjust = 0.84, was for horizontals
  
  ## to hide x axis line
  if ( !xaxis) {
    if ( !horizontal) {
       gg <- gg + theme(axis.line.x = element_blank())
    } else {
      gg <- gg + theme(axis.line.y = element_blank())
    }
  }
  ## to hide y axis labels
  if ( !yaxis) {
    if ( !horizontal) {
       gg <- gg + theme(axis.text.y = element_blank()) + ylab("")
    } else {
      gg <- gg + theme(axis.text.x = element_blank()) + ylab("")
    }
  }
  
  
   
  
  gg
}
