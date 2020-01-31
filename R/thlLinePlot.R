#' Make thlLinePlot
#' 
#' @param data a data frame
#' @param xvar variable to be plotted on the x-axis.
#' @param yvar variable to be plotted on the y-axis.
#' @param groupvar grouping variable. Defaults to NULL (no grouping).
#' @param ylabel y-axis label. Defaults to yvar.
#' @param xlabel x-axis label. Defaults to NULL (omitted).
#' @param colors colors to be used. 
#' Defaults to THL qualitative color palette for line plots 
#' (see \code{\link{thlColors}}).
#' @param title a character string specifying the title of the plot.
#' @param subtitle a charater string specifing the subtitle of the plot.
#' @param caption a character string specifying the caption of the plot .
#' @param legend.position e.g. "topright", 
#' (see \code{\link{theme}}). Omit with "none".
#' @param base.size the size of basic text in the plot title, 
#' axis labels in PostScript points, defaults to 16.
#' @param linewidth width of the line in PostScript points, defaults to 3.
#' @param show.grid.x TRUE/FALSE, defaults to FALSE. 
#' @param show.grid.y TRUE/FALSE, defaults to TRUE. 
#' Set the grid sequence with parameter yaxis.breaks 
#' @param lang language options for the y-axis labels. 
#' Defines the style of big marks and decimal marks 
#' (see \code{\link{thlYaxisControl}})
#' @param ylimits limits for the y-axis
#' @param marked.treshold the maximum amount of data points plotted 
#' with both line and a dot (default 10). 
#' If the number of data points is greater than this treshold, 
#' dots are plotted only at the beginning and the end of the line. 
#' Setting this to NULL will omit all dots.
#' @param plot.missing TRUE/FALSE, should the missing values be linearly 
#' interpolated and plotted with dotted line 
#' (see \code{\link{thlNaLines}} for more details)?
#' @param xaxis.breaks a character vector defining the x-axis breaks and tickmarks. 
#' Also affects the x-axis grid when it is set to TRUE. 
#' To allow automatic calculation, use waiver()
#' @param yaxis.breaks a character vector defining the y-axis breaks and tickmarks. 
#' Also affects the y-axis grid when it is set to TRUE. 
#' To allow automatic calculation, use waiver()
#' @param panels TRUE/FALSE, should multiple graphs be plotted in separate panels? 
#' If TRUE, the panels are based on the grouping variable 
#' defined by the groupvar -argument. 
#' @param nrow.panels In how many rows should the panels be plotted?  
#' @param labels.end TRUE/FALSE. 
#' Whether the line labels should be displayed at the right end of each line
#' @param circle TRUE/FALSE. If set FALSE shape parameter is set aes
#' 
#' @author Salla Toikkanen, Tarja Palosaari, Petteri Mäntymaa
#' 
#' @return a ggplot -object
#' 
#' @examples 
#' \dontrun{
#' ## Generate data for demonstration
#' year <- 1999:2017
#' set.seed(1234)
#' value1 <- jitter(c(250:(250+length(year)-1)), factor = 100)*1000
#' value2 <- jitter(c(200:(200-length(year)+1)), factor = 300)*1000
#' value3 <- jitter(c(100:(100-length(year)+1)), factor = 400)*1000
#' dat <- data.frame(year = rep(year,3), value = c(value1, value2, value3), 
#'                 group = factor(rep(c("Rabbits", "Magicians", "Playing cards"), 
#'                                    each = length(year))))
#' dat$value <- with(dat, ifelse(year %in% c(2012,2013) & group == "Rabbits", NA, value))
#' dat$value <- with(dat, ifelse(year == 2007 & group == "Magicians", NA, value))
#' 
#' ## Default line plot:
#' thlLinePlot(data = subset(dat, year<2007), xvar = year, yvar = value, 
#'                    groupvar = group, ylimits = c(0,350000))

#' ## Add some decoration and transparency to the colors
#' ## A caption which is partially in bold
#' mycaption <- c(expression(paste(bold("Source:"), " My hat from the rack")))  
#' thlLinePlot(data = subset(dat, year<2007), xvar = year, yvar = value, 
#'                   groupvar = group, 
#'                   title = "Random numbers from the hat!",
#'                   caption = mycaption, 
#'                   subtitle = "Maybe not totally random?", 
#'                   ylab = "#N",
#'                   base.size = 18, 
#'                   linewidth = 4,
#'                   colors = thlColors(n = 3, type ="quali", 
#'                    name = "line", alpha = 0.8), 
#'                   ylimits = c(0,350000))

#' ## By default the missing values are not plotted
#' thlLinePlot(data = dat, xvar = year, yvar = value, groupvar = group, 
#'                   title = "Random numbers from the hat!",
#'                   caption = mycaption,
#'                   subtitle = "Maybe not totally random?",
#'                   ylab = "#N", linewidth = 4,
#'                   ylimits = c(0,350000), xaxis.breaks = 1999:2017)

#' ## Allow linear interpolation of missings and add labels at the end of the lines:          
#' thlLinePlot(data = dat, xvar = year, yvar = value, groupvar = group, 
#'                   title = "Random numbers from the hat!",
#'                   caption = mycaption, 
#'                   subtitle = "Maybe not totally random?",
#'                   ylab = "#N", 
#'                   ylimits = c(0,350000), 
#'                   xaxis.breaks = 1999:2017, plot.missing = TRUE, 
#'                   labels.end = TRUE)
#'                   

#' ## Instead of labels at the line ends you can also use legend...     
#' thlLinePlot(data = dat, xvar = year, yvar = value, groupvar = group, 
#'                   title = "Random numbers from the hat!",
#'                   caption = mycaption, ## Add the caption
#'                   subtitle = "Maybe not totally random?",
#'                   ylab = "#N", 
#'                   ylimits = c(0,350000), 
#'                   xaxis.breaks = 1999:2017, plot.missing = TRUE,
#'                   legend.position = "right")

#' ## ... or plot all groups on different panels ... 
#' thlLinePlot(data = subset(dat, year>2010), xvar = year, yvar = value, 
#'                   groupvar = group, 
#'                   title = "Random numbers from the hat!",
#'                   caption = mycaption, 
#'                   subtitle = "Maybe not totally random?",
#'                   ylab = "#N", 
#'                   ylimits = c(0,350000), 
#'                   xaxis.breaks = 1999:2017, plot.missing = TRUE,
#'                   panels = TRUE, nrow.panels = 1)

#' ## ... or annotate the information manually with thlAnnotate(): 
#' res <- thlLinePlot(data = subset(dat, group %in% c("Rabbits", "Playing cards")), 
#'                    xvar = year, yvar = value, groupvar = group, 
#'                   title = "Random numbers from the hat!",
#'                   caption = mycaption, 
#'                   subtitle = "Maybe not totally random?",
#'                   ylab = "#N", 
#'                   ylimits = c(0,350000), 
#'                   xaxis.breaks = 1999:2017, plot.missing = TRUE)
#'                   
#' res + thlAnnotate(type = "label", x = 2001.5, y = 290000, 
#'                    label = "Bunnies",  style = "white") + 
#'   thlAnnotate(type = "label", x = 2001.5, y = 180000, 
#'                label = "Cards", style = "white",
#'                fill = TRUE) + 
#'   thlAnnotate(type = "box", x = 2012.5, y = 200000, yend = 260000,
#'                label = "2011-14 are missing due \nto a hole in my hat", 
#'                style = "white",
#'                fill = TRUE, text.size = 12) 


#' ## Make a slope graph
#' thlLinePlot(data = subset(dat, year %in% c(1999,2017)),
#'  xvar = year, yvar = value, groupvar = group, 
#'                   title = "Change from 1999 to 2017",
#'                   ylab = "#N", 
#'                   ylimits = c(0,350000), 
#'                   xaxis.breaks=c(1999,2017), 
#'                   labels.end = TRUE, 
#'                   show.grid.x = TRUE, lang ="ENG")

#' data("yli180")
#' yli180$kk2<-factor(yli180$kk, levels = yli180$kk)
#' thlLinePlot(data = yli180, xvar = kk2, yvar = pros, ylimits = c(0,2), ylab = "%",
#'            title = "Erikoissairaanhoidosssa yli 180 vuorokautta hoitoa 
#' odottaneet kolmannesvuosittain 2015 - 2018",
#'            caption="", xaxis.breaks = c("12/2015","12/2016","12/2017",
#'            "12/2018"))
#' thlPlotLogo(label = NULL, x = .67, y = .04, size = 1, fontsize = 10)
#' }
#' @export 

thlLinePlot<-function(data, 
                             xvar, 
                             yvar, 
                             groupvar = NULL, 
                             ylabel = yvar, 
                             xlabel = NULL,                      
                             colors = thlColors(n = 12, type = "quali", 
                                                        name ="line"),
                             title = NULL, 
                             subtitle = NULL, 
                             caption = NULL, 
                             legend.position= "none", 
                             base.size = 16, 
                             linewidth = 3,
                             show.grid.x = FALSE,
                             show.grid.y = TRUE,
                             lang = "fi", 
                             ylimits = NULL, 
                             marked.treshold = 10, 
                             plot.missing = FALSE,  
                             xaxis.breaks = waiver(),
                             yaxis.breaks = waiver(),
                             panels = FALSE, 
                             nrow.panels = 1, 
                             labels.end = FALSE,
                             circle = TRUE) {
  
  lwd <- thlPtsConvert(linewidth)
  
  ## make a simple line plot
  gg <- ggplot(data, aes_(x = substitute(xvar), y = substitute(yvar), 
                          group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar),NA), 
                          colour = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), ""))) + 
    geom_line(size = lwd)

  ## in case plot.missing = TRUE, plot the missings as dotted lines
  if(isTRUE(plot.missing)){
    df <- thlNaLines(data = data, xvar = deparse(substitute(xvar)), 
                                   yvar = deparse(substitute(yvar)),
                                   groupvar =  unlist(ifelse(deparse(substitute(groupvar)) != "NULL", 
                                                             deparse(substitute(groupvar)), 
                                                             list(NULL))))

    
    if(!is.null(df)){
      gg <- gg + geom_line(data = df, aes_(x = substitute(xvar), y= substitute(yvar),
                                           group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar),NA),
                                           colour = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), "")), 
                           linetype = 2, size = lwd) 
    }
  }
  
  if(!is.null(marked.treshold)){
  ## in case of more than marked.treshold data points, geom_point() only on the first and last data point
   if(length(unique(data[,deparse(substitute(xvar))])) > marked.treshold){
    if(is.factor(data[,deparse(substitute(xvar))]) ||
        is.character(data[,deparse(substitute(xvar))]) || 
        is.logical(data[,deparse(substitute(xvar))]))  {
       levs<-levels(factor(data[,deparse(substitute(xvar))]))
       min <- levs[1]
       max <- levs[length(levs)]
       }
         else{
           min <- min(data[,deparse(substitute(xvar))])
           max <- max(data[,deparse(substitute(xvar))])
      }
      subdata<-data[c(data[,deparse(substitute(xvar))] %in% c(min,max)),]
   } else {
     subdata <- data
   }
  if(circle | is.null(substitute(groupvar))) {
    gg <- gg + geom_point(data = subdata, 
                          aes_(x = substitute(xvar), y = substitute(yvar), 
                               group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar),NA), 
                               colour = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), "")),
                               shape = 21, # Shape is constant
                               stroke = 1.35 * lwd, 
                               fill = "white",
                               size = 10/3 * lwd )
  } else {
    gg <- gg + geom_point(data = subdata, 
                          aes_(x = substitute(xvar), y = substitute(yvar), 
                               group = ifelse(!is.null(substitute(groupvar)), substitute(groupvar),NA), 
                               colour = ifelse(!is.null(substitute(groupvar)), substitute(groupvar), ""),
                               shape = substitute(groupvar)), # Shape is aesthetics
                               stroke = 1.35 * lwd, 
                               fill = "white",
                               size = 10/3 * lwd )
    
  }
  gg <- gg + scale_shape_manual(values=21:25) # These are hollow shapes
  }

  ## labels at the end of the lines?
  if(isTRUE(labels.end)){
    if(is.factor(data[,deparse(substitute(xvar))]) ||
       is.character(data[,deparse(substitute(xvar))]) || 
       is.logical(data[,deparse(substitute(xvar))]))  {
       levs<-levels(factor(data[,deparse(substitute(xvar))]))
       maxd <- data[data[,deparse(substitute(xvar))] == 
                      levs[length(levs)],]
    }
    else{ maxd <- data[data[,deparse(substitute(xvar))] == 
                   max(data[,deparse(substitute(xvar))]),]
    }
       brks <- maxd[,deparse(substitute(yvar))]
      labsut <- maxd[,deparse(substitute(groupvar))]
      }
  else(brks <- labsut <- waiver())
  ## assign the rest of the elements to the plot
  gg <- gg +  
    ylab(ifelse(deparse(substitute(ylabel))=="yvar", deparse(substitute(yvar)), ylabel)) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    thlTheme(show.grid.y = show.grid.y, 
              show.grid.x = show.grid.x, base.size = base.size, 
              legend.position = legend.position, 
              x.axis.title = ifelse(!is.null(xlabel), TRUE, FALSE)) +
    xlab(ifelse(!is.null(xlabel), xlabel, "")) + 
    scale_color_manual(values=colors) +
    thlYaxisControl(lang = lang, limits = ylimits, breaks = yaxis.breaks,
                    sec.axis = labels.end,
                    sec.axis.breaks = brks, sec.axis.labels = labsut)
  
  ## check whether xvar is discrete
  if(is.factor(data[,deparse(substitute(xvar))]) ||
     is.character(data[,deparse(substitute(xvar))]) || 
     is.logical(data[,deparse(substitute(xvar))]))  {
      gg <- gg + scale_x_discrete(breaks = xaxis.breaks, expand = expand_scale(mult=c(0.05)))
    }
  else(gg <- gg + scale_x_continuous(breaks = xaxis.breaks))
  if (isTRUE(panels)){
    fmla <- as.formula(paste0("~",  substitute(groupvar)))
    gg <- gg + facet_wrap(fmla, scales = "free", nrow = nrow.panels)
  }
  
  gg
}
