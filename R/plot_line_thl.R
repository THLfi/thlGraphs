# Lineplot v2

#' Make a line plot
#' 
#' 
#' 
#' @param data a data frame
#' @param xvar variable to be plotted on the x-axis.
#' @param yvar variable to be plotted on the y-axis.
#' @param groupvar grouping variable. Defaults to NULL (no grouping).
#' @param colors Colors to be used. 
#' Defaults to THL qualitative color palette for line plots.
#' (see \code{\link{palette_thl}}).
#' @param lang language options for the y-axis labels. 
#' Defines the style of big marks and decimal marks 
#' (see \code{\link{format_thl}})
#' @param linewidth width of the line in PostScript points, defaults to 3.
#' @param y.limits limits for the y-axis
#' @param marked.treshold The maximum amount of data points plotted. 
#' A dot (default 10). 
#' If the number of data points is greater than this treshold, 
#' dots are plotted only at the beginning and the end of the line. 
#' Setting this to NULL will omit all dots.
#' @param plot.missing TRUE/FALSE, should the missing values be linearly 
#' interpolated and plotted with dotted line.
#' (see \code{\link{thlNaLines}} for more details)
#' @param linetype.missing linetype for missing observations
#' @param breaks.x A character vector defining the x-axis breaks.
#' @param breaks.y A character vector defining the y-axis breaks.
#' @param panels Should multiple graphs be plotted in separate panels?
#' If TRUE, the panels are based on the grouping variable defined 
#' by the `groupvar` -argument.
#' @param nrow.panels In how many rows should the panels be plotted?  
#' @param labels.end Whether the line labels should be 
#' displayed at the right end of each line instead of legend.
#' @return A ggplot -object.
#' @author Oskari Luomala, Salla Toikkanen, Tarja Palosaari, Petteri Mäntymaa
#' @export
#' @examples
#' year <- 1999:2017
#' set.seed(1234)
#' value1 <- jitter(c(250:(250+length(year)-1)), factor = 100)*1000
#' value2 <- jitter(c(200:(200-length(year)+1)), factor = 300)*1000
#' value3 <- jitter(c(100:(100-length(year)+1)), factor = 400)*1000
#' dat <- data.frame(year = rep(year,3), value = c(value1, value2, value3), 
#'                   group = factor(rep(c("Rabbits", "Magicians", "Playing cards"), 
#'                                  each = length(year))))
#' dat$value <- with(dat, ifelse(year %in% c(2012,2013) & group == "Rabbits", NA, value))
#' dat$value <- with(dat, ifelse(year == 2007 & group == "Magicians", NA, value))
#' 
#' plot_line_thl(data = dat, xvar = "year", yvar = "value", groupvar = "group")
#' 
#' plot_line_thl(data = dat, xvar = "year", yvar = "value", groupvar = "group",
#'               plot.missing = TRUE)
#' plot_line_thl(data = dat, xvar = "year", yvar = "value", groupvar = "group",
#'               labels.end = TRUE) +
#'   labs(title = "Random numbers from the hat!",
#'        caption = "(c) Magician", 
#'        subtitle = "Maybe not totally random?",
#'        y = "#N", x = "Year")
plot_line_thl <- function(data, xvar, yvar, groupvar = NULL,                     
                         linewidth = 3,
                         colors = palette_thl("line"),
                         lang = "fi", 
                         plot.missing = FALSE, 
                         linetype.missing = 2,
                         marked.treshold = 10, 
                         breaks.x = waiver(),
                         breaks.y = waiver_thl(),
                         y.limits = c(0,NA),
                         panels = FALSE, 
                         nrow.panels = 1, 
                         labels.end = FALSE) {
  
  data <- as.data.frame(data)
  mapping <- aes_string(x = xvar, y = yvar, group = groupvar, color = groupvar)
  # Add empty placeholder string to color mapping for later overwrite with scale_color_manual
  if(is.null(groupvar)) {
    mapping$colour <- ""
  }
  xvec <- data[,xvar]
  # Baseplot
  gg <- ggplot(data, mapping) + geom_line_thl(linewidth = linewidth)
  
  # in case plot.missing = TRUE, plot the missings as dotted lines -------------
  if (isTRUE(plot.missing)) {
    df_missing <- thlNaLines(data = data, xvar = xvar, 
                             yvar = yvar, groupvar = groupvar)
    if (!is.null(df_missing)) {
      gg <- gg + geom_line_thl(data = df_missing, linewidth = linewidth, 
                               linetype = linetype.missing)
    }
  }
  
  ## Adding geom_points to plot ------------------------------------------------
  if (!is.null(marked.treshold)) {
    ## in case of more than marked.treshold data points
    ## plot only the first and last data point
    if (length(unique(xvec)) > marked.treshold) {
      if (is.factor(xvec) || is.character(xvec) || is.logical(xvec)) {
        levs <- levels(factor(xvec))
        min <- levs[1]
        max <- levs[length(levs)]
      } else {
        min <- min(xvec, na.rm = TRUE) # na.rm added just in case
        max <- max(xvec, na.rm = TRUE)
      }
      df_points <- data[xvec %in% c(min,max),]
      gg <- gg + geom_point_thl(data = df_points, linewidth = linewidth)
    } else {
      ## otherwise, add geom_point() to all data points
      gg <- gg + geom_point_thl(linewidth = linewidth) 
    }
  }
  # Adding labels at the end of the lines?
  if (isTRUE(labels.end)) {
    if (is.factor(xvec) || is.character(xvec) || is.logical(xvec)) {
      levs <- levels(factor(xvec))
      maxd <- data[xvec == levs[length(levs)],]
    } else {
      maxd <- data[xvec == max(xvec),]
    }
    legend.position <- "none"
    sec.axis <- dup_axis(name = "", breaks = maxd[,yvar], 
                         labels = maxd[,groupvar])
  } else {
    legend.position <- "right"
    sec.axis <- waiver()
  }
  
  # When plotting a single line (groupvar is NULL) do not plot legend
  if(is.null(groupvar)) {
    legend.position <- "none"
  }
  
  ## assign the rest of the elements to the plot
  gg <- gg + 
    theme_thl(legend.position = legend.position) +
    scale_color_manual(values = colors) +
    scale_y_continuous_thl(language = lang, breaks = breaks.y, 
                           limits = y.limits, sec.axis = sec.axis)
  
  ## check whether xvar is discrete
  if (is.factor(xvec) || is.character(xvec) || is.logical(xvec))  {
    gg <- gg + scale_x_discrete(breaks = breaks.x)
  } else {
    gg <- gg + scale_x_continuous(breaks = breaks.x)
  }
  if (isTRUE(panels)) {
    fmla <- stats::as.formula(paste0("~",  substitute(groupvar)))
    gg <- gg + facet_wrap(fmla, scales = "free", nrow = nrow.panels)
  }
  
  # ENDS HERE
  gg
  
}
