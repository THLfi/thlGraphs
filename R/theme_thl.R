#' THL visual theme using ggplot2 
#' 
#' Complete theme controlling all non-data display.
#' 
#' @author Oskari Luomala, Tarja Palosaari, Salla Toikkanen, Petteri Mäntymaa
#' 
#' @param base_size base font size in PostScript points
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @param horizontal Set as TRUE if plotting horizontal bars.
#' @param show_axis_line Should axis line be drawn? Axis line should be removed
#' if the coordinate axes are not in the border of the plot
#' @param ... additional properties defined in theme()
#' @return theme() object, to be combined with ggplot object with "+"
#' 
#' @export
#' @examples
#' # add this to the usual plot with "+"
#' theme_thl()
#' # and this to horizontal plots, for example to horizontal bar plot
#' theme_thl(horizontal = TRUE)
#' # axis line should be hidden if coordinate axis is not in plotting area
#' theme_thl(show_axis_line = FALSE)
theme_thl <- function(base_size = 16, base_family = "ArialMT",
                      base_line_size = thlPtsConvert(1.5), 
                      base_rect_size = thlPtsConvert(1.5), 
                      horizontal = FALSE, 
                      show_axis_line = TRUE, 
                      # show_ticks # manually show ticks when needed
                      ...) {
  # Font fixing
  if (.Platform$OS.type == 'windows') {
    windowsFonts(ArialMT = windowsFont("ArialMT"))
  }
  
  # Commonly used measures
  half_line <- base_size / 2
  margin_q <- base_size / 4
  
  # Axis line defined based on `show_axis_line`
  axis.line <- element_line(colour = "#606060", size = 1.5 * base_line_size,
                            lineend = "butt")
  if (isFALSE(show_axis_line)) {
    axis.line <- element_blank()
  } 
  
  # Text elements to be merged depending on their position t r b l -------------
  text_plain <- element_text(size = 0.625 * base_size, face = "plain")
  text_bold  <- element_text(size = 0.750 * base_size, face = "bold")
  text_t <- element_text(margin = margin(b = 0.8 * margin_q), vjust = 0)  
  text_b <- element_text(margin = margin(t = 0.8 * margin_q), vjust = 1)
  text_l <- element_text(margin = margin(r = 0.8 * margin_q), hjust = 1)
  text_r <- element_text(margin = margin(l = 0.8 * margin_q), hjust = 0)
  
  # Axis titles elements defined on multiple locations
  axis.title.x       <- element_text(margin = margin(t = margin_q), vjust = 1)
  axis.title.x.right <- element_text(margin = margin(t = margin_q), vjust = 1, hjust = 1)
  axis.title.y     <- element_text(angle = 90, margin = margin(r = margin_q), hjust = 0.5)
  axis.title.y.top <- element_text(angle = 0, hjust = 1, vjust = 1.05,
                                   margin = margin(r = -margin_q)) # Change right margin for better positioning
  
  # Setting up axis and grid defaults different based on horizontal
  if (isTRUE(horizontal)) {
    axis.line.x <- element_blank()
    axis.line.y <- axis.line
    axis.text.x <- merge_element(text_plain, text_b)
    axis.text.y <- merge_element(text_bold,  text_l)
    panel.grid.major.x <- element_line()
    panel.grid.major.y <- element_blank()
  } else {
    axis.line.x <- axis.line
    axis.line.y <- element_blank()
    axis.text.x <- merge_element(text_bold,  text_b)
    axis.text.y <- merge_element(text_plain, text_l)
    panel.grid.major.x <- element_blank()
    panel.grid.major.y <- element_line()
  }
  
  # Actual theme ---------------------------------------------------------------
  theme(
    line = element_line(colour = "#dcdfe2", size = base_line_size, 
                        linetype = 1, lineend = "butt"),
    rect = element_rect(colour = "white", fill = "white",
                        size = base_rect_size, linetype = 1),
    text = element_text(family = base_family, colour = "#606060", 
                        face = "plain", size = 0.625 * base_size, 
                        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, 
                        margin = margin(), debug = FALSE), 
    ## Axis elements -----------------------------------------------------------
    axis.line = element_line(),
    axis.line.x = axis.line.x,
    # axis.line.x.top, axis.line.x.bottom,
    axis.line.y = axis.line.y,
    # axis.line.y.left, axis.line.y.right,
    
    axis.text = element_text(size = 0.625 * base_size, face = "plain"),
    axis.text.x = axis.text.x,
    axis.text.x.top = merge_element(text_plain,  text_t), 
    # axis.text.x.bottom,
    axis.text.y = axis.text.y,
    # axis.text.y.left, 
    axis.text.y.right = merge_element(text_plain, text_r),
    
    axis.ticks = element_blank(),
    # axis.ticks.x, axis.ticks.x.top, axis.ticks.x.bottom,
    # axis.ticks.y, axis.ticks.y.left, axis.ticks.y.right,
    axis.ticks.length = unit(half_line/2, "pt"),
    
    axis.title = element_text(size = 0.75 * base_size, angle = 0),
    axis.title.x = axis.title.x,
    axis.title.x.top = element_text(vjust = 0, margin = margin(b = margin_q)),  
    #axis.title.x.bottom,
    axis.title.y = axis.title.y.top,
    axis.title.y.right = element_text(vjust = 1.05, hjust = 0, # angle = 90,
                                      margin = margin(l = margin_q)),
    # axis.title.y.left, 
    
    ## Legend elements ---------------------------------------------------------
    legend.background = element_rect(), 
    legend.margin = margin(half_line, half_line, half_line, half_line),
    legend.spacing = unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = element_blank(),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = 0.652 * base_size),
    legend.text.align = NULL,
    legend.title = element_text(size = 0.75 * base_size, hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    # legend.box.just,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),
    
    ## Panel elements ----------------------------------------------------------  # FIXME 
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL,
    panel.grid = element_line(colour = "#dcdfe2", size = rel(2/3)),
    # panel.grid.major,
    panel.grid.minor = element_blank(),
    panel.grid.major.x = panel.grid.major.x,
    panel.grid.major.y = panel.grid.major.y,
    # panel.grid.minor.x, panel.grid.minor.y,
    panel.ontop = FALSE,
    
    ## Plot common elements ---------------------------------------------------- # FIXME margins?
    plot.background = element_rect(colour = "white", fill = "white"),
    plot.title =  element_text(family = base_family, size = base_size, 
                               face = "bold", hjust = 0, color = "#606060",
                               margin(b = 10, t = 10)),
    plot.subtitle = element_text(size = 0.75 * base_size, hjust = 0),
    plot.caption = element_text(size = 0.625 * base_size, hjust = 1, vjust = 0),
    plot.tag = element_text(size = base_size, hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 15,  unit = "pt"),
    
    ## Strip elements ------------------------------------------------------
    strip.background = element_rect(colour = "white", fill = "white"),
    # strip.background.x,
    # strip.background.y,
    strip.placement = "outside",
    strip.text = element_text(size = 0.625 * base_size), ## 10pt in instructions?
    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),
    strip.switch.pad.grid = unit(half_line/2, "pt"), 
    strip.switch.pad.wrap = unit(half_line/2, "pt"),
    #..., 
    complete = TRUE, 
    validate = FALSE
    
    # Additional parameters defined for theme ----------------------------------
  )  + theme(...)
  
}

