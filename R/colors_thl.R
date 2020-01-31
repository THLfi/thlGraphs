# THL COLORS -------------------------------------------------------------------

#' Show THL color names
#' 
#' Shows the predefined names of colors.
#' 
#' @param lang Language of names fetched
#' @param include.secondary Should the additional colors be shown?
#' Quantitative and two-way color palettes have additional colors predefined.
#' Names of such might not be informative and they should not be used alone.
#' 
#' @export
#' @examples 
#' colornames_thl("fi")
#' colornames_thl("en")
#' colornames_thl(include.secondary = TRUE)
colornames_thl <- function(lang = c("fi", "en"), include.secondary = FALSE) {
  lang <- match.arg(lang, several.ok = TRUE)
  colors <- colors_thl_
  if (isFALSE(include.secondary)) {
    colors <- subset(colors_thl_, !is.na(description))
  } 
  colors <- subset(colors, select =  c("color_nimi", "color_name"))
  names(colors) <- c("fi", "en")
  colors[, lang]
}


#' Generate THL colors by name
#' 
#' Generate THL colos by referring to them with finnish or english name
#' 
#' @param colorname Names of the colors in finnish or in english
#' 
#' @export
#' @examples
#' colors_thl("thl.green")
#' colors_thl("petrol")
colors_thl <- function(colorname = NULL) {
  cols <- subset(colors_thl_, 
                 color_nimi %in% colorname | color_name %in% colorname)
  cols$color_hex
}


#' Generate predefined THL color sets 
#' 
#' @param set Name of the color set
#' @param n Number of colors in set
#' @param set_id Identifier of set
#' 
#' @export
#' @examples
#' colorset_thl("area", 2)
#' colorset_thl("area", 3, 2)
#' colorset_thl("line", 2)
colorset_thl <- function(set, n, set_id = 1) {
  set_c <- unique(c(colorsets_thl_$colorset_name, colorsets_thl_$colorset_nimi))
  set <- match.arg(set, set_c)
  colset <- subset(colorsets_thl_, set == colorset_nimi | set == colorset_name) 
  
  n_c <- unique(colset$colorset_n)
  n <- match.arg(as.character(n), n_c)
  colset <- subset(colset, colorset_n == n)
  
  set_c <- unique(colset$colorset_id)
  set_id <- match.arg(as.character(set_id), set_c)
  colset <- subset(colset, colorset_id == set_id)
  colset$color_hex
}


#' Generate THL color palette
#' 
#' Get the hex values of colors in predefined THL color palette.
#' 
#' @param name Spesify the name of the palette.
#' @param n Number of colours to generate from palette.
#' @param force Allows to generate more colors than the default palette has.
#' Supressses the warnings produced from recycling or interpolating the colors.
#' 
#' @return Character vector of the hex values of the colors
#' 
#' @export
#' @examples
#' palette_thl("viiva", 6)
#' palette_thl("line")
palette_thl <- function(name, n = NULL, force = FALSE) {
  name_c <- unique(c(palettes_thl_meta_$palette_nimi, 
                     palettes_thl_meta_$palette_name))
  name <- match.arg(name, name_c)

  # Palette hex codes
  pal_df <- subset(palettes_thl_, palette_nimi == name | palette_name == name)
  pal_df <- with(pal_df, pal_df[order(color_order), ])
  pal_colors <- pal_df$color_hex

  # Palette meta information
  meta_df <- subset(palettes_thl_meta_, 
                    palette_nimi == name | palette_name == name)
  pal_type <- meta_df$palette_type
  pal_maxcol <- meta_df$palette_maxcolors
  
  # Plot all palette's colors if n is a null
  if (is.null(n)) {
    n <- pal_maxcol
  }
  if (pal_type == "sarjallinen") {
    pal <- grDevices::colorRampPalette(colors = pal_colors)(n)
  } else if (pal_type == "kaksisuuntainen") {
    if (n %% 2 != 1 & !force) {
      warning("The default twoway palette supports odd number of colours")
    }
    pal <- grDevices::colorRampPalette(colors = pal_colors)(n)
  } else {
    pal <- rep_len(pal_colors, length.out = n)
  }
  # If color amount exceed maximum show warning
  if (isTRUE(n > pal_maxcol) & !force) {
    warning(sprintf("\n  Palette %s has maximum of %d colors.\n  ", name, pal_maxcol),
            "Now using grDevices::colorRampPalette() to interpolate colors OR colors were recycled.")
  }
  pal
}


#' View colors with a given color hex codes
#' 
#' @param hexcode A hexadecimal string defining colors
#' @param nrow Number of rows used in plotting window
#' 
#' @export
#' @examples 
#' # View all predefined colors of the THL
#' colnams <- colornames_thl("en")
#' cols <- colors_thl(colnams)
#' plot_colors(cols)
plot_colors <- function(hexcode, nrow = NULL) {
  df <- data.frame(col = factor(hexcode, levels = unique(hexcode)),
                   group = seq_along(hexcode)) 
  # paste0(seq_along(hexcode), "\n", hexcode)
  ggplot(data = df, aes(x = 1, y = 1, fill = col, label = col)) +
    geom_tile() +
    geom_text() +
    scale_fill_identity() +
    facet_wrap("group", nrow = nrow) +
    theme_void()
}


#' Plot a THL color palette
#' 
#' Simple helpful wrapper to quickly see colors in a THL palette.
#' 
#' @param name String spesifying the name of the palette
#' @param n Number of colors to be printed from the palette
#' 
#' @export
#' @examples 
#' plot_palette_thl("quali")
#' plot_palette_thl("line")
#' plot_palette_thl("area")
#' plot_palette_thl("s1", 7)
#' plot_palette_thl("k1", 5)
plot_palette_thl <- function(name, n = NULL) {
  hexcodes <- palette_thl(name, n = n)
  plot_colors(hexcodes, nrow = 1)
}

