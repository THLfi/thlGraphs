#' thlGraphs: ggplot2 with THL's graphical guidelines
#' 
#' @section Wrappers:
#' List wrappers here
#' 
#' 
#' @section Colors:
#' Color usage
#' 
#' @section Plotting functions:
#' Commonly used plot types made easy.
#' 
#'
#' @docType package
#' @import ggplot2
#' @name thlGraphs
"_PACKAGE"

# This fixes warnings with build when using internal dataset 
utils::globalVariables(
  c("description", "color_nimi", "color_name", "colorset_nimi", "colorset_name",
    "colorset_n", "colorset_id", "palette_nimi", "palette_name")
)
