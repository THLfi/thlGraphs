
#' Get path to installed resources
resourcePath  <- function() {
  path <- system.file('resources/', package = 'thlGraphs')
  if(!grepl("/$", path))
    path <- paste0(path, "/")
  
  if(.Platform$OS.type == "windows") {
    path <- shortPathName(path)
    path <- gsub("\\\\", "/", path)
  }
  path
}

#' Get path to default logo in installed resources path
logopath <- function() {
  file.path(resourcePath(), "img/THLDEFAULTLOGO.png")
}
