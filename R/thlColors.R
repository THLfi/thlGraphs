#' Generate THL colour palettes
#' 
#' This function is deprecated, use palette_thl() instead.
#' 
#' @author  Salla Toikkanen, Tarja Palosaari, Petteri Mäntymaa
#' @param n a numerical value describing how many colours 
#' @param type a character string describing the type of colour palette. 
#' Allowed values are "main" for the THL main colors (8 colors, for printed media), 
#' "powerpoint" for the THL powerpoint colors (9 colors, for PowerPoint presentations),
#' "quali" for qualitative palettes, 
#' "quanti" for quantitative palettes and 
#' "twoway" for twoway quantitative palettes
#' @param name a character string specifying the name of the specific colour palette. 
#' Valid values include "line", "bar" and NULL for type "quali", 
#' "S1" - "S6" for type "quanti" and "K1" - "K6" for type "twoway" (see examples)
#' @param allow.interpolation a logical TRUE/FALSE whather colour palette 
#' interpoation with grDevices::colorRampPalette() should be allowed 
#' if n is outside the range of official number of colours in each quantitative 
#' or twoway quantitative palettes.  
#' @param alpha Amount of transparency, 1 = not transparent, 0 = fully transparent.
#' Recycled.
#' @param thin Amount of lightening, 1 = none, 0 = white.
#'
#' 
#' @seealso \code{\link{thlShade}}, \code{\link{thlColorsDisplay}}
#' 
#' @examples 
#' ## Generate 3 colours from the quantitative colour palette S2
#' thlColors(n = 3, type = "quanti", name = "S2")
#' 
#' ## Generate colours from the THL PowerPoint colour palette S2
#' thlColors(type = "powerpoint")
#' 
#' ## Generate 3 colours from the quantitative colour palette S2 adding 50% transparency
#' thlColors(n = 3, type = "quanti", name = "S2", alpha = 0.5)
#' 
#' ## Generate 3 colours from the main THL color palette
#' thlColors(n = 3, type = "main")
#'
#' ## Generate all colors from the quantitative color palette S2 by setting n to NULL
#' thlColors(n = NULL, type = "quanti", name = "S2")
#' 
#' ## THL qualitative colours
#' thlColors(n = 10, type = "quali", name = NULL)
#' 
#' ## THL qualitative colours in the order in which they should be used in line plots
#' thlColors(n = 6, type = "quali", name = "line")
#' 
#' ## THL qualitative colours in the order in which they should be used 
#' ## in bar plots when 3 colors are needed
#' thlColors(n = 3, type = "quali", name = "bar")
#' 
#' ## Attempt to generate 11 colours from twoway quantitative palette K1 yields an error
#' thlColors(n = 11, type = "twoway", name = "K1")
#' 
#' ## Allowing interpolation 11 colours can be generated, although not recommended
#' thlColors(n = 11, type = "twoway", name = "K1", allow.interpolation = TRUE)
#' @export

thlColors <- function(n = 1, type = "main", 
                              name = NULL, 
                              allow.interpolation = FALSE, 
                      alpha = 1, thin = 1) {
  .Deprecated("palette_thl")
  if(isTRUE(allow.interpolation)){require(grDevices)}
  type <- match.arg(type, choices = c("main", "quali", "quanti", "twoway", "powerpoint"))
  if(!type %in% c("main", "quali", "quanti", "twoway", "powerpoint")){
    stop("Argument type should be one of the following: \"main\",  \"powerpoint\", \"quali\", \"quanti\" or \"twoway\".")
  }
  if(type == "powerpoint"){  ## THL powerpoint värit
    pal <- c("#7bc143", "#519b2f", "#f2f2f2", "#606060", "#0060a6", "#079e9e", "#6bc9c7", 
    "#9171bc", "#bc4ba7")
   if(is.null(n)) n <- length(pal)
   pal <- as.vector(rep(pal, length.out = n))
  }
  
  if(type == "main"){ ## thl päävärit
  pal <- c("#7BC143", "#807F83", "#6BC9C7", "#5191CD", "#B19ACA",
               "#FAA61A", "#ED1651", "#E20077")
  if(is.null(n)) n <- length(pal)
  pal <- as.vector(rep(pal, length.out = n))
  }

  if(type == "quali"){
    if(!name%in%c("line", "bar") && !is.null(name)){
      message("The name argument should be one of the following: \"line\", \"bar\", NULL, now taken as NULL.")
      name <- NULL
    }
    pal <- c("tummansininen" = '#2f62ad', 
             "rubiininpunainen" = '#be3f72', 
             "tummanvihrea" = "#519b2f",
             "syaaninsininen" = "#29a0c1", 
             "roosa" = "#cc77ac",  
             "oranssi" = "#faa61a", 
             "vaaleanvihrea" = "#7bc143", 
             "tumma harmaa" = "#606060", 
             "keskiharmaa" = "#c3c2c6", 
             "vaalea harmaa" = "#dcdfe2")
    if(is.null(n)) n<- length(pal)
    if (!is.null(name) && name == "line"){  
      pal <- pal[c("tummanvihrea", "tummansininen", "rubiininpunainen","syaaninsininen", 
                   "oranssi", "roosa")]
    }
    
    if (!is.null(name) && name == "bar"){
      if(n==1) pal <- pal[c("tummanvihrea")]
      if(n==2) pal <- pal[c("tummanvihrea", "vaaleanvihrea")]
      if(n==3) pal <- pal[c("tummanvihrea", "tummansininen", "oranssi")]
      if(n>3 & n<7) { pal <- pal[c("tummanvihrea", "tummansininen", "roosa",  "syaaninsininen",
                                 "rubiininpunainen",  "oranssi")][1:n] }
    }
    pal <- as.vector(rep(pal, length.out = n))
  }
  if(type == "quanti"){  
    vaalea <- "#fce4c3"
    vsin <- "#d3eaed"
    if(!name%in%c("S1", "S2", "S3", "S4", "S5", "S6") || is.null(name)){
      message("The name argument should be one of the following: \"S1\", \"S2\", \"S3\", \"S4\", \"S5\", \"S6\", now taken as S1")
      name <- "S1"
    }
    ## Määrälliset S1, tummanvihrea
    if(name == "S1"){
      tvihr <- "#519b2f"
      if(is.null(n)) n <- 7
      if(n==7) {pal <- c(tvihr, "#87b65c", "#9fbf70", "#b6c985", "#cdd29a","#e5dbae", vaalea)}
      if(n==6) {pal <- c(tvihr, "#8cb860", "#a8c379", "#c4ce91", "#e0d9aa", vaalea)}
      if(n==5) {pal <- c(tvihr, "#93bb66", "#b6c985", "#d9d6a4",  vaalea)}
      if(n==4) {pal <- c(tvihr, "#9fbf70", "#cdd29a", vaalea)}
      if(n==3) {pal <- c(tvihr, "#b6c985", vaalea)}
      if(n==2) {pal <- c(tvihr, vaalea)}
      if(n==1) {pal <- tvihr}
      if(!n %in% c(1:7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has max 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tvihr, vaalea))(n)}
        else{
          warning("The official palette ", name, " has max 7 colors")
          return(NULL)
        }
      }
    }
    ## Määrälliset S2, tummansininen
    if(name == "S2"){
      tsin <-  '#2f62ad'
      if(is.null(n)) n <- 7
      if(n==7) pal <- c(tsin, "#337aac","#6594b2", "#98afb7", "#cac9bd", "#e7d9c1", vaalea)
      if(n==6) pal <- c(tsin, "#4080ad", "#7fa2b4", "#bdc3bc", "#e3d7c0", vaalea)
      if(n==5) pal <- c(tsin, "#558bb0", "#a8b8b9", "#ddd3c0", vaalea)
      if(n==4) pal <- c(tsin, "#7ea2b5" ,"#d2cebe", vaalea)
      if(n==3) pal <- c(tsin, "#bec3bc", vaalea) 
      if(n==2) pal <- c(tsin, vaalea)
      if(n==1) pal <- tsin
      if(!n%in% c(1:7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has max 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tsin, vaalea))(n)}
        else{
          warning("The official palette ", name, " has max 7 colors")
          return(NULL)
        }
      }
    }
    ## Määrälliset S3, rubiininpunainen
    if(name == "S3"){
      rpun <- '#be3f72'
      if(is.null(n)) n <- 7
      if(n == 7) pal <- c(rpun, "#cb6083", "#d78193",  "#e4a2a2", "#f0c3b3", "#f6d3bb", vaalea)
      if(n == 6) pal <- c(rpun,  "#ce6887", "#de929b","#edbbaf", "#f6d3bb", vaalea)
      if(n == 5) pal <- c(rpun, "#d3768e", "#e8ada8", "#f5d0b9", vaalea)
      if(n == 4) pal <- c(rpun, "#de929b", "#f2c9b5", vaalea)
      if(n == 3) pal <- c(rpun,  "#edbbaf", vaalea) 
      if(n == 2) pal <- c(rpun, vaalea)  
      if(n == 1) pal <- rpun
      if(!n%in% c(1:7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has max 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(rpun, vaalea))(n)}
        else{
          warning("The official palette ", name, " has max 7 colors")
          return(NULL)
        }
      }
    }
    ## Määrälliset - sarja S4, tvihr to vsin
    if(name == "S4"){
      tvihr <- "#519b2f"
      if(is.null(n)) n <- 7
      if(n==7) pal <- c(tvihr, "#67a84f",   "#7cb56e",  "#92c38e",  "#a8d0ae", "#bdddcd", vsin)
      if(n==6) pal <- c(tvihr, "#6bab55","#85bb7b","#9fcaa1", "#b9dac7",  vsin)
      if(n==5) pal <- c(tvihr, "#72af5f", "#92c38e", "#b3d6be", vsin)
      if(n==4) pal <- c(tvihr,"#7cb56e","#a8d0ae", vsin)
      if(n==3) pal <- c(tvihr, "#92c38e", vsin) 
      if(n==2) pal <- c(tvihr, vsin)
      if(n==1) pal <- tvihr
      if(!n%in% c(1:7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has max 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tvihr, vsin))(n)}
        else{  
          warning("The official palette ", name, " has max 7 colors")
          return(NULL)
        }
      }
    }  
    ## Määrälliset S5, tsin to vsin
    if(name == "S5"){
      tsin <-  '#2f62ad'
      if(is.null(n)) n <- 7
      if(n == 7) pal <- c(tsin, "#4a79b8", "#668fc2", "#81a6cd",     "#9cbdd8",  "#b8d3e2",   vsin)
      if(n == 6) pal <- c(tsin,"#507dba", "#7198c7", "#91b4d3", "#b2cfe0", vsin)
      if(n == 5) pal <- c(tsin,"#5884bd", "#81a6cd","#aac8dd", vsin)
      if(n == 4) pal <- c(tsin,"#668fc2", "#9cbdd8", vsin)
      if(n == 3) pal <- c(tsin, "#81a6cd", vsin) 
      if(n == 2) pal <- c(tsin, vsin) 
      if(n == 1) pal <- c(tsin) 
      if(!n%in% c(1:7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has max 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tsin, vsin))(n)}
        else{  
          warning("The official palette ", name, " has max 7 colors")
          return(NULL)}
      }
    }  
    ## Määrälliset S6, rpun to vsin
    if(name == "S6"){
      rpun <- '#be3f72'
      if(is.null(n)) n <- 7
      if(n == 7) pal <- c(rpun, "#c25c87", "#c5789b","#c994b0",  "#ccb1c4",    "#cfcdd8",  vsin)
      if(n == 6) pal <- c(rpun, "#c2618b",   "#c683a3", "#cba6bc","#cfc8d4", vsin)
      if(n == 5) pal <- c(rpun,"#c36a91","#c994b0","#cebfce",vsin)
      if(n == 4) pal <- c(rpun,"#c5789b", "#ccb1c4",vsin)
      if(n == 3) pal <- c(rpun, "#c994b0", vsin) 
      if(n == 2) pal <- c(rpun, vsin) 
      if(n == 1) pal <- rpun 
      if(!n%in% c(1:7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has max 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(rpun, vsin))(n)}
        else{
          warning("The official palette ", name, " has max 7 colors")
          return(NULL)}
      }
    }  
  }
  if(type == "twoway"){  
    if(!name%in%c("K1", "K2", "K3", "K4", "K5", "K6") || is.null(name)){
      warning("The name argument should be one of the following: \"K1\", \"K2\", \"K3\", \"K4\", \"K5\", \"K6\", now taken as K1")
      name<- "K1"
    }
    vharm <- "#dcdfe2"
    ## Määrälliset K1, tsin to rpun
    if(name == "K1"){
      tsin <-  '#2f62ad'
      rpun <- '#be3f72'
      if(is.null(n)) n <- 9
      if(n == 9){ pal <- c(tsin, "#387fb5", "#6f9fc4", "#a5bfd3",   vharm, "#d5b7c6", "#ce8fab",   "#c6678f",  rpun)}
      if(n == 7) {pal <- c(tsin, "#4a8aba", "#93b4ce",
                           vharm, "#d2aabd", "#c97498", rpun)}
      if(n == 5){pal <- c(tsin,"#6f9fc4",  vharm,"#ce8fab", rpun)}
      if(n == 3){pal <- c(tsin, vharm, rpun)}
      if(n == 1){pal <- c(tsin)}
      
      if(!n%in% c(1,3,5,7,9)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tsin, vharm, rpun))(n)}
        else{ 
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors")
          return(NULL)}
      }
      
    }
    ## Määrälliset K2, tsin to oranssi
    if(name == "K2") {
      tsin <- '#2f62ad'
      ora <- "#faa61a"
      if(is.null(n)) n <- 9
      if(n==9) pal <- c(tsin, "#387fb5", "#6f9fc4", "#a5bfd3", vharm,"#e4d1b0", "#ebc37e",  "#f3b44c", ora)
      if(n==7) pal <- c(tsin, "#4a8aba", "#93b4ce", vharm,  "#e6cc9f", "#f0b95d",ora)
      if(n==5) pal <- c(tsin, "#6f9fc4", vharm, "#ebc37e", ora)
      if(n==3) pal <- c(tsin, vharm, ora)
      if(n==1) pal <- tsin
      if(!n%in% c(1,3,5,7,9)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tsin, vharm, ora))(n)}
        else{ 
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors")
          return(NULL)}
      }
    }
    ## Määrälliset K3, syaani to oranssi
    if(name == "K3") {
      ssin <- "#29a0c1"
      ora <- "#faa61a"
      if(is.null(n)) n <- 9
      if(n==9){pal<-c(ssin, "#56b0c9",  "#83c0d2",   "#afcfda",     vharm, "#e4d1b0",
                      "#ebc37e", "#f3b44c",  ora)}
      if(n==7){pal<-c(ssin, "#7cb4cb", "#afc9d6",vharm, "#e6cc9f", "#f0b95d",   ora)}
      if(n==5){pal<- c(ssin,"#83c0d2",vharm,"#ebc37e",ora)}
      if(n==3){pal<- c(ssin,vharm,ora)}
      if(n==1){pal<- ssin}
      if(!n%in% c(1,3,5,7,9)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(ssin, vharm, ora))(n)}
        else{ 
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors")
          return(NULL)}
      }
    }
    ## Määrälliset K4, tvihr to rpun
    if(name == "K4") {
      rpun <- '#be3f72'
      tvihr <- "#519b2f"
      if(is.null(n)) n <- 9
      if(n==9){pal<-c(tvihr,"#8bba6e", "#a6c694", "#c1d3bb",
                      vharm,"#d5b7c6", "#ce8fab",
                      "#c6678f",rpun)}
      
      if(n==7){pal <- c(tvihr, "#94be7b", "#b8ceae",
                        vharm, 
                        "#d2aabd",
                        "#c97498",
                        rpun)}
      
      if(n==5){pal <- c(tvihr,"#a6c694", vharm,
                        "#ce8fab",rpun)}
      if(n==3){pal <- c(tvihr, vharm,rpun)}
      if(n==1){pal <- tvihr}
      if(!n%in% c(1,3,5,7,9)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tvihr, vharm, rpun))(n)}
        else{ 
          warning("The official palette ", name, " has either 1, 3, 5, 7 or 9 colors")
          return(NULL)}
      }
      
    }  
    ## Määrälliset K5, syaani to rpun
    if(name == "K5") {
      ssin <- "#29a0c1"
      rpun <- '#be3f72'
      if(is.null(n)) n <- 7
      if(n==7){pal <- c(ssin,"#7cb4cb", "#afc9d6",
                        vharm,   "#d2aabd","#c97498",  rpun)}
      if(n==5){pal <- c(ssin, "#83c0d2",vharm,
                        "#ce8fab",rpun)}
      if(n==3){pal <- c(ssin,vharm,rpun)}
      if(n==1) pal <- ssin
      if(!n%in% c(1,3,5,7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has either 1, 3, 5 or 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(ssin,vharm,rpun))(n)}
        else{  
          warning("The official palette ", name, " has either 1, 3, 5 or 7 colors")
          return(NULL)}
      }
    }  
    ## Määrälliset K6, tvihr to oranssi
    if(name == "K6") {
      tvihr <- "#519b2f"
      ora <- "#faa61a"
      if(is.null(n)) n <- 7
      if(n==7){pal <- c(tvihr, 
                        "#94be7b",
                        "#b8ceae",
                        vharm, 
                        "#e6cc9f",
                        "#f0b95d",
                        ora)}
      if(n==5){pal<- c(tvihr, 
                       "#a6c694",
                       vharm,
                       "#ebc37e",
                       ora)}
      if(n==3) pal <- c(tvihr,vharm,ora)
      if(n==1) pal <- tvihr
      if(!n%in% c(1,3,5,7)){
        if(isTRUE(allow.interpolation)){
          warning("The official palette ", name, " has either 1, 3, 5 or 7 colors, now palette generated with grDevices::colorRampPalette()")
          pal <- grDevices::colorRampPalette(c(tvihr,vharm,ora))(n)}
        else{ 
          warning("The official palette ", name, " has either 1, 3, 5 or 7 colors")
          return(NULL)}
      }
    }
  }
  thlShade(pal, alpha=alpha, thin=thin)  
}


