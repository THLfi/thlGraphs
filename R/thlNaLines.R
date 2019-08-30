#' Linear interpolation of missing values with dotted line in line plots
#' @param data dataset
#' @param xvar x axis variable
#' @param yvar y thlNaLines
#' @param groupvar omit setting NULL
#' @author Salla Toikkanen,  Tarja Palosaari, Petteri Mäntymaa
#' @export

thlNaLines <- function(data, xvar, yvar, groupvar){
  
  foodf<-data[is.na(data[,yvar]),]
  missing.list<-lapply(as.list(1:dim(foodf)[1]), function(x) foodf[x[1],])
  todo <- !all(sapply(missing.list, function(a) all(is.na(a))) == TRUE)

  if(isTRUE(todo)){ 
    vals <- lapply(missing.list, function (a) {
      if(is.factor(data[,xvar])){
        data[,xvar] <- as.character(data[,xvar])
        a[,xvar]<-as.character(a[,xvar])
      }
      mindf <- subset(data, get(xvar) < a[,xvar] & !is.na(get(yvar)))
      maxdf <- subset(data, get(xvar) > a[,xvar] & !is.na(get(yvar)))
      
      if(!is.null(groupvar)) {
        mindf <- subset(mindf, get(groupvar) == a[,groupvar])
        maxdf <- subset(maxdf, get(groupvar)== a[,groupvar])
      }
      maxx <- min(maxdf[,xvar], na.rm=T)
      minx <- max(mindf[,xvar], na.rm=T)
      
      rbind(subset(mindf, get(xvar) == minx),
            subset(maxdf, get(xvar) == maxx))
      
    })
    interp<-unique(do.call("rbind",vals))
  }
  else(interp <- NULL)
  interp
}   
