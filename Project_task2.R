#load data
install.packages("data.table")
library(data.table)
taxi_data = fread(file.choose(), header = T, sep = ',',data.table=FALSE)


taxiData <- read.csv("taxi.csv", header = TRUE)


#------------------------------------------------------------


stepPoint <- function(x1,y1,x2,y2,x3,y3,s) { #(x,y) is the latitude and longitude and s is the step size
  u <- c(x2-x1, y2-y1)
  v <- c(x3-x2, y3-y2)
  umag = length(u)
  vmag = length(v)
  A <- acos(crossprod(u,v)/umag*vmag)
  if ( (umag*sin(A)/s) > 1) {
    
    # no solution
    point <- c(-1000, -1000)
    return(point)
  }
  
  U <- asin(umag*sin(A)/s)
  t <- 180-U-A
  d <- sqrt(umag^2 +s^2 - 2*umag*s*cos(t))
  r <- (v / vmag) * d
  rmag <- length(r)
  w <- atan2(v[2],v[1])
  x <- rmag*cos(w)
  y <- rmag*sin(w)
  point <- c(x,y)
  return(point)
}