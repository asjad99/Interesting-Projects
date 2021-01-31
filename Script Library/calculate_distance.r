 
#This function is used for distance calculations
# The input for the function are two sets of coordinates for 2 location points
#The function will return the distance between these two location points
#Note: The value used for R is 6378.145 which is a more accurate value. But this can just be changed in the function
#if required
#long1  = longitude of point1
#lat1  = latitude of point 1
#long2 = longitude of point 2
#lat2= latitude of part 2
#d = the value of distance returned by the function

calculate.distance <- function (long1, lat1, long2, lat2) 
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
