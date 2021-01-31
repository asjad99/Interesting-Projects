#Important : this function requires the input to be in numeric format like the output of the time.converter function
#Sum time function is useful for calculating the time for a group using a group by or aggregate function
#it is a customized version of the "sum" functions for calculating the sum of timestamps
#Note the statement if(timediff>3600) decides the threshold of 1 hour This can be changed to any value
#At the moment the function assumes that if the driver was inactive for more than an hour then he is on break
#or off duty. This value can be changed as per requirement

sum.time <-function(...,na.rm=FALSE)
{
  dots = list(...)
  dots_int = unlist(dots)
  ndots = lengths(dots)
  
  for(i in 1:ndots)
  {
    
    if(i==1)
    {
      totsum = 0
      
    }
    
    else
    {
      
      timediff = abs(dots_int[i]-dots_int[i-1])
      if(timediff>3600) # Threshold value is 3600. This value can be changed
      {
        timediff=0
      }
      else
      {
        totsum = totsum + timediff
      }
      
    }
  }
  return(totsum)
  
}

#Sample use of the aggregate function 
#output <- aggregate(Date.and.Time ~ DriveNo,newframe,FUN = sum.time)
# This function will sum all the time for a particular driver number 

#Sample Code for subsetting columns
## selecting just two columns
#taxidata3.t <- taxidata3[, c("DriveNo", "Date.and.Time")]