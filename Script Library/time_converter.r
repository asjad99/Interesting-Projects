#converts the time to numeric format for the purpose of calculations
#input takes a time stamp
#output is the timestamp in numeric format

time.converter <- function (x) 
{
  x = as.character(x)
  op <- options(digits.secs=6)
  
  x = strptime(x, "%Y-%m-%d %H:%M:%OS")
  options(digits=6)
  x = as.numeric(x)
  
  options(op)
  
  
  return(x)
}

# Sample usage
# testt= "2014-02-01 00:00:00739166+01"
# testt=time.converter(testt)
# testt
# 1391173200