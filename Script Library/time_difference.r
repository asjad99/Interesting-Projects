#Returns the time difference between two time stamps
#The input is the two time stamps tim1 and tim2
#The output is td :- the time difference
time.difference <- function (tim1,tim2) 
{
  tim1 = as.character(tim1)
  tim2 = as.character(tim2)
  tm1 = strptime(tim1, "%Y-%m-%d %H:%M:%OS")
  print(tm1)
  op <- options(digits.secs=6)
  
  tm2 = strptime(tim2, "%Y-%m-%d %H:%M:%OS")
  print(tm2)
  op <- options(digits.secs=6)
  options(op)
  
  options(digits=6)
  td = as.numeric(tm2-tm1)
  print(td)
  td = abs(td)
  
  return(td)
}

#Sample Run
# t1 = "2010-01-15 13:56:23.739166+01"
# t2 = "2010-01-15 13:57:23.839166+01"
# testing1 <- time.difference(t1,t2)

#testing1
# 1.00167
