#load data
install.packages("data.table")
library(data.table)
taxi_data = read.csv(file.choose(), header = T)

#subset data
taxi_data_subset = taxi_data[1:200000,]

time.converterog <- function (x) 
{
  x = as.character(x)
  op <- options(digits.secs=6)
  x = substring(x,11,19)
  x = strptime(x, "%H:%M:%OS")
  options(digits=6)
  x = as.numeric(x)
  
  options(op)
  
  
  return(x)
}

#convert timestamp into timezones 
time.converter <- function (x) 
{
  #x=  "2014-02-01 00:00:00739166+01"
  x = as.character(x)
  op <- options(digits.secs=6)
  x = substring(x,11,19)
  #print(x)
  
  x = strptime(x, "%H:%M:%OS")
  #print(x)
  options(digits=6)
  x = as.numeric(x)
  #print(x)
  options(op)
  #print(x)
  
  val = 99
  
  #print("between 12-6")
  if ( x >= 1463839200 && x <= 1463860800)
    val = 1 
  
  #print("between 6-9")
  if (x > 1463860800 && x <= 1463871600)
    val = 2
  
  #print("between 9:00-16:00")
  if ( x > 1463871600 && x <= 1463896800)
    val = 3
 
  #print("between 16:00-21:00")
  if ( x > 1463896800 && x <= 1463914800)
    val = 4
   
  #print("between 21:00-24:00")
  if ( x > 1463914800 && x <= 1463925599)
    val = 5
  
  return(val)
}

time.converter2 <- function (x) 
{

  return(99)
}

testt= "2014-02-01 16:48:15.139973+01"
testt=time.converter(testt)
testt


taxi_data_subset$Date.and.Time = time.converter2(taxi_data_subset$Date.and.Time)

temp = taxi_data_subset[99000,]
temp


# Sample usage
 testt= "2014-02-01 7:00:00739166+01"
 testt=time.converterog(testt)
 testt
# 1391173200

# Sample usage
 testt= "2014-02-01 7:00:00739166+01"
 testt=time.converter(testt)
 testt
# 1391173200

testt = ""
testt=time.converter("2014-02-01 11:12:44.833642+01")
testt

taxiData <- read.csv("taxi.csv", header = TRUE)

mystr = substring("2014-02-01 00:00:28.50404+01",11,19)
summary(mystr)
a = time.converter(mystr)
a
testval = time.converter("2014-02-01 11:12:44.833642+01")
testval #1463839200

testval = time.converter("2014-02-01 6:12:44.833642+01")
testval #1463860800

testval = time.converter("2014-02-01 18:12:44.833642+01")
testval #1463868000

testval = time.converter("2014-02-01 22:12:44.833642+01")
testval #1463896800

testval = time.converter("21:00:00")
testval  #1463914800

 

val = 0 

x= time.converter("22:00:00")


taxi_pickup_data <- data.frame(taxi_data_subset[,.(DriveNo)],taxi_data_subset[,.(Longitude)], taxi_data_subset[,.(Latitude)])

results = kmeans(x, centers)