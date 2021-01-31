taxi_data = read.csv(file.choose(), header = T)

taxi_data_subset_driver = taxi_data[taxi_data$DriveNo==352,]

taxi_data_subset_driver <- subset(taxi_data,(DriveNo == 352)|(DriveNo==135)|(DriveNo==365)|(DriveNo==228)|(DriveNo==94))

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
  
 # val = 99
  
  #print("between 12-6")
  if ( x >= 1463839200 && x <= 1463860800)
    val = 1 
  
 # #print("between 6-9")
  if (x > 1463860800 && x <= 1463871600)
    val = 2
  
  ##print("between 9:00-16:00")
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

taxi_data_subset = taxidata[1:200000,]
taxi_data_subset_bk = taxi_data_subset

taxi_data_subset = taxi_data_subset[-c(2)]

taxi_data_subset <- scale(taxi_data_subset)

head(taxi_data_subset)
testt=time.converter("2014-03-02 10:45:11.069019+01")
testt

testt=time.converter("2014-02-01 12:22:25.804404+01")
testt

testt=time.converter("2014-02-01 07:57:01.808798+01")
testt

taxi_data_subset[15445,]
taxi_data_subset[113778,]
taxi_data_subset[57461,]

taxi_data_subset[99000,]

#taxi_data_subset$Date.and.Time <- lapply(taxi_data_subset$Date.and.Time,time.converter)


taxi_data_subset$timezone <- lapply(taxi_data_subset$Date.and.Time,time.converter)


taxi_data_subset_driver$timezone <- lapply(taxi_data_subset_driver$Date.and.Time,time.converter)


results = kmeans(taxi_data_subset,5, iter.max = 10000, nstart = 1,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                     "MacQueen"), trace=FALSE)
install.packages("reshape2")
library(cluster)
library(fpc)
library(rgl)
library(reshape2)
plot3d(taxi_data_subset$Longitude,taxi_data_subset$Latitude,taxi_data_subset$Date.and.Time,col= results$cluster)