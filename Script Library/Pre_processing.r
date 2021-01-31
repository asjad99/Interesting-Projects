#swapping the names of the columns in the data set 
#taxidata3 
names(taxidata3)[3]<-paste("Latitude")
names(taxidata3)[4]<-paste("Longitude")