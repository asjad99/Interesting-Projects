
#------------------------------------------
#load data 
install.packages("data.table") 

library(data.table)

taxi_data = fread(file.choose(), header = T, sep = ',',data.table=FALSE)

#----------------------------------------------


#------------------------------------------------------------------------
#part A: Obtain a Plot of location values  
#------------------------------------------------------------------------


#Plot the location points (2D plot),


#create a dataframe
taxi_pickup_data <- data.frame(taxi_data[,.(Longitude)], taxi_data[,.(Latitude)])

taxi_pickup_data <- data.frame(taxi_data[,.(DriveNo)], taxi_data[,.(DateandTime)])


#basic plot
plot(taxi_pickup_data)


#since we are plotting 27million rows of data, the basic plot get us nothing but a splash of circles on the screen

#a better plot that is visually making sense can be obtained using gglot, and varying the size of dots(this also saves compute time)

install.packages("ggplot2")
library(ggplot2)

ggplot(taxi_data, aes(x= Longitude, y= Latitude)) +
             geom_point(size=0.02)


#--------------------------------------------------------------------------------
#Part A: Preprocessing, removing outliers 
#--------------------------------------------------------------------------------

#we create a Tranformation Function for preprocessing :


#creating a bounding box(based on rome city coordinates from google maps)

 nw <- list(lat = 40.5, lon = 13)
 se <- list(lat = 43, lon = 11)


applying Tranformation to raw data. We will convert time cloumn from character format 
to POSIXcT 


 trans <- function(x) { 
     
    # set coordinates outside of NYC bounding box to NA
   ind <- which(x$Longitude < nw$lon | x$Longitude > se$lon)
   x$dropoff_longitude[ind] <- NA
   ind <- which(x$Longitude < nw$lon | x$Longitude > se$lon)
   x$Longitude[ind] <- NA}


#Apply the Tranformation

taxi_data_subset = trans(taxi_data_subset)

#-------------------------------------------------------------------------------
# Part B:
#Compute the minimum, maximum, and mean location values 
#-------------------------------------------------------------------------------

#specs are not very clear on what we mean by min,mix location points.
#assuming that we mean numerical mean of data contained in individual columns
#this can be obtained using the summary command 


summary(taxi_data)
    DriveNo      Date and Time        Longitude        Latitude    
 Min.   :  2.0   Length:21817830    Min.   :41.45   Min.   :11.75  
 1st Qu.: 99.0   Class :character   1st Qu.:41.89   1st Qu.:12.47  
 Median :190.0   Mode  :character   Median :41.90   Median :12.48  
 Mean   :188.5                      Mean   :41.89   Mean   :12.47  
 3rd Qu.:275.0                      3rd Qu.:41.91   3rd Qu.:12.50  
 Max.   :374.0                      Max.   :42.30   Max.   :12.96 


From the data above we can say that 

#Min location Point is (41.45,11.75)
#Max location Point is (41.45,11.75)
#Mean location Point is (42.30,12.96)


#TODO:  take a look at sp and spatstat


#-------------------------------------------------------------------------------
# Part C:
#calculate taxi Activity (min,max,average time driven)
#-------------------------------------------------------------------------------

#to save time we will subset the data(1 million rows)

taxi_data_subset = taxi_data[1:1000000,]

#Removing dublicates

	taxi_data_subset = taxi_data_subset[!duplicated(taxi_data_subset[1:3]), ] 

    #output: no dublicates are found 

    #in first 1 million rows. 



#TODO: Add command and plot here
#compute histogram to compare acitivity of different drivers 


#-------------------------------------------------------------------------------

#TODO: Add code in this section


#alternatively we can compute the time traveled by each driver


compute_distance_travelled <- function(taxi_data) { 

library(dplyr)

times <- as.difftime(taxi_data[,2],"%Y-%m-%d %H:%M:%OS",units = "secs")

mydata <- data.frame(time = times,group = taxi_data[,1])


summarise(group_by(mydata, group), sum(time))

}

compute_distance_travelled(taxi_data_subset)




#compute the max,min,avg. time driven 


#TOOD: Find out the driverNo using studnet number

#-------------------------------------------------------------------------------
# Part D:
#-------------------------------------------------------------------------------

#Part D:  Plots the location points of taxi=ID
#-------------------------------------------------------------------------------


plot_taxi_id <- function(DriverID,taxi_data) { 


#create a dataframe containing only the rows of that taxi driver 
temp_dataf <- taxi_data[taxi_data$DriveNo == DriverID,]


install.packages("ggplot2")
library(ggplot2)

ggplot(temp_dataf, aes(x= Longitude, y= Latitude)) +
             geom_point(size=0.02)
            
}


plot_taxi_id(245,taxi)


#TODO: obtain the plot 


#Part D)i:  calculate taxi Activity 
#-------------------------------------------------------------------------------

plot_taxi_id <- function(DriverID,taxi_data) { 


#create a dataframe containing only the rows of that taxi driver 
temp_dataf <- taxi_data[taxi_data$DriveNo == DriverID,]


install.packages("ggplot2")
library(ggplot2)

ggplot(temp_dataf, aes(x= Longitude, y= Latitude)) +
             geom_point(size=0.02)
            
}


plot_taxi_id(245,taxi_data_subset)

#TODO: Add a plot here


#Part D)ii):  Compute the minimum, maximum, and mean location values
#-------------------------------------------------------------------------------

min_max_location <- function(DriverID,taxi_data) { 


#create a dataframe containing only the rows of that taxi driver 
temp_dataf <- taxi_data[taxi_data$DriveNo == DriverID,]

 summary(temp_dataf)

}

min_max_location(245,taxi_data_subset)

output: 
  DriveNo    Date and Time        Longitude        Latitude    
 Min.   :245   Length:2031        Min.   :41.86   Min.   :12.43  
 1st Qu.:245   Class :character   1st Qu.:41.90   1st Qu.:12.48  
 Median :245   Mode  :character   Median :41.91   Median :12.49  
 Mean   :245                      Mean   :41.91   Mean   :12.49  
 3rd Qu.:245                      3rd Qu.:41.92   3rd Qu.:12.50  
 Max.   :245                      Max.   :41.96   Max.   :12.54  



#Global Values: 
#Min location Point is (41.45,11.75)
#Max location Point is (41.45,11.75)
#Mean location Point is (42.30,12.96)

#values for DriverNo = 245:

#Min location Point is (41.86,12.43)
#Max location Point is (41.96,12.54)
#Mean location Point is (41.91,12.49)


#Part D)ii): Compare total time driven by taxi=ID with the global mean, min, and max values.
#-------------------------------------------------------------------------------------------


compute_timedriven <- function(DriverID,taxi_data) { 


#create a dataframe containing only the rows of that taxi driver 
temp_dataf <- taxi_data[taxi_data$DriveNo == DriverID,3:4]

 summary(temp_dataf)


times <- as.difftimeDate(temp_dataf[,2],"%Y-%m-%d %H:%M:%OS",units = "secs")

sum(times)

}

compute_timedriven (245,taxi_data_subset)


#-------------------------------------------------------------------------------------------



#Part D)iii): Compute the distance traveled by taxi=ID
#-------------------------------------------------------------------------------------------
temp_dataf <- taxi_data[taxi_data$DriveNo == DriverID,3:4]



compute_distance_travelled <- function(DriverID,taxi_data) { 


#create a dataframe containing only the rows of that taxi driver 
temp_dataf <- taxi_data[taxi_data$DriveNo == DriverID,3:4]

 summary(temp_dataf)


times <- as.difftimeDate(temp_dataf[,2],"%Y-%m-%d %H:%M:%OS",units = "secs")

sum(times)




}

#-------------------------------------------------------------------------------------------















































Total_time = 0 

for index, row in taxi_data_subset.iterrows():

	temp = difftime(row+1[,2], row + 1[,2],units="secs") time diff between two rows 
	total time = temp+total time 


Total_time

}

#taxi_data_subset_2 contains Driver IDs and Time in POSIXt format 

compute_timedriven (245,taxi_data_subset)





#create a new dataframe. Column 1 is driver no. column 2 is Data and time 

taxi_data_subset2 <- data.frame(taxi_data_subset[,1] ,strptime(taxi_data_subset[,2],"%Y-%m-%d %H:%M:%OS"))


#compute total time travelled of each driver 

Total time = 0 

for index, row in df.iterrows():
	temp = diff(row, row + 1) time diff between two rows 
	total time = temp+total time 






other commands: 
rm(taxi_data_2)   	#remove variable 

DM_taxi_data = data.matrix(taxi_data) 	#covert data frame to data matrix 

> temp <- strptime(taxi_data[1,2],"%Y-%m-%d %H:%M:%OS")   #convert between character representations and objects of classes "POSIXlt" and "POSIXct" representing calendar dates and times.

taxi_data_2 <-strptime(taxi_data[,2],"%Y-%m-%d %H:%M:%OS")	#extracts the timestamp column and stores it into a matrix of the POSIXlt format


Dealing with timestamp in R: http://stackoverflow.com/questions/1962278/dealing-with-timestamps-in-r 
