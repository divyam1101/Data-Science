#load the uber data in the data frame from csv file
uber_request_data <- read.csv("Uber Request Data.csv" , stringsAsFactors = FALSE)

#changing name of columns and removing period
colnames(uber_request_data) <- gsub("\\.","_",colnames(uber_request_data))

#changing pickup point and status as factors
uber_request_data$Pickup_point <- as.factor(uber_request_data$Pickup_point)
uber_request_data$Status <- as.factor(uber_request_data$Status)


#some data facts analyzed at looking at data
str(uber_request_data)
summary(uber_request_data)

#all the available data is for 11 to 15 in the month July of 2016 

#When trip is cancelled or no car available for the same the there is no drop time(NA)
#When the cars were not available then there is no driver id associated with it

#keeping driver id where it is NA as NA because we dont want to be it part of our frequent driver analysis
#treatment of drop timestmap as NA not required because we have not taken it as a part of our analysis


#converting columns drop and request timestamp to date time and assigning it to new columns
#extracting  hour to do a segemented analysis
convert_to_datetime <- function(x)
{
  if (grepl("[0-9]*/[0-9]*/[0-9]* [0-9]*:[0-9]*",x))
  {
    return(as.POSIXlt(x, format = "%d/%m/%Y %H:%M"))
  }
  else
  {
    return(as.POSIXlt(x,format = "%d-%m-%Y %H:%M"))
  }
}

uber_request_data$Request_Hours <- sapply(uber_request_data$Request_timestamp , function(x) {format(convert_to_datetime(x),"%H")})
uber_request_data$Drop_Hours <- sapply(uber_request_data$Drop_timestamp , function(x) {format(convert_to_datetime(x),"%H")})

#derive the day from the date to do a segemented analyis

uber_request_data$Request_Day_Of_Week <- sapply(uber_request_data$Request_timestamp , function(x) {format(convert_to_datetime(x),"%A")})
uber_request_data$Drop_Day_Of_Week <- sapply(uber_request_data$Drop_timestamp , function(x) {format(convert_to_datetime(x),"%A")})

uber_request_data$Request_Day_Of_Week <- as.factor(uber_request_data$Request_Day_Of_Week)
uber_request_data$Drop_Day_Of_Week <- as.factor(uber_request_data$Drop_Day_Of_Week)

#derive the day into various slot like morning (before 10am), afternoon (from 10am-2pm) and evening (2pm -6pm) and night (6pm-12) from the date to do a segemented analyis

convert_to_dayslot <- function(x)
{
  if (is.na(x))
  {
    return(x)
  }
  if (x <= 10)
  {
    return("Morning")
  } else if (x >10 & x <14)
  {
    return("Afternoon")
  } else 
  {
    return("Evening")
  }
}

uber_request_data$Pickup_time_slot  <- sapply(uber_request_data$Request_Hours , convert_to_dayslot)
uber_request_data$Drop_time_slot  <- sapply(uber_request_data$Drop_Hours , convert_to_dayslot)

uber_request_data$Pickup_time_slot <- as.factor(uber_request_data$Pickup_time_slot)
uber_request_data$Drop_time_slot <- as.factor(uber_request_data$Drop_time_slot)

uber_request_data$Drop_timestamp <- sapply(uber_request_data$Drop_timestamp , function(x) {format(convert_to_datetime(x),"%d-%m-%Y %H:%M:%S")})
uber_request_data$Request_timestamp <- sapply(uber_request_data$Request_timestamp , function(x) {format(convert_to_datetime(x),"%d-%m-%Y %H:%M:%S")})

#derived metrics created for most frequent drivers (drivers who have atleast 15 trips regardless of status from airport to city)

library(sqldf)
frequent_driver <- sqldf("select Driver_id from (select Driver_id , count(Driver_id)  from uber_request_data group by Driver_id having  count(Driver_id) >=15)")
uber_request_data$is_frequent_Driver <- sapply(uber_request_data$Driver_id , function(x) any(x== frequent_driver))

str(uber_request_data)
summary(uber_request_data)


#focus is on doing a analysis on the problematic areas of Uber - trip cancelled or no cars avaiable
#visualizing the frequency of problematic areas with respect to the trip done from airport to city or vice versa

install.packages("ggplot2")
library(ggplot2)

#Segemented Analysis on Pickup Point , plotting of bar graph based on the status of the trip per route
uber_bar <- ggplot(uber_request_data , aes(x=Pickup_point))
uber_bar + aes(fill = Status)+ geom_bar() + ggtitle("Frequency of Trip Status per Route") +
  xlab("Route") + ylab("Frequency")

#Segemented Analysis on Pickup Point , plotting of bar graph based on the status of the trip per slot
level_order <- c('Morning', 'Afternoon', 'Evening')
uber_bar <- ggplot(uber_request_data , aes(x=factor(Pickup_time_slot, level = level_order)))
uber_bar + aes(fill = Status)+ geom_bar() + ggtitle("Frequency of Trip Status per Time Slot") +
  xlab("Time Slot") + ylab("Frequency")


#demand pattern throughout the day per Route
uber_request_pattern <- ggplot(uber_request_data, aes(x = Request_Hours, fill = Pickup_point)) 
uber_request_pattern_route_plot <- uber_request_pattern +  geom_bar() + ggtitle("Cab demand pattern of the Day for Pickup Point") +
  xlab("Request Hour") + ylab("Frequency")

#demand pattern throughout the day per Status
uber_request_pattern_status <- ggplot(uber_request_data, aes(x = Request_Hours, fill = Status)) 
uber_request_pattern_status_plot <- uber_request_pattern_status +  geom_bar() + ggtitle("Cab demand pattern of the Day for Status") +
  xlab("Request Hour") + ylab("Frequency")

install.packages("gridExtra")
library(gridExtra)
library(grid)
grid.arrange(uber_request_pattern_route_plot,uber_request_pattern_status_plot, ncol=2,top = textGrob("Demand Supply Pattern for All Drivers",gp=gpar(fontsize=12,font=2,lty = "solid")))
 

#Findings from the above graphs
#Demand is high (>200 requests) in morning from City to Airport between 4am-10am
#Demand is high (>200 requests) in evening from Airport to City between 5pm-11pm
#Cancellation is highest in the morning (for City to Aiport)
#Cars Unavilability is highest in the evening (for Airport to City)
#Afternoon from 10am-3pm the demand is very less and cars unavailibility and cancellation is also less compared to demand

#With above analysis it can be infered that demand supply gap is highest in Night because the inflow of cabs from city to aiport is less than what is required
#Reason for cancellation in morning can be heavy traffic during morning business hour and less air traffic during morning hours which doesnt deliver 
#motivate drivers to go to airport more in morning hours by providing better rates to them if they go towards aiport
#create a parking area for uber cabs at airport and cab allocation must be done in first in first out manner rather than random 

#validating analysis by analyzing pattern for most frequent drivers (drivers who have 15 or more request)seeing if they follow the saame trends

uber_frequent_driver_request_data <- uber_request_data[which(uber_request_data$is_frequent_Driver == TRUE),]

#demand pattern throughout the day per Route
uber_request_pattern_frequent_driver <- ggplot(uber_frequent_driver_request_data, aes(x = Request_Hours, fill = Pickup_point)) 
uber_request_pattern_route_frequent_driver_plot <- uber_request_pattern_frequent_driver +  geom_bar() + ggtitle("Cab demand pattern of the Day for Pickup Point") +
  xlab("Request Hour") + ylab("Frequency")

#demand pattern throughout the day per Status
uber_request_pattern_status_frequent_driver <- ggplot(uber_frequent_driver_request_data, aes(x = Request_Hours, fill = Status)) 
uber_request_pattern_status_frequent_driver_plot <- uber_request_pattern_status_frequent_driver +  geom_bar() + ggtitle("Cab demand pattern of the Day for Status") +
  xlab("Request Hour") + ylab("Frequency")

grid.arrange(uber_request_pattern_route_frequent_driver_plot,uber_request_pattern_status_frequent_driver_plot, ncol=2 ,top = textGrob("Demand Supply Pattern for Frequent Driver",gp=gpar(fontsize=12,font=2,lty = "solid")))

#as we see the trend seems similar for frquent drivers also so we validate  our previous claim
