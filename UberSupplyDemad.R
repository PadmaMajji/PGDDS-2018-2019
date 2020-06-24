
#Load the Uber Request Data set

Uber_Request <- read.csv("Uber Request Data.csv", stringsAsFactors = F)

# Load the libraries

library(stringr)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

#Find duplicate values in Data set

sum(duplicated(Uber_Request$Request.id))

#Find NA values

sum(is.na(Uber_Request$Request.id))
sum(is.na(Uber_Request$Pickup.point))
sum(is.na(Uber_Request$Status))
sum(is.na(Uber_Request$Request.timestamp))
sum(is.na(Uber_Request$Drop.timestamp))

# convert date and time column in proper format

Uber_Request$Request.timestamp <- str_replace_all(Uber_Request$Request.timestamp, "[/]",  "-")
Uber_Request$Drop.timestamp<- str_replace_all(Uber_Request$Drop.timestamp, "[/]",  "-")

View(Uber_Request)

# convert time into consitent format in date and time column
Uber_Request$Request.timestamp <-strptime(Uber_Request$Request.timestamp, format = "%d-%m-%Y %H:%M")
Uber_Request$Drop.timestamp <- strptime(Uber_Request$Drop.timestamp, format = "%d-%m-%Y %H:%M")

View(Uber_Request)


#Extract the hour and day data from the request time
Uber_Request$Request_hr <- format(Uber_Request$Request.timestamp, "%H")
Uber_Request$Request_day <- format(Uber_Request$Request.timestamp, "%d")

View(Uber_Request)

Uber_Request$Drop_hr <- format(Uber_Request$Drop.timestamp, "%H")
Uber_Request$Drop_day <- format(Uber_Request$Drop.timestamp, "%d")

View(Uber_Request)

# Demand and Supply from Airport and City
Airport_demand <- ggplot(subset(Uber_Request, Uber_Request$Pickup.point =="Airport"), aes(x=Request_hr, fill=Status)) + geom_bar() +ggtitle("Demand at Airport")
Airport_demand
Airport_supply <- ggplot(subset(subset(Uber_Request, Uber_Request$Pickup.point== "Airport"),!is.na(Drop_hr)), aes(Drop_hr)) + geom_bar(fill ="purple") +ggtitle("Supply from Airport")
Airport_supply

City_demand <- ggplot(subset(Uber_Request, Uber_Request$Pickup.point =="City"), aes(x=Request_hr,fill= Status)) + geom_bar() +ggtitle("Demand at City")
City_demand
City_supply <- ggplot(subset(subset(Uber_Request, Uber_Request$Pickup.point== "City"),!is.na(Drop_hr)), aes(Drop_hr)) + geom_bar(fill ="purple") +ggtitle("Supply from City")
City_supply

# OVerall trend in demand and supply
plot_grid(Airport_demand, Airport_supply, City_demand, City_supply)


#Number of Requests per hour

ggplot(Uber_Request, aes(x = as.factor(Request_hr),fill = Pickup.point))+geom_bar(position = "dodge")+labs(x = "Hour", y = "Number of Requests", fill = "Pickup Point" )

#converting Request hour to numeric
Uber_Request$Request_hr <- as.numeric(Uber_Request$Request_hr)

#making slots by number of trips

Uber_Request$Time_Slot = ifelse(Uber_Request$Request_hr < 10,"Morning",ifelse(Uber_Request$Request_hr < 17,"Day",ifelse(Uber_Request$Request_hr < 22,"Evening","Late Night")))
View(Uber_Request)

#finding the number of trips made in each slot
nrow(subset(Uber_Request, Uber_Request$Time_Slot == "Morning"))
nrow(subset(Uber_Request, Uber_Request$Time_Slot == "Day"))
nrow(subset(Uber_Request, Uber_Request$Time_Slot == "Evening"))
nrow(subset(Uber_Request, Uber_Request$Time_Slot == "Late Night"))

#Plot and Identifying the most critical Time slots
ggplot(Uber_Request, aes(x = as.factor(Time_Slot), fill= as.factor(Uber_Request$Status))) + geom_bar()+labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )+ggtitle("Time Slots vs Requests")

#Problem 1 - High cancellations in the morning
Morning_requests <- subset(Uber_Request,Time_Slot=="Morning")
ggplot(Morning_requests, aes(x = as.factor(Pickup.point), fill= as.factor(Morning_requests$Status))) + geom_bar() +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )+ggtitle("Morning Pickup Vs Number of Requests")

#Cancellation from City and Airport
nrow(subset(Morning_requests, Morning_requests$Pickup.point == "Airport" & Morning_requests$Status == "Cancelled"))
nrow(subset(Morning_requests, Morning_requests$Pickup.point == "City" & Morning_requests$Status == "Cancelled"))

#Percentage of Cancellation from City
data_problem1 <- subset(Morning_requests, Pickup.point %in% "City")
ggplot(data_problem1, aes(x = data_problem1$Pickup.point, fill= as.factor(data_problem1$Status))) + geom_bar() + coord_polar(theta = "y", start=0)+ labs( y = "Number of Requests", x = "", fill = "Status")+ggtitle("Morning Cancellation Status")

#Supply and Demand
Supply_City <- nrow(subset(Morning_requests, Morning_requests$Pickup.point == "City" & Morning_requests$Status == "Trip Completed"))
Supply_City
Demand_City <- nrow(subset(Morning_requests, Morning_requests$Pickup.point == "City"))
Demand_City

#Problem 2 - No cars available in the evening 
Evening_requests <- subset(Uber_Request,Time_Slot=="Evening")
ggplot(Evening_requests, aes(x = as.factor(Pickup.point), fill= as.factor(Evening_requests$Status))) + geom_bar()+labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )+ggtitle("Evening Pickup Vs Number of Requests")

#No Cars available from City and Airport
nrow(subset(Evening_requests, Evening_requests$Pickup.point == "Airport" & Evening_requests$Status == "No Cars Available"))
nrow(subset(Evening_requests, Evening_requests$Pickup.point == "City" & Evening_requests$Status == "No Cars Available"))

# Percentage of No cars available from Airport
data_problem2 <- subset(Evening_requests, Pickup.point %in% "Airport")
ggplot(data_problem2, aes(x = data_problem2$Pickup.point, fill= as.factor(data_problem2$Status))) + geom_bar() + coord_polar(theta = "y", start=0) + labs( y = "Number of Requests", x = "", fill = "Status")+ggtitle("Evening Unavailability of Cars")

#Supply and Demand at Airport
Supply_Airport <- nrow(subset(Evening_requests, Evening_requests$Pickup.point == "Airport" & Evening_requests$Status == "Trip Completed"))
Supply_Airport
Demand_Airport <- nrow(subset(Evening_requests, Evening_requests$Pickup.point == "Airport"))
Demand_Airport
