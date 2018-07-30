####################################################################################

#              **************UBER ASSIGNMENT***************

####################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)

#setting up working directory----------------------------------------------

#setwd("C:\\Users\\sony\\Documents\\uber")

uber_ds <- read.csv("Uber Request Data.csv")
str(uber_ds)

#Request.id and Driver.id should be unique for every request and driver therefore should be converted
#into factor .

uber_ds$Request.id <- as.factor(uber_ds$Request.id)
uber_ds$Driver.id <- as.factor(uber_ds$Driver.id)

str(uber_ds)


######............ DATA CLEANING & DATA PREPARATION............. ########

#Making the date format to standard , Removing "/" to "-" ----------------------------

uber_ds$Request.timestamp <- str_replace_all(uber_ds$Request.timestamp, "/", "-")
index <- str_which(uber_ds$Request.timestamp, "[:space:][:digit:]{1,2}:[:digit:]{1,2}$")
uber_ds$Request.timestamp[index] <- paste(uber_ds$Request.timestamp[index], ":00", sep ="" )
uber_ds$Request.timestamp <- as.POSIXct(uber_ds$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")

uber_ds$Drop.timestamp <- str_replace_all(uber_ds$Drop.timestamp, "/", "-")
index <- str_which(uber_ds$Drop.timestamp, "[:space:][:digit:]{1,2}:[:digit:]{1,2}$")
uber_ds$Drop.timestamp[index] <- paste(uber_ds$Drop.timestamp[index], ":00", sep ="" )
uber_ds$Drop.timestamp <- as.POSIXct(uber_ds$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

#Now converting date $ time both to standard format -------------------------------------


#Creating separate columns for day , month , year , hour and minute from date & time 

uber_ds$req_date <- format(uber_ds$Request.timestamp,"%d")
uber_ds$req_month <- format(uber_ds$Request.timestamp,"%m")
uber_ds$req_year <- format(uber_ds$Request.timestamp,"%Y")
uber_ds$req_hr <- format(uber_ds$Request.timestamp,"%H")
uber_ds$req_min <- format(uber_ds$Request.timestamp,"%M")

#Separating only the hour and minute for the Drop Time 

uber_ds$drop_hr <- format(uber_ds$Drop.timestamp,"%H")
uber_ds$drop_min <- format(uber_ds$Drop.timestamp,"%M")


#calculating the total time taken for each journey from diffrent pickups 

uber_ds$journey_mins <- uber_ds$Drop.timestamp - uber_ds$Request.timestamp

#-----------------------UNIVARIATE ANALYSIS -----------------------------------------

#To check if any duplicate request exists ?

nrow(uber_ds)

apply(uber_ds,2,function(x)length(unique(x)))

#Each request is unique .
#There are 2 pickup points from airport to city and vice versa .
#3 level of status , Trip Completed , No cars Available , Cancelled 


#----------------------BIVARIARTE & SEGMENTED ANALYSIS ---------------------------------


#Creating Plots to visualise the frequency of request that get Cancelled , Trip completed and 
#No cars available , and finding out the status causing problem .

ggplot(data = uber_ds,aes(x=uber_ds$Status))+geom_histogram(stat = "count")+geom_text(stat = "count", aes(label=..count..), vjust=-1,position = position_dodge(0.9))+labs(title = "Frequency of Cab Request based on Trip Status",x="Cab Request in a day(hrs)")

#ggsave("Status_Freq.pdf")---------------------------

ggplot(data = uber_ds,aes(x=uber_ds$Pickup.point))+geom_histogram(stat = "count")+geom_text(stat = "count", aes(label=..count..), vjust=-1,position = position_dodge(0.9))+labs(title = "Frequency of Cab Request by Pickup point",x="Pickup points")

#Now Identifying the frequency of Trips completed,cancelled, and No cars available based on te pickup points

ggplot(data = uber_ds,aes(x=uber_ds$Status,fill = uber_ds$Pickup.point))+geom_histogram(position = "stack",stat = "count")+labs(title  = "Frequenct of Status for Cab Request",subtitle = "Considering the Pickup Points",x="Cab Request in a day(hrs)")



#---------------------Analysis From Plot 1 and 2 ---------------------------

#Cancellation and No cars Available are more than Trip completed . 

#More than 50 % of the request are either getting cancelled or No cars are available , Which is causing Uber a problem . 

#One More insight is that , the  No Cars Available status is more for trip Airport to City .
# and Cancellation of request are more for trips City to Airport .

#-------------------------------------------------------------------------------


#Finding out the demand every day and Identifying the peak hours through plot ------

ggplot(data = uber_ds,aes(x=uber_ds$req_hr))+geom_bar()+geom_text(stat = "count", aes(label=..count..), vjust=-1,position = position_dodge(0.9)) +labs(title = "Cab Request Pattern during the day(hrs)",subtitle= "Peak hours is between 5AM-10AM & 5PM-10PM",x= "Cab Request in a day(hrs)")


#Through this plot , we have derived the morning peak hours and the evening peak hours -------

uber_ds$is_peakhrs <- factor(ifelse(uber_ds$req_hr>="05" & uber_ds$req_hr<="10","morn_peakhr",ifelse(uber_ds$req_hr>="17"& uber_ds$req_hr<="22","evng_peakhr","non_peakhr")))

str(uber_ds)

#Plot Showing the Frequency of Request During Morning peak hour ,Evening peak hour , and non peak hour . 

ggplot(data = uber_ds,aes(x=uber_ds$is_peakhrs))+geom_bar()+geom_text(stat = "count", aes(label=..count..), vjust=-1,position = position_dodge(0.9))+labs(title = "Cab Request Pattern during the day(hrs)",subtitle= "Peak hours is between 5AM-10AM & 5PM-10PM",x= "Cab Request in a day(hrs)")

#CANCELLATION AND NON CAR AVAILABILITY ARE HIGH DURING PEAK HOURS , DUE TO WHICH UBER IS LOOSING BUSINESS .

req_status <- ggplot(data=uber_ds,aes(x=uber_ds$req_hr,fill=uber_ds$Status))+geom_bar()+labs(title = " Cab Request Pattern during the day", subtitle = "5AM-10AM high cancellation & 5PM-10PM High Cab Unavailability",x="Cab Request in a day(hrs)")
req_status




#CAB Request Pattern by pickup_point ------------------------------------------------------

req_pickup <-ggplot(data=uber_ds,aes(x=uber_ds$req_hr,fill=uber_ds$Pickup.point))+geom_bar()+labs(title = " Cab Request Pattern during the day", subtitle = "5AM-10AM City to airport Req & 5PM-10PM Airport to city Req",x="Cab Request in a day(hrs)")
req_pickup


#Plotting both the Graphs

grid.arrange(req_status,req_pickup,ncol=2)

#------------------------Analysis From Plot  ------------------------------------------

#During morning peak hours city to Airport Req are more .
#Durig evening peak hours Airport to city Req are more .
#Trip Cancellation is more during the morning peak hours and no cab availaibility is more during
#Evening peak hours .

#THIS STEP IS TO DETERMINE THE POSSIBLE REASON FOR CANCELLATION OF MORNING REQUEST OF UBER -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Determining how the journey time is affecting the uber services .

ggplot(data=uber_ds,aes(x=uber_ds$req_hr,y=uber_ds$journey_mins,fill=uber_ds$Pickup.point))+geom_boxplot()+labs(title = "Journey pattern in the day(hrs)",subtitle = "Journey time is higher in morning peak hours from city to Airport compare to Other trip ",x="Cab Req in a day(hrs)")



#-----------------------DEMAND & SUPPY CASE-----------------------------------

#1. Demand = number of cab requests done.  
#Trip completed + Trip cancelled + Cabs unavailable
#2. Supply = Number of cab availability. Only trip completed is considered as supply.

#------------------------------------------------------------------------------

#Finding The demand Supply gap for the uber dataset.

#-------------------------Cab DEMAND Entire Day --------------------------------------------------------

aggregate(Request.id~Pickup.point,uber_ds,length)

#Finding out out the many requests , how many trips completed and how many request did not cater customer need .

complete_demand <- aggregate(Request.id~Pickup.point+Status,uber_ds,length)
names(complete_demand) = c("Pickup.point","Status", "complete_cab_demand")
complete_demand



# cab supply is taken from uber_date (has trip completed data) data frame where 
#only trip completed data available
# obtaining only trip completed data from uber_data to find supply----------------------------

trips_completed_entireday = 
  uber_ds %>% filter(Status == "Trip Completed")

#View(trips_completed_entireday)---------------------------------------------------

complete_supply = aggregate(Driver.id~Pickup.point + Status,trips_completed_entireday,length)
names(complete_supply) = c("Pickup.point","Status","complete_supply")
complete_supply

complete_supply_demand= merge(complete_demand,complete_supply,all.x = TRUE)
complete_supply_demand

complete_supply_demand$complete_supply[which(is.na(complete_supply_demand$complete_supply))] =0
complete_supply_demand 


#Plotting the Trip Completed Graph ------------------------------------------------


complete_demand_plot <-ggplot(data=complete_supply_demand,
                                     aes(x=Pickup.point,y=complete_cab_demand,fill=Status))+geom_bar(stat = "identity",position = "stack")+
  labs(title = "Cab Demand Patern for entire day by Pickup",subtitle = "Summary : Cab Demand is All the Request in Entire Day",x="Pickup point",y="No. of cab request entire day")




complete_supply_plot = ggplot(data = complete_supply_demand, 
                                 aes(x = Pickup.point, y = complete_supply, fill = Status )) + geom_bar(stat = "identity")+labs(title = "Cab Supply for entire from Pickup Points", 
       subtitle ="Summary: Entire day Cab supply is based on Pickup Point and trips completed",
       caption ="source: uber_data data frame subsetted for Trip completed" ,
       x = "Pickup.point", y = "overall cab supply (No.of cab reqsts for entire day") 


grid.arrange(complete_demand_plot,complete_supply_plot, ncol =2)

#----------------------Cab DEMAND during peak hours-------------------------------------------------------

#Peak hours is considered from 5AM-10AM and 5PM-10PM and it has been filtered from the main data "uber_ds"

peakhr_data <- uber_ds %>% filter(uber_ds$req_hr %in% c("05","06","07","08","09","10"
                                                        ,"17","18","19","20","21","22"))

#Finding the Cab Demand during peakhrs----------------------------------------------- 

aggregate(Request.id~Pickup.point,peakhr_data,length)

#Finding out out the many requests , how many trips completed and how many request did not cater customer need .

peakhr_demand <- aggregate(Request.id~Pickup.point+Status,peakhr_data,length)
names(peakhr_demand)=c("Pickup.point","Status", "peakhr_demand")
peakhr_demand

# Cab supply  during peak hours

# Cab supply is taken from driver_trips (has trip completed data) data frame where 
#only trip completed data available
# Filtering only peak hours data from driver_trip to find supply

driver_trips = uber_ds %>% filter(!is.na(Driver.id) & Status =="Trip Completed") %>% 
  group_by(Driver.id)

trips_completed_peakhr = 
  driver_trips %>% 
  filter(req_hr %in% c("05","06","07","08","09","10",
                            "17","18","19","20","21","22"))
#View(trips_completed_peakhr)

aggregate(Driver.id~Pickup.point,trips_completed_peakhr,length)

peakhr_supply = aggregate(Driver.id~Pickup.point + Status,trips_completed_peakhr,length)
names(peakhr_supply) = c("Pickup.point","Status","peakhr_supply")
peakhr_supply

peakhr_supply_demand= merge(peakhr_demand,peakhr_supply,all.x = TRUE)

peakhr_supply_demand$peakhr_supply[which(is.na(peakhr_supply_demand$peakhr_supply))] =0
peakhr_supply_demand 

#Plotting the supply demand gap plot during peak hrs ---------------------------------------
peakhr_demand_plot <-
ggplot(data=peakhr_supply_demand,aes(x=Pickup.point,y=peakhr_demand,fill=Status))+
  geom_bar(stat = "identity",position = "stack")+theme(title = element_text(size=9, face="bold"))+labs(title = "Cab Demand Patern in peak hrs by Pickup",
                                                                                                       subtitle ="Summary: Cab Demand during peak hours based on Pickup Point and trips completed",x="Pickup point",y="No. of cab request entire day")

peakhr_supply_plot <- ggplot(data = peakhr_supply_demand, 
                                aes(x = Pickup.point, y = peakhr_supply, fill = Status )) + 
  geom_bar(stat = "identity") +
  theme(title = element_text(size=9, face="bold"))+
  labs(title = "Cab Supply from Pickup Points during Peak hours", 
       subtitle ="Summary: Cab supply during peak hours based on Pickup Point and trips completed",
       caption ="source: driver_trips data frame subsetted for peak hours" ,
       x = "Pickup.point", y = "cab supply (no of cab reqsts)") 

#---------------------Plotting the 2 graphs-----------------------

grid.arrange(peakhr_demand_plot, peakhr_supply_plot, ncol =2)

#---------------------------Analysis from Plot -----------------------------------

#Airport Pickups : Out of 3238 Request only 1327 Trips were Completed . Hence Supply ,Demand gap exists in this trip.
#City Pickups : Out of 3507 Request only 1507 Trips were completed . Hence Demand Supply gap exists here as well .

#And during peak hours out of 2484 requests of Airport to city trip , only 888 was completed .
#which is a matter of concern for Uber Sevices .
#Therefore ,This gap is most severe in the identified time slots.

#------------------------------------------------------------------------------------

#############......Plot: CANCELLATION......###########################

# Testing hypothesis,  the cab cancellation pattern during morning peak hours holds good by
#analyzing per hour data during all days.

# filtering Status "Trip Cancelled" rows from uber_data to further analysis

cancelled_trip <- uber_ds %>% filter(Status == "Cancelled") %>% group_by(Driver.id)


#View(cancelled_trip)------------------------------------------


supplygap_notrips_plot <-
  ggplot(data <- cancelled_trip, mapping = aes(x = Pickup.point, fill = is_peakhrs)) + 
  geom_bar() + theme(title = element_text(size=9, face="bold")) +
  labs(title = "Trip Cancellation duirng the Day from Pickup point",
       subtitle ="Summary: Trip Cancellations are high from city during peak morning hours (5-10AM)",
       x = "Trip Cancellations Pickup Point ",y ="count of Cab Request")


supplygap_days_notrips_plot <-
 ggplot(data = cancelled_trip, mapping = aes(x = req_hr, fill = is_peakhrs)) + 
  geom_bar()+facet_wrap(~Pickup.point) + theme(title = element_text(size=9, face="bold")) +
  labs(title = "Trip Cancellation duirng the Day from Pickup point",
       subtitle ="Summary: Trip Cancellations are high from city during peak morning hours (5-10AM)",
       x = "Trip Cancellations Pickup Point ",y ="count of Cab Request")

grid.arrange(supplygap_notrips_plot,supplygap_days_notrips_plot,ncol=2)

# clearly cancellations are high in the peak hours ( 5-10AM) morning time and high from city




#--------------------------------------------------------------------------------------------------

#############......Plot: NO CABS AVAIALBLE......###########################


# hypothesis testing,  the cab unavailability pattern during evening peak hours holds good 
# by analyzing per hour data during all days.

# ASSUMPTIONS
#Cab unavailable, When customer wants to book a trip but cab is unavailable. So Driver ID and
#Drop Time stamp data is unavailable for the analysis
#From the given Uber data set, we extracted data/rows related NO CAB AVAILABLE status only 
#and analyzed patterns on this data. Hence, data considered for plots in this slide consists
#of 'NO CAB AVAILABLE' data only

# filtering Status "No Cars Available" rows from uber_data to further analysis 

cabs_unavailable <- uber_ds %>% filter(Status == "No Cars Available") 


#View(cabs_unavailable)

supplygap_nocabs_plot <- 
  ggplot(data = cabs_unavailable, mapping = aes(x = Pickup.point, fill = is_peakhrs)) + 
  geom_bar() + theme(title = element_text(size=9, face="bold")) +
  labs(title = "Cab Unavailability during Peak hours To/from City/Airport", 
       subtitle ="Summary: Cab Unavailability is high during peak evening hours from Airport",
       x = "Pickup Point Cab Unavailability", y = "No.of Cab Requests (count)")


supplygap_days_nocabs_plot<-
  ggplot(data = cabs_unavailable, mapping = aes(x =req_hr, fill = is_peakhrs)) + 
  geom_bar() + facet_wrap(~Pickup.point) + 
  theme(title = element_text(size=9, face="bold"))+
  labs(title = "Cab unavaialble during various days To/from City/Airport", 
       subtitle ="Summary: Cab Unavailability persists through all days in peak evening hours from Airport",
       x = "Pickup Point Cab Unavailability", y = "No.of Cab Requests (count)")


grid.arrange(supplygap_nocabs_plot,supplygap_days_nocabs_plot,ncol=2)

#-----------------------END OF ANALYSIS-------------------------------------------------
