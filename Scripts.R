# Author: SAMUEL ABOYE
# Title: Comcast Telecom Consumer Complaints.
# Date: Feb 2021


# Step 1: Load the necessary packages
library(dplyr)
library(ggplot2)
library(stringi) # text/natural language processing
library(lubridate) # Date-time data
library(ggcal)  # calendar plot
library(tidyverse)

# Step 2: load the dataset
data <- read.csv("dataset_comcast/Comcast_Telecom_Complaints_data.csv")

# check the sample dataset 
head(data,5)

# Review the structure of the dataset 
str(data)

# Check if there is any missing data point 
any(is.na(data))

# parsing the date to one format 
data$Date<- dmy(data$Date)

# Filter for observations daily
daily<- dplyr::summarise(group_by(data,Date),Count =n())
View(daily)

# Filter for observations monthly 
monthly<- dplyr::summarise(group_by(data,Month =as.integer(month(Date))),Count = n())
monthly.total<-arrange(monthly,Month)
View(monthly)

# renaming the months to factor 
month.name <-  c("Jan","Feb","Mar",
                 "Apr","May","Jun",
                 "Jul","Aug","Sep",
                 "Oct","Nov","Dec")
monthly$Month <- month.name[monthly$Month]
monthly$Month <- as.(monthly$Month)
monthly$Month <- factor(monthly$Month , levels = monthly$Month )
#  display the monthly complaints 
View(monthly)

# plotting the number of complaints per day
ggplot(daily, aes(daily$Date, daily$Count, group = 1)) + 
  geom_point(color='red') + 
  geom_line(color='green') + 
  xlab("Dates") + 
  ylab("Number of Complaints") +
  ggtitle("Number of complaints per Day")+
  theme(plot.title = element_text(hjust = 0.5))

# plotting the number of complaints per Month
ggplot(monthly, aes(monthly$Month, monthly$Count, group = 1)) + 
  geom_point(color='red') + 
  geom_line(color='green') + 
  xlab("Months") + 
  ylab("Number of Complaints") +
  ggtitle("Number of complaints per Month")+
  theme(plot.title = element_text(hjust = 0.5))

# plotting the number of complaints per Day  per Month
gg <- ggcal(daily$Date, daily$Count) +
  scale_fill_gradient2(low="#6f696c",
                       mid="#58905a", 
                       high="#385a58",
                       midpoint=0,
                       na.value="#829e5e") +  
  ggtitle("Number of complaints per Day/Months")+
  theme(plot.title = element_text(hjust = 0.5))
# printing Calender                 
print(gg)

library(stringi)

library(dplyr)
#Converting All String Values to Lower, so as to Eliminate Duplication of Any Complaint
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


library(stringi)

library(dplyr)
#Converting All String Values to Lower, so as to Eliminate Duplication of Any Complaint
data.temp<-data%>% 
  mutate(tolower(Customer.Complaint))
Complaint.Type <- table(data.temp$Customer.Complaint)
Complaint.Type <- data.frame(Complaint.Type)
filtered <- Complaint.Type %>% 
  rename(
    Complaint.Types = Var1,
    Frequency = Freq
  )
Most.Frequency <- filtered %>% 
  arrange(desc(Frequency))

Most.Frequency<-head(final,10)
Most.Frequency



wordcloud(words = Most.Frequency$Complaint.Types, freq = Most.Frequency$Frequency, min.freq = 2,
         max.words=200, random.order=TRUE, rot.per=0.45, 
         colors=brewer.pal(12, "Dark2"))




internet_tickets <-data %>%
  filter(grepl('internet', data$Customer.Complaint,ignore.case = TRUE)) %>%
  nrow()

data_tickets <- data %>%
  filter(grepl('data', data$Customer.Complaint, ignore.case = TRUE))%>%
  nrow()

billing_tickets <- data %>%
  filter(grepl('bill', data$Customer.Complaint, ignore.case = TRUE))%>%
  nrow()

data_cap_tickets <- data %>%
  filter(grepl('data cap', data$Customer.Complaint, ignore.case = TRUE))%>%
  nrow()

speed_tickets <- data %>%
  filter(grepl('bill', data$Customer.Complaint,ignore.case = TRUE))%>%
  nrow()

others_ticket <- length(data$Customer.Complaint) -sum(c(service_tickets,
                                                        data_tickets,
                                                        billing_tickets,
                                                        data_cap_tickets,
                                                        speed_tickets))

temp  <- c(others_ticket,service_tickets,data_tickets,billing_tickets,data_cap_tickets, speed_tickets)
sum(temp)

number.Of.ticket <- c(795, 489, 379, 150, 192)
lbls <- c("other", "Internet", "data", "billing","data Caps", "Speed")
pie(number.Of.ticket, labels = lbls,col=rainbow(length(lbls)), main="Number of issue Per case")

##############################################################################################
open <- ( data$Status == "Open"| 
          data$Status =="Pending")

closed <- (data$Status == "Closed"| 
           data$Status =="Solved")

data$ComplaintStatus[open]<-"Open" 
data$ComplaintStatus[closed]<- "Closed"

comcast_data<- group_by(data,State,ComplaintStatus)
chart_data<- summarise(data,Count = n())


####################################  Map ####################################################

