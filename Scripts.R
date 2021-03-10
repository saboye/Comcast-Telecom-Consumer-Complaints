# Author: SAMUEL ABOYE
# Title: Comcast Telecom Consumer Complaints.
# Date: Feb 2021


# Step 1: Load the necessary packages

library(dplyr)     # data frame manipulation in an intuitive
library(ggplot2)   # data visualization package
library(stringi)   # text/natural language processing
library(lubridate) # Date-time data
library(ggcal)     # calendar plot
library(tidyverse) # opinionated collection of R packages



# Step 2: load the data set
data <- read.csv("dataset_comcast/Comcast_Telecom_Complaints_data.csv")

# check the sample data set 
head(data,5)

# Review the structure of the data set 
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


# Converting All String Values to Lower, so as to Eliminate Duplication of Any Complaint
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

##############################################################################################
# plot the world cloud for the most freqeustly used words 
wordcloud(words = Most.Frequency$Complaint.Types, freq = Most.Frequency$Frequency, min.freq = 2,
          max.words=200, random.order=TRUE, rot.per=0.45, 
          colors=brewer.pal(12, "Dark2"))



# find the frequecney of the words 
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

# Pie chart to show the number of cases as per the finding of the keywords 
number.Of.ticket <- c(795, 489, 379, 150, 192)
lbls <- c("other", "Internet", "data", "billing","data Caps", "Speed")
pie(number.Of.ticket, labels = lbls,col=rainbow(length(lbls)), main="Number of issue Per case")




#################################################################################################
#################################### Topic Modeling #############################################

# In this section  use a Topic modeling in the text-mining tool for discovery of hidden semantic
# structures in a text body, in the above part I used a hard coded  method to find the frequency 
# of the words that exist in the text. 

library(tm)
library(readr)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(topicmodels)
library(tidytext)

require(Snowballc)


# load the data set for the topic modeling again 
data2 <- read.csv("dataset_comcast/Comcast_Telecom_Complaints_data.csv")

text <- data2$Customer.Complaint
View(text)

text <- as.character(text)
sample <- sample(text, (length(text)))
corpus <- Corpus(VectorSource(list(sample)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),'comcast'))

#document-term matrix frequency of terms that occur in a collection of documents
dtm <-  DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

dtm


# Latent Dirichlet allocation (LDA) method for fitting a topic model
lda <- LDA(dtm, k=4, control = list(seed=500))

# LDA model
lda     

# Top Model
topics <- tidy(lda, matrix= "beta")

# different topic 
topics 


# filtering the topics using group by 
topic_term <- topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>%
  arrange(topic, -beta)

View(topic_term)


# plot the four main topics 
topic_term %>% 
  mutate(term= reorder(term,beta)) %>% 
  ggplot(aes(term, beta, fill= factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap( ~ topic, scales = "free") +
  coord_flip()


##############################################################################################
open <- ( data$Status   == "Open"| 
            data$Status =="Pending") 

closed <- (data$Status   == "Closed"| 
             data$Status =="Solved")

data$ComplaintStatus[open]  <-"Open" 
data$ComplaintStatus[closed]<- "Closed"

comcast_data <- group_by(data,State,ComplaintStatus) 
status_data<- dplyr::summarise(comcast_data,Count = n())

View(chart_data)


#write.csv(chart_data,"name.csv", row.names = FALSE)

# plot the Ticket Status Distribution per States chart 
p1 <- ggplot(status_data, aes(x =status_data$State, y = status_data$Count)) +
  geom_bar(stat="identity", color="#FF9832",fill="#000000")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#FF9832"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Total Number of Ticket Status Distribution per States ",
       x = "States",y = "Tickets", fill= "Status")      
# display the Ticket Status Distribution per States chart  
p1

# plot the stack graph 

p2 <- ggplot(status_data, aes(x =status_data$State, y = status_data$Count))+
  geom_col(aes(fill = status_data$ComplaintStatus),  width=.85)+
  geom_text(aes(y = status_data$Count,
                label=status_data$Count,
                group=status_data$ComplaintStatus))+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#3299FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Distribution per States ",
       x = "States",y = "Tickets",fill= "Status")
# display the Ticket Status Distribution per States stack bar chart   

p2 + scale_fill_manual(values = c("#3299FF", "#FF9832"))


# To count the total Number of the Internet tickets

internet <-data %>%
  filter(grepl('internet', data$Received.Via,ignore.case = TRUE)) %>%
  roup_by(Status)
nrow()
View(internet)


# To count the total Number of the customer care tickets
Customer.Care.Call <-data %>%
  filter(grepl('Customer Care Call', data$Received.Via,ignore.case = TRUE)) %>%
  nrow()
Customer.Care.Call


# new dataframe for the agrgated value of the open an closed tickets 
data1 <- table(data$Received.Via, data$ComplaintStatus)
data1 <- cbind(data1, Total = rowSums(df1))
View(data1)


# Pie Chart with Percentages
open <- sum( 255, 262)
closed <- sum(864,843)
number.Of.tickets <- c(517, 1707)
pct <- round(number.Of.tickets/sum(number.Of.tickets)*100)
lable <- c("Open", "Closed")
lable <- paste(lable, pct)
lable <- paste(lable,"%",sep="")
pie(number.Of.tickets, labels = lable,col=rainbow(length(lable)), main="Percentage of Open and Closed Tickets")

