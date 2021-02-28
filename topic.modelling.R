# Author: SAMUEL ABOYE
# Title: Comcast Telecom Consumer Complaints.
# Date: Feb 2021

library(tm)
library(ggplot2)
library(readr)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(topicmodels)

require(Snowballc)


# Step 2: load the dataset
data <- read.csv("dataset_comcast/Comcast_Telecom_Complaints_data.csv")

text <- data$Customer.Complaint
View(text)

text <- as.character(text)
sample <- sample(text, (length(text)))
corpus <- Corpus(VectorSource(list(sample)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),'comcast'))

dtm <-  DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
dtm

lda <- LDA(dtm, k=4, control = list(seed=500))
lda     

library(tidytext)
topics <- tidy(lda, matrix= "beta")

topics 
library(dplyr)

topic_term <- topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>%
  arrange(topic, -beta)

view(topic)

topic_term %>% 
  mutate(term= reorder(term,beta)) %>% 
  ggplot(aes(term, beta, fill= factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap( ~ topic, scales = "free") +
  coord_flip()





