#BUS462D100 - Final Project - IMDB Movie Review Sentiment Analysis 
#We pledge on our honor that we have not given or received any unauthorized assistance on this assignment. 

cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all data objects
gc() # clear memory
set.seed(42)


#install the necessary packages
install.packages((c("tm", "tidytext", "dplyr", "readr", "caret")))

#load the necessary packages                  
library(readr) 
library(dplyr) 
library(tm)
library(tidytext)
library(caret)

#function to read the text files
read_files_and_label <- function(directory, sentiment) {
  file_paths <- list.files(directory, pattern = "*.txt", full.names = TRUE)
  reviews <- lapply(file_paths, read_file)
  return(data.frame(text = unlist(reviews), sentiment = sentiment))} 

# Paths to the directories
train_pos_path <- "/Users/chase/Downloads/aclImdb/train/pos"
train_neg_path <- "/Users/chase/Downloads/aclImdb/train/neg"
test_pos_path <- "/Users/chase/Downloads/aclImdb/test/pos"
test_neg_path <- "/Users/chase/Downloads/aclImdb/test/neg"

# Read the files and label them
train_pos_reviews <- read_files_and_label(train_pos_path, "positive")
train_neg_reviews <- read_files_and_label(train_neg_path, "negative")
test_pos_reviews <- read_files_and_label(test_pos_path, "positive")
test_neg_reviews <- read_files_and_label(test_neg_path, "negative")


# Combine the data and codify the sentiment as a binary variable
train_data <- rbind(train_pos_reviews, train_neg_reviews, test_pos_reviews, test_neg_reviews)  
train_data$sentiment <- as.factor(ifelse(train_data$sentiment == "positive", 1, 0))

#view the data to see if it was read correctly 
head(train_data) 

#preprocess the data
# Create a corpus from the vector of reviews
docs <- Corpus(VectorSource(train_data$text))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace) 

# Create a document-term matrix 
dtm <- DocumentTermMatrix(docs)
dtm <- removeSparseTerms(dtm, 0.99)

#view the document term matrix 
inspect(dtm)   

#convert the document term matrix to a matrix 
train_data_matrix <- as.matrix(dtm) 

#QUESTION for CK - This matrix contains a very large amount of elements, do we need to do more precprocessing to reduce the size of the matrix? 
#stack overflow recommends either a) more stopwords, b)TD-IDF Weighting or C) Dimensionality reduction? 

#add a column to the data to capture the length of the review 
train_data$review_length <- nchar(train_data$text)  

#shuffle the data 
train_data <- train_data[sample(nrow(train_data)), ] 

#split the data into training and testing sets using the caret package 
set.seed(42) 
trainIndex <- createDataPartition(train_data$sentiment, p = .8, list = FALSE, times = 1) 
train_data <- train_data[trainIndex, ]
test_data <- train_data[-trainIndex, ]


#exploratory data analysis

#plot the distribution of review lengths using ggplot2 

library(ggplot2) 
ggplot(train_data, aes(x = review_length)) + 
  geom_histogram(binwidth = 100) + 
  labs(title = "Distribution of Review Lengths", x = "Review Length", y = "Frequency") 

#plot the distribution of sentiment using ggplot2 
ggplot(train_data, aes(x = sentiment)) + 
  geom_bar() + 
  labs(title = "Distribution of Sentiment", x = "Sentiment", y = "Frequency") 

#plot the distribution of sentiment by review length using ggplot2 
ggplot(train_data, aes(x = review_length, fill = sentiment)) + 
  geom_histogram(binwidth = 100, position = "dodge") + 
  labs(title = "Distribution of Sentiment by Review Length", x = "Review Length", y = "Frequency") 





#hypothesis 1: Can we predict the sentiment of a movie review (positive or negative) based on the frequency of words used in the text?

#starting with logistic regression 
