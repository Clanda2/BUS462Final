
# BUS 462 -- Final Project 

######################################################### 

cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all data objects
gc() # clear memory
set.seed(42) 

#installing any missing packages

install.packages("googledrive")
install.packages("corrplot")

#loading the data and packages

library(dplyr)
library(data.table)
library(ggplot2) 
library(tidyr)
library(googledrive) 
library(corrplot) 
library(scales)

drive_auth() #connecting to Google Drive API for data download
file_id <- "https://drive.google.com/file/d/1LtLVMOV2yBkhXo-DrvXS3E5O_U_l1M0K/view?usp=drive_link"
downloaded_file <- drive_download(as_id(file_id),overwrite = TRUE) 
movies <- read.csv(downloaded_file$name, header = TRUE)
rm(downloaded_file, file_id)                        

names(movies)

####### DATA CLEANING ######## 

summary(movies) 
str(movies) 

#check each column for missing values and sort by the number of missing values 
missing_values <- sapply(movies, function(x) sum(is.na(x) | x == ""))
missing_values <- missing_values[order(missing_values, decreasing = TRUE)]
missing_values 

#drop the critic consensus column 
movies_cleaned <- movies %>% select(-critics_consensus) #many missing values in this column so dropping it 
names(movies_cleaned) 

#counting and removing rows with any missing values 
movies_with_na <- movies_cleaned
movies_with_na[movies_with_na == ""] <- NA
num_rows_with_missing_or_blank <- sum(!complete.cases(movies_with_na)) # Count the number of rows with a missing value (including blank strings) in any column
num_rows_with_missing_or_blank # Print the number of rows with any missing value or blank strings
rm(movies_with_na) # Remove the temporary data frame (movies_with_na)
rm(missing_values, num_rows_with_missing_or_blank)

#remove rows with missing values 
movies_cleaned[movies_cleaned == ""] <- NA 
movies_cleaned <- na.omit(movies_cleaned) # Remove rows with missing values

#drop the columns that are not needed for our analysis 
movies_cleaned <- movies_cleaned %>% select(-rotten_tomatoes_link) # Drop the rotten_tomatoes_link column 
movies_cleaned <- movies_cleaned %>% select(-audience_status) # Drop the audience_status column

#converting the data types of the columns 
movies_cleaned$runtime <- as.numeric(movies_cleaned$runtime)
movies_cleaned$audience_rating <- as.numeric(movies_cleaned$audience_rating)
movies_cleaned$audience_count <- as.numeric(movies_cleaned$audience_count)
movies_cleaned$original_release_date <- as.Date(movies_cleaned$original_release_date, format = "%Y-%m-%d")
movies_cleaned$streaming_release_date <- as.Date(movies_cleaned$streaming_release_date, format = "%Y-%m-%d")
movies_cleaned$genres <- as.factor(movies_cleaned$genres)
movies$tomatometer_status <- factor(movies$tomatometer_status, levels = c("Rotten", "Fresh", "Certified-Fresh")) #convert tomatometer_status to factor with levels for regression analysis

#check zeros in the numeric columns 
zero_values <- sapply(movies_cleaned, function(x) sum(x == 0))
zero_values <- zero_values[order(zero_values, decreasing = TRUE)]
zero_values # file contains zeros in some rating columns but these make sense in this context so leaving them as is 
rm(zero_values)

#check for duplicates movie titles 
duplicates <- movies_cleaned %>% group_by(movie_title) %>% summarise(n = n()) %>% filter(n > 1)
duplicates #405 duplicates found, will remove them 
movies_cleaned <- movies_cleaned[!duplicated(movies_cleaned$movie_title),] #remove duplicates
rm(duplicates) #remove duplicates object


#check for outliers in runtime  
ggplot(movies_cleaned, aes(x = "", y = runtime)) + 
  geom_boxplot(fill = "tomato", color = "navy") +
  labs(title = "Distribution of Movie Run Times",
       x = "", 
       y = "Runtime (minutes)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title.x = element_blank(),  # Remove x axis label
        axis.ticks.x = element_blank(),  # Remove x axis ticks
        axis.text.x = element_blank())  # Remove x axis text

#data set has a number of outliers in runtime, but these are valid values so will not remove them 

#create a long format data set to seperate each comma seperated genre into its own column to check balance 

movies_genre_long <- movies_cleaned %>%
  separate_rows(genres, sep = ",\\s*") %>%
  mutate(genres = trimws(genres))

#check the balance of the long format data set 

genre_balance <- movies_genre_long %>% group_by(genres) %>% summarise(n = n()) %>% arrange(desc(n))

#plot the infrequent genres 
ggplot(genre_balance, aes(x = reorder(genres, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Infrequent Genres",
       x = "Count of Movies",
       y = "Genre") +
  theme_minimal() 

## Plot shows that there are some infrequent genres, think we would need to remove these from the cleaned_data somehow 

#add a column that calculates the age of the movie at the time of streaming release 
movies_cleaned$age_at_streaming <- as.numeric(difftime(movies_cleaned$streaming_release_date, 
                                                       movies_cleaned$original_release_date, 
                                                       units = "days"))

#check for outliers in age at streaming 
ggplot(movies_cleaned, aes(x = "", y = age_at_streaming)) + 
  geom_boxplot(fill = "tomato", color = "navy") +
  labs(title = "Distribution of Age of Movies at Streaming Release",
       x = "", 
       y = "Age at Streaming Release (days)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title.x = element_blank(),  # Remove x axis label
        axis.ticks.x = element_blank(),  # Remove x axis ticks
        axis.text.x = element_blank())  # Remove x axis text 

#data set has a number of outliers in age at streaming, but these are valid values so will not remove them  


#check for distribution of age at streaming 
ggplot(movies_cleaned, aes(x = age_at_streaming)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 30) +
  labs(title = "Distribution of Age of Movies at Streaming Release",
       x = "Age at Streaming Release (days)", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

### this has a number of zeros indicating the movies were released direct to streaming, might considering subsetting into movies 
### released in theaters first and those that were not 

#data is right skewed, but this is expected so will not transform the data  (might be needed confirm) 

#add a column for the number of actors in the cast 
movies_cleaned$num_actors <- sapply(movies_cleaned$actors, function(x) length(unlist(strsplit(x, ",\\s*")))) 

#check for outliers in the number of actors
ggplot(movies_cleaned, aes(x = "", y = num_actors)) + 
  geom_boxplot(fill = "tomato", color = "navy") +
  labs(title = "Distribution of Number of Actors in the Cast",
       x = "", 
       y = "Number of Actors") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title.x = element_blank(),  # Remove x axis label
        axis.ticks.x = element_blank(),  # Remove x axis ticks
        axis.text.x = element_blank())  # Remove x axis text

#one extreme outlier identified so will need to remove, run the regession twice to see the impact or do a log transformation???? 


#check for distribution of number of actors
ggplot(movies_cleaned, aes(x = num_actors)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 30) +
  labs(title = "Distribution of Number of Actors in the Cast",
       x = "Number of Actors", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title


##extremely right skewed might consider a log transformation???? 

#add a column for the number of authors 

movies_cleaned$num_authors <- sapply(movies_cleaned$authors, function(x) length(unlist(strsplit(x, ",\\s*"))))

#check for outliers in the number of authors

ggplot(movies_cleaned, aes(x = "", y = num_authors)) + 
  geom_boxplot(fill = "tomato", color = "navy") +
  labs(title = "Distribution of Number of Authors",
       x = "", 
       y = "Number of Authors") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title.x = element_blank(),  # Remove x axis label
        axis.ticks.x = element_blank(),  # Remove x axis ticks
        axis.text.x = element_blank())  # Remove x axis text

## same here has outliers might consider a transformation or running the regression twice if including  

#add a column for the number of directors 

movies_cleaned$num_directors <- sapply(movies_cleaned$directors, function(x) length(unlist(strsplit(x, ",\\s*"))))

#check for outliers in the number of directors

ggplot(movies_cleaned, aes(x = "", y = num_directors)) + 
  geom_boxplot(fill = "tomato", color = "navy") +
  labs(title = "Distribution of Number of Directors",
       x = "", 
       y = "Number of Directors") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title.x = element_blank(),  # Remove x axis label
        axis.ticks.x = element_blank(),  # Remove x axis ticks
        axis.text.x = element_blank())  # Remove x axis text

#same with this one, number of outtliers so might consider a transformation or running the regression twice if including 


####### EXPLORATORY ANALYSIS ######## 


####### HYPOTHESIS TESTING AND MODELS ######## 


#Hypothesis 1: 


