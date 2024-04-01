
# BUS 462 -- Final Project 

######################################################### 

cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all data objects
gc() # clear memory
set.seed(42) 

#installing any missing packages

install.packages("googledrive")
install.packages("corrplot") 
install.packages("fastDummies")
install.packages("httr")
install.packages("timeDate") 
install.packages("car")
install.packages("glmnet") 
install.packages("lmtest") 
install.packages("sandwich")
install.packages("rpart") 
install.packages("rpart.plot")
install.packages("pscl")

#loading the data and packages

library(dplyr)
library(data.table)
library(ggplot2) 
library(tidyr)
library(googledrive) 
library(corrplot) 
library(scales) 
library(stargazer)
library(fastDummies)
library(timeDate)
library(car)
library(glmnet)
library(caret)
library(rpart)
library(rpart.plot)
library(pscl)

drive_auth() #connecting to Google Drive API for data download
file_id <- "https://drive.google.com/file/d/1LtLVMOV2yBkhXo-DrvXS3E5O_U_l1M0K/view?usp=drive_link" #remove this before pushing to GitHub 
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
movies_cleaned <- movies_cleaned %>% 
  filter(content_rating != "NC17") #Remove NC17 ratings as these are related to TV shows and not movies 
movies_cleaned <- movies_cleaned %>% select(-movie_info) # Drop the movie_info column
movies_cleaned <- movies_cleaned %>% select(-production_company)
movies_cleaned <- movies_cleaned %>% select(-tomatometer_top_critics_count) # Drop the tomatometer_top_critics_count column
movies_cleaned <- movies_cleaned %>% select(-tomatometer_rotten_critics_count) # Drop the tomatometer_rotten_critics_count column 
movies_cleaned <- movies_cleaned %>% select(-tomatometer_fresh_critics_count)
movies_cleaned <- movies_cleaned %>% select(-tomatometer_status)
movies_cleaned <- movies_cleaned %>% select (-tomatometer_count)
names(movies_cleaned)


movies_cleaned$runtime <- as.numeric(movies_cleaned$runtime)
movies_cleaned$audience_rating <- as.numeric(movies_cleaned$audience_rating)
movies_cleaned$audience_count <- as.numeric(movies_cleaned$audience_count)
movies_cleaned$original_release_date <- as.Date(movies_cleaned$original_release_date, format = "%Y-%m-%d")
movies_cleaned$streaming_release_date <- as.Date(movies_cleaned$streaming_release_date, format = "%Y-%m-%d")
movies_cleaned$genres <- as.factor(movies_cleaned$genres)
movies_cleaned$tomatometer_rating <- as.numeric(movies_cleaned$tomatometer_rating) #convert to numeric for regression analysis
movies_cleaned$content_rating <- factor(movies_cleaned$content_rating, levels = c("PG", "NR", "PG-13", "G", "R")) 

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

#data is right skewed, might consider a log transformation for the regression analysis 

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

#one extreme outlier identified, this is a valid entry but wiill skew the data so will remove it 

movies_cleaned <- movies_cleaned %>% filter(num_actors < 300) #remove the extreme outlier


#check for distribution of number of actors
ggplot(movies_cleaned, aes(x = num_actors)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 30) +
  labs(title = "Distribution of Number of Actors in the Cast",
       x = "Number of Actors", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title


#data is extremely right skewed, may consider a log transformation for the regression analysis 

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

#two extreme outliers identified, so we will remove them 

movies_cleaned <- movies_cleaned %>% filter(num_authors < 21) #remove the extreme outliers

#plot the distribution of the number of authors 
ggplot(movies_cleaned, aes(x = num_authors)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 30) +
  labs(title = "Distribution of Number of Authors",
       x = "Number of Authors", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

#same problem with skewness, may consider a log transformation for the regression analysis 

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


#create a long format data set to seperate each comma seperated genre into its own column to check balance 

movies_genre_long <- movies_cleaned %>%
  separate_rows(genres, sep = ",\\s*") %>%
  mutate(genres = trimws(genres))

#one-hot encode the genres column to create dummy variables
movies_genres_encoded <- fastDummies::dummy_cols(movies_genre_long, select_columns = "genres", remove_selected_columns = TRUE)

#convert back to wide format
movies_genres_encoded <- movies_genres_encoded %>% group_by(movie_title) %>% summarise(across(starts_with("genres"), sum))

#count the number of occurrences of each genre
genre_occurrences <- colSums(movies_genres_encoded[, -which(names(movies_genres_encoded) %in% c("movie_title", "other_non_genre_columns"))])
genre_occurrences_df <- data.frame(Genre = names(genre_occurrences), Occurrences = genre_occurrences, row.names = NULL) #converting the list to a data frame 
genre_occurrences_df <- genre_occurrences_df[order(-genre_occurrences_df$Occurrences),] #sort by occurrences
print(genre_occurrences_df) 

#plot the count 
ggplot(genre_occurrences_df, aes(x = reorder(Genre, Occurrences), y = Occurrences)) + 
  geom_bar(stat = "identity", fill = "tomato", color = "navy") +
  labs(title = "Number of Movies by Genre",
       x = "Genre", 
       y = "Number of Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x axis labels

#check for balance in the genres
genre_balance <- genre_occurrences_df$Occurrences / nrow(movies_cleaned) * 100
genre_balance_df <- data.frame(Genre = genre_occurrences_df$Genre, Balance = genre_balance, row.names = NULL) #converting the list to a data frame
genre_balance_df <- genre_balance_df[order(-genre_balance_df$Balance),] #sort by balance
print(genre_balance_df) 


#genres are extremely unbalanced, so will aggregate the less common genres into an "Other" category 

movies_genres_encoded$genres_Other <- 0 #create the other column with initial value of 0
genres_to_aggregate <- c("genres_Animation", "genres_Western", "genres_Television", 
                         "genres_Sports & Fitness", "genres_Cult Movies", "genres_Gay & Lesbian",
                         "genres_Faith & Spirituality", "genres_Anime & Manga", "genres_Special Interest", "genres_Musical & Performing Arts") #aggregate these genres
movies_genres_encoded$genres_Other <- rowSums(movies_genres_encoded[, genres_to_aggregate]) #sum the specified genres
movies_genres_encoded <- movies_genres_encoded[ , !(names(movies_genres_encoded) %in% genres_to_aggregate)] #drop the original columns for aggregated genres
movies_genres_encoded$genres_Other <- ifelse(movies_genres_encoded$genres_Other > 0, 1, 0) #convert to binary

#now we will recount to confirm the aggregation worked and check the balance again 
genre_occurrences <- colSums(movies_genres_encoded[, -which(names(movies_genres_encoded) %in% c("movie_title", "other_non_genre_columns"))])
genre_occurrences_df <- data.frame(Genre = names(genre_occurrences), Occurrences = genre_occurrences, row.names = NULL) #converting the list to a data frame 
genre_occurrences_df <- genre_occurrences_df[order(-genre_occurrences_df$Occurrences),] #sort by occurrences
print(genre_occurrences_df) 

genre_balance <- genre_occurrences_df$Occurrences / nrow(movies_cleaned) * 100
genre_balance_df <- data.frame(Genre = genre_occurrences_df$Genre, Balance = genre_balance, row.names = NULL) #converting the list to a data frame
genre_balance_df <- genre_balance_df[order(-genre_balance_df$Balance),] #sort by balance
print(genre_balance_df) 

#data is sstill skewed but balance is still skewed, may consider log transformation or LASSO model 


#bind the encoded genres back to the cleaned data set and drop the original genres column 
movies_cleaned <- movies_cleaned %>% select(-genres)
movies_cleaned <- left_join(movies_cleaned, movies_genres_encoded, by = "movie_title")

#cleaning up the environment by dropping the temporary data sets and unneeded values 
rm(movies_genre_long, movies_genres_encoded, genre_occurrences, genre_occurrences_df, genre_balance, genre_balance_df)
rm(genres_to_aggregate)

#additional feature engineering to add the required columns for hypothesis 2: seasonality  

#check for blank values in the release date column 
blank_release_dates <- sum(movies_cleaned$original_release_date == "")
blank_release_dates #no blank values so we can proceed


movies_cleaned$release_month <- format(movies_cleaned$original_release_date, "%m") #extract the month from the release date

movies_cleaned$release_year <- format(movies_cleaned$original_release_date, "%Y") #extract the year from the release date
movies_cleaned$release_month <- as.numeric(movies_cleaned$release_month) #convert to numeric for categorization
movies_cleaned$release_year <- as.numeric(movies_cleaned$release_year) #convert to numeric for categorization

movies_cleaned$season <- cut(movies_cleaned$release_month,
                             breaks = c(0, 3, 6, 9, 12), # Adjust breaks to include December in Winter
                             labels = c("Winter", "Spring", "Summer", "Fall"),
                             right = TRUE) # Include the right endpoint in intervals

summary(movies_cleaned$season) #check the season column

#check the distribution of the seasons
ggplot(movies_cleaned, aes(x = season)) + 
  geom_bar(fill = "tomato", color = "navy") +
  labs(title = "Distribution of Movies by Season",
       x = "Season", 
       y = "Number of Movies") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

#approximately equal distribution of movies across the seasons so we can proceed with the analysis 


#one hot encode the season column 
movies_cleaned <- fastDummies::dummy_cols(movies_cleaned, select_columns = "season", remove_selected_columns = TRUE)


#reconstructing seasons to examine time series analysis 
# Example of reconstructing the 'season' variable from one-hot encoded columns
movies_cleaned$season_visualization <- with(movies_cleaned,
                                            ifelse(season_Spring == 1, "Spring",
                                                   ifelse(season_Summer == 1, "Summer",
                                                          ifelse(season_Fall == 1, "Fall", "Winter")
                                                   )
                                            )
)

movies_cleaned$season_visualization <- factor(movies_cleaned$season_visualization, levels = c("Winter", "Spring", "Summer", "Fall"))
season_visualization <- as.factor(movies_cleaned$season_visualization) #convert to factor for visualization

#Check the data was properly encoded  

#additional feature engineering to add the required columns for actor popularity scores 

#Create actor_appearances table
actor_appearances <- table(unlist(strsplit(as.character(movies_cleaned$actors), ",\\s*")))

#Very large table, so will process in small batches for efficiency
batch_size <- 100  # Set the batch size
num_batches <- ceiling(nrow(movies_cleaned) / batch_size)
actor_popularity_scores <- integer(nrow(movies_cleaned))  # Initialize the vector to store scores

##WARNING - This function takes about 20 minutes to run on a standard computer 

# Initialize variables for tracking progress
current_row <- 1  # Start from the first row
total_rows <- nrow(movies_cleaned)

pb <- txtProgressBar(min = 0, max = total_rows, style = 3)  # Progress bar with accurate total steps

# Loop through each row until all movies are processed
while (current_row <= total_rows) {
  setTxtProgressBar(pb, current_row)  # Update progress bar
  
  # Process the current row
  actor_list <- strsplit(as.character(movies_cleaned$actors[current_row]), ",\\s*")[[1]]
  actor_popularity_scores[current_row] <- sum(actor_appearances[actor_list])
  
  current_row <- current_row + 1  # Move to the next row
}

close(pb)  # Close the progress bar
movies_cleaned$actor_popularity <- actor_popularity_scores #Assign the calculated scores back to the movies_cleaned data frame

#check for zeros in the actor popularity scores
sum(movies_cleaned$actor_popularity == 0) #no zeros so we can proceed 

#remove the actors list column 
movies_cleaned <- movies_cleaned %>% select(-actor_list) 


movies_cleaned$audience_count <- log1p(movies_cleaned$audience_count) #apply a log transformation to the auidence_count


#Check for outliers in the actor popularity scores 

ggplot(movies_cleaned, aes(x = "", y = actor_popularity)) + 
  geom_boxplot(fill = "tomato", color = "navy") +
  labs(title = "Distribution of Actor Popularity Scores",
       x = "", 
       y = "Actor Popularity Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title.x = element_blank(),  # Remove x axis label
        axis.ticks.x = element_blank(),  # Remove x axis ticks
        axis.text.x = element_blank())  # Remove x axis text 

#check distibution of actor popularity scores

ggplot(movies_cleaned, aes(x = actor_popularity)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 30) +
  labs(title = "Distribution of Actor Popularity Scores",
       x = "Actor Popularity Score", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

#highly skewed data, may consider a log transformation 

movies_cleaned$actor_popularity <- as.numeric(movies_cleaned$actor_popularity) #convert to numeric for regression analysis


#save the cleaned data set to a csv file
write.csv(movies_cleaned, "movies_cleaned_3.csv", row.names = FALSE)

names(movies_cleaned)
####### Question 3 - EXPLORATORY ANALYSIS ######## 

#create a correlation matrix to check for multicollinearity and correlation between the numeric columns

numeric_columns <- movies_cleaned %>%
  select(runtime, audience_rating, audience_count, tomatometer_rating, 
         age_at_streaming, num_actors, num_authors, num_directors, 
        actor_popularity) # Select only the numeric columns and exclude the dummy variables

# Calculate the correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")  

corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, 
         cl.pos = "r", 
         cl.ratio = 0.1,
         tl.cex = 0.8, 
         number.cex = 0.7, 
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), 
         title = "Correlation Matrix", 
         mar = c(0,0,1,0)) 

#matrix shows some potential multicollinearity between number of actors and actor_popularity so we wil remove num actors from the analysis 

movies_cleaned <- movies_cleaned %>% select(-num_actors) #drop the num_actors column


#check stargazer for the summary statistics excluding the dummy variables 

stargazer(movies_cleaned %>% select(-starts_with("season"), -starts_with("genres")), type = "text") #exclude the dummy variables


#plot the distribution of audience count had to use a log transformation to see the distribution 

ggplot(movies_cleaned, aes(x = audience_count)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 50) +  # Increased number of bins for more granularity
  labs(title = "Distribution of Audience Count",
       x = "Audience Count", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_y_continuous(labels = scales::comma)  # Ensure y-axis labels are not in scientific notation

#applying the log normalized the distribution 



#check the distribution of the years in the data set 

ggplot(movies_cleaned, aes(x = release_year)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 30) +
  labs(title = "Distribution of Release Years",
       x = "Release Year", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title 

#data is skewed but this is expected, will consider this in the analysis 

#maybe we should consider dropping movies released before 2000 to see if there is a difference in the ratings


#examining effect of year on seasonality of ratings 

#data set to large for meaningful visualization so will subset to a smaller sample 
sample_size <- floor(0.1 * nrow(movies_cleaned))
movies_sampled <- movies_cleaned[sample(nrow(movies_cleaned), sample_size), ] #take a 10% sample of the data set 

#check that the sample is representative of the original data set 

# Full dataset summary using dplyr for a cleaner output
summary_full <- movies_cleaned %>%
  summarise(Minimum = min(tomatometer_rating, na.rm = TRUE),
            First_Quartile = quantile(tomatometer_rating, 0.25, na.rm = TRUE),
            Median = median(tomatometer_rating, na.rm = TRUE),
            Mean = mean(tomatometer_rating, na.rm = TRUE),
            Third_Quartile = quantile(tomatometer_rating, 0.75, na.rm = TRUE),
            Maximum = max(tomatometer_rating, na.rm = TRUE))

# Sampled data summary
summary_sampled <- movies_sampled %>%
  summarise(Minimum = min(tomatometer_rating, na.rm = TRUE),
            First_Quartile = quantile(tomatometer_rating, 0.25, na.rm = TRUE),
            Median = median(tomatometer_rating, na.rm = TRUE),
            Mean = mean(tomatometer_rating, na.rm = TRUE),
            Third_Quartile = quantile(tomatometer_rating, 0.75, na.rm = TRUE),
            Maximum = max(tomatometer_rating, na.rm = TRUE))

# Combine full and sampled summaries into one data frame for comparison
comparison <- bind_rows(Full = summary_full, Sampled = summary_sampled)
print(comparison)

rm(summary_full, summary_sampled, comparison) #remove the temporary data sets

#plot the relationship between release year and tomatometer rating by season
year_breaks <- seq(min(movies_sampled$release_year, na.rm = TRUE),
                   max(movies_sampled$release_year, na.rm = TRUE), by = 5) 

movies_sampled$release_year <- as.numeric(movies_sampled$release_year)  # Convert release year to factor for better visualization


# Generate the plot
years_seasonality <- ggplot(movies_sampled, aes(x = release_year, y = tomatometer_rating)) +
  geom_jitter(aes(color = season_visualization), alpha = 0.6, width = 0.2, height = 0) +
  geom_smooth(method = "lm", aes(color = season_visualization), se = TRUE) +  # Adding confidence interval
  facet_wrap(~season_visualization, scales = "free_x") +
  theme_minimal(base_size = 12) +
  scale_x_continuous(breaks = seq(min(movies_sampled$release_year), max(movies_sampled$release_year), by = 5)) +  # Only show labels every 5 years
  scale_color_manual(values = c("Winter" = "#1f77b4", "Spring" = "#2ca02c", "Summer" = "#ff7f0e", "Fall" = "#d62728")) +  # Assign new colors
  labs(
    title = "Tomatometer Rating by Release Year (Faceted by Season)",
    x = "Release Year",
    y = "Tomatometer Rating"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),  # Bold and center the title with a smaller font size
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8),  # Smaller text for years
    plot.margin = margin(4, 4, 4, 4, "points"),  # Adjust margins if needed
    legend.title = element_blank()
  )

print(years_seasonality) #years_seasonlity stored as a data object for later use 

#while not entirely clear there seems to be between an inconsistent relationship betweeen release year and tomatometer rating by season, 
#we will need to run a regression to confirm and include the interaction term 




####### HYPOTHESIS TESTING AND MODELS ########  

movies_cleaned <- read.csv("/Users/chase/Documents/movies_cleaned_3.csv" , header = TRUE) #read in the cleaned data set

#Key Question: what factors influence the tomatometer rating of a movie on Rotten Tomatoes?   

#drop the movie_title, directors, authors, actors, original_release_date, streaming_release_date columns as they are not needed for the analysis

movies_cleaned <- movies_cleaned %>% select(-movie_title, -directors, -authors, -actors, -original_release_date, -streaming_release_date)
movies_cleaned <- movies_cleaned %>% select(-season_visualization) #drop the season_visualization column as it is not needed for the analysis
names(movies_cleaned)
#shuffling the data set to ensure randomness in the training and testing sets 
movies_cleaned <- movies_cleaned[sample(nrow(movies_cleaned)), ] # Shuffle the data set 

#movies_cleaned contains dummy variables so we will create a new df with one droped to avoid multicollinearity 
names(movies_cleaned)
OLS_set <- movies_cleaned %>% select(-season_Spring, -genres_Romance) #drop the season_Spring and genres_Action...Adventure columns to avoid multicollinearity

#splitting the data set into training and testing sets
set.seed(123) # Ensure reproducibility
training_indices <- createDataPartition(OLS_set$tomatometer_rating, p = 0.8, list = FALSE)
training_set <- OLS_set[training_indices, ]
testing_set <- OLS_set[-training_indices, ]  

#check for missing values in the data set 

sum(is.na(training_set)) #no missing values in the training set 
#remove the missing value from the training set 
training_set <- training_set[complete.cases(training_set), ] #remove the missing value from the training set
sum(is.na(testing_set)) #no missing values in the testing set


#run the first kitchen sink model to see the effect of all variables on the tomatometer rating 

lm_modelKS <- lm(tomatometer_rating ~ ., data = training_set)
summary(lm_modelKS) #R2 = 0.8099

#check for multicollinearity 
vif(lm_modelKS) #check the VIF to ensure no multicollinearity

#ask expected, age_at_streaming and release year are highly correlated so we will remove release_year from the model 
lm_model1 <- lm(tomatometer_rating ~ . - release_year, data = training_set)
summary(lm_model1) #check the summary statistics) #R2 = 0.243, P-value = 2.2e-16 (signifigant)
#set the plot to 2x2 
par(mfrow = c(2, 2))

plot(lm_model1) #check the residuals

#plot shows some high leverage points and issues with non-linearity, we will add interaction terms to attempt to account for non-linearity 


#step 2 - addressing non-linearity

#adding interaction terms 
# Model with interaction effects between release_year and each season

#formula to collect all genres and push them into formula string without manually typing 
genre_vars <- names(training_set)[grepl("^genres_", names(training_set))]
base_formula <- "tomatometer_rating ~ runtime + audience_rating + audience_count + age_at_streaming + num_authors + num_directors + actor_popularity + content_rating + release_year + season_Winter + season_Summer + season_Fall"
full_formula_str <- paste(base_formula, paste(genre_vars, collapse=" + "), sep=" + ")

#adds the interaction effect between release year and seaon to the formual 
interaction_terms <- " + release_year:season_Winter + release_year:season_Summer + release_year:season_Fall"
full_formula_str <- paste(full_formula_str, interaction_terms, sep="")


full_formula <- as.formula(full_formula_str)

lm_model2 <- lm(full_formula, data=training_set)

summary(lm_model2) #R2 = 0.5092, adding the interaction terms significantly reduced model performance 
plot(lm_model2)


#improved linearity but still not ideal, will consider adding polynomial terms to release year 



#adding polynomial terms to account for non-linearity in year and content ratings 


base_formula <- "tomatometer_rating ~ runtime + audience_rating + audience_count + age_at_streaming + num_authors + num_directors + actor_popularity + content_rating + poly(release_year, 2) + season_Winter + season_Summer + season_Fall"
full_formula_str <- paste(base_formula, paste(genre_vars, collapse=" + "), sep=" + ")
interaction_terms <- " + release_year:season_Winter + release_year:season_Summer + release_year:season_Fall"
full_formula_str <- paste(full_formula_str, interaction_terms, sep="")
full_formula <- as.formula(full_formula_str)
lm_model3 <- lm(full_formula, data=training_set)
summary(lm_model3) #R2 0.5145
plot(lm_model3)

#no change in linearity 

#running stepwise regression to determine the best model 

lm_model4<- step(lm_model3, direction = "both", trace = 1) #run the stepwise regression model
summary(lm_model4)
plot(lm_model4) 

lm_model5 <- step(lm_modelKS, direction = "both", trace = 1) #run the stepwise regression model 
summary(lm_model5)
plot(lm_model5)
#stepwise regression does not remove any variables so we will consider this the model for now  

AIC(lm_model5, lm_model4, lm_model3, lm_model2, lm_model1, lm_modelKS) #AIC shows the stepwise model is the best model

#AIC suggest lm_model1 (no interactions or polynomial terms) is the best model we will continue with this model for now. 

#cross validation of the model 

train_control <- trainControl(method = "cv", number = 10)  # Set up the cross-validation method 
lm_model1_cv <- train(tomatometer_rating ~ . - release_year, data = training_set, method = "lm", trControl = train_control)  # Fit the model using cross-validation
lm_model1_cv$results  # Display the results of the cross-validation
 

#cross-validation shows a stable model at 0.8091 R2 suggesting good model fit 

#using the test set to validate the model 

predictions <- predict(lm_model1, newdata = testing_set)  # Make predictions on the testing set 
rmse <- sqrt(mean((testing_set$tomatometer_rating - predictions)^2))  # Calculate the RMSE 
print(paste("RMSE:", round(rmse, 2)))  # Print the RMSE 

mae <- mean(abs(testing_set$tomatometer_rating - predictions))  # Calculate the MAE
print(paste("MAE:", round(mae, 2)))  # Print the MAE


results_df <- data.frame(Actual = testing_set$tomatometer_rating, Predicted = predictions) # Creating a data frame with actual and predicted values

# Plotting Actual vs Predicted values
ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point(aes(color = Actual), size = 2, alpha = 0.6) + 
  scale_color_gradient(low = "blue", high = "red") + 
  geom_smooth(method = lm, se = TRUE, color = 'darkred', fill = 'pink') +  
  xlab("Actual Tomatometer Rating") + 
  ylab("Predicted Tomatometer Rating") + 
  ggtitle("Actual vs Predicted Tomatometer Rating") +
  theme_minimal(base_size = 14) +  
  theme( 
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_blank(),  
    legend.position = "bottom"  
  ) +
  guides(color = guide_legend(title.position = "top"))  

#plot shows the model is generally good at fitting but loses accuracy in the extremes (below 20 and above 80)

stargazer(lm_model1, type = "text") #print the model summary statistics


### Step 2: Binary encoding into high and low to run logistic regression and CART models  

#calculate the median to split the data set into high and low ratings 
median(movies_cleaned$tomatometer_rating) #median is 63

#calculate the median to split the data set into high and low ratings 
movies_cleaned$rating_category <- ifelse(movies_cleaned$tomatometer_rating > 63, "high", "low") # Assign "high" to movies with a rating above 70
movies_cleaned$rating_category <- factor(movies_cleaned$rating_category, levels = c("low", "high"))


#splitting the data set into training and testing sets

#reshuffle the data set to ensure randomness in the training and testing sets

movies_cleaned <- movies_cleaned[sample(nrow(movies_cleaned)), ] # Shuffle the data set

#re-split the data set into training and testing sets

set.seed(123) # Ensure reproducibility
training_indices <- createDataPartition(movies_cleaned$rating_category, p = 0.8, list = FALSE)
training_set <- movies_cleaned[training_indices, ]
testing_set <- movies_cleaned[-training_indices, ] 

#check for missing values in the data set 
sum(is.na(training_set)) #no missing values in the training set
sum(is.na(testing_set)) #no missing values in the testing set

#remove the missing value from the training set 

training_set <- training_set[complete.cases(training_set), ] #remove the missing value from the training set

#removing the tomater_rating column as it will result in perfect multicollinearity 

glm_model1 <- glm(rating_category ~ . - release_year - tomatometer_rating, data = training_set, family = "binomial") 
summary(glm_model1)

#running a stepwise regression to see if we can improve the model 

glm_model2 <- step(glm_model1, direction = "both", trace = 1) #run the stepwise regression model

#evaluating the models 

pR2(glm_model1)
pR2(glm_model2)


AIC(glm_model1, glm_model2) #AIC shows the stepwise model is the best model 

#virtually no change but the stepwise model is slightly better so we will continue with this model 

predictions_prob <- predict(glm_model5, newdata = testing_set, type = "response")
predictions_class <- ifelse(predictions_prob > 0.5, "high", "low")

# Confusion Matrix
table(Predicted = predictions_class, Actual = testing_set$rating_category) 

# Accuracy
accuracy <- sum(predictions_class == testing_set$rating_category) / nrow(testing_set)
accuracy 

#model is generally accurate with a 71.3% accuracy rate 

#recall and precision 

# Confusion matrix values
TP <- 637  # True Positives: "high" predicted as "high"
FP <- 546  # False Positives: "low" predicted as "high"
FN <- 254  # False Negatives: "high" predicted as "low"

# Calculate Precision
Precision <- TP / (TP + FP)  

#precision score is 0.54, model has a tendency to predict high ratings

# Calculate Recall
Recall <- TP / (TP + FN) 

#recall is 0.71, model is good at predicting high ratings

# Calculate F1 Score
F1_Score <- 2 * (Precision * Recall) / (Precision + Recall) 

#F1 score is 0.62, suggesting the model is not perfect but generally better than random guessing 

# Print the results
cat("Precision for 'high':", Precision, "\n")
cat("Recall for 'high':", Recall, "\n")
cat("F1 Score for 'high':", F1_Score, "\n")



#running a CART model 

library(rpart)

# Assuming movies_cleaned_removed is your dataset and it's already processed
# Fitting a CART model to the training data
cart_model <- rpart(rating_category ~ runtime + age_at_streaming + content_rating +
                      actor_popularity + audience_count + num_authors + num_directors +
                      num_actors + release_month + genres_Comedy + genres_Horror +
                      genres_Art.House...International + genres_Documentary + genres_Drama +
                      genres_Kids...Family + genres_Musical...Performing.Arts +
                      genres_Mystery...Suspense + genres_Science.Fiction...Fantasy +
                      genres_Special.Interest + season_Winter + season_Summer +
                      season_Fall, 
                    data = training_set, method = "class")

print(cart_model)  # Display the CART model 

rpart.plot(cart_model, main="CART Model for Movie Ratings", extra=102, under=TRUE, faclen=0)


control <- trainControl(method="cv", number=10, savePredictions = TRUE, search = "grid")

results <- data.frame(maxdepth = integer(), 
                      Accuracy = numeric(), 
                      F1_Score = numeric(), 
                      Precision = numeric(),
                      Recall=numeric())

# Loop over desired maxdepth values
for (maxdepth in 1:20) {
  # Train the model with the current maxdepth setting using Cross Validation
  fit <- train(rating_category ~ runtime + age_at_streaming + content_rating +
                 actor_popularity + audience_count + num_authors + num_directors +
                 num_actors + release_month + genres_Comedy + genres_Horror +
                 genres_Art.House...International + genres_Documentary + genres_Drama +
                 genres_Kids...Family + genres_Musical...Performing.Arts +
                 genres_Mystery...Suspense + genres_Science.Fiction...Fantasy +
                 genres_Special.Interest + season_Winter + season_Summer +
                 season_Fall,  
               data = training_set, 
               method = "rpart",
               trControl = control, 
               tuneGrid = expand.grid(cp = 0.01), # cp is set to a single value as an example
               control = rpart.control(maxdepth = maxdepth, cp = 0.01)) # Adjust cp as needed
}

  # Predict on the testing set to get the confusion matrix
  predictions <- predict(fit, training_set, type = "raw")
  cm <- confusionMatrix(predictions, training_set$rating_category)
  
  # Calculate F1-score on test set
  precision <- as.numeric(cm$byClass['Pos Pred Value'])
  recall <- as.numeric(cm$byClass['Sensitivity'])
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # Collect and store the results
  accuracy <- as.numeric(max(fit$results$Accuracy))
  results <- rbind(results, 
                   data.frame(maxdepth = maxdepth, 
                              Accuracy = accuracy, 
                              F1_Score = f1_score,
                              Precision= precision,
                              Recall = recall))



# Plot F1_Score vs. Prediction Performance Metrics

# Convert the data to a long format for plotting with ggplot2
data_long <- reshape2::melt(results, 
                            id.vars = "maxdepth", 
                            variable.name = "Metric", 
                            value.name = "Value")

# Plotting
ggplot(data_long, aes(x = maxdepth, y = Value, color = Metric)) +
  geom_line() + geom_point() +
  scale_color_manual(values = c("Accuracy" = "blue", "F1_Score" = "red", "Precision" = "green", "Recall" = "black" )) +
  ggtitle("Model Performance by Max Depth") +
  xlab("Max Depth") +
  ylab("Score") +
  theme_minimal() 

best_maxdepth_f1 <- results$maxdepth[which.max(results$F1_Score)]

best_fit_F1 <- rpart(rating_category ~ runtime + age_at_streaming + content_rating +
                       actor_popularity + audience_count + num_authors + num_directors +
                       num_actors + release_month + genres_Comedy + genres_Horror +
                       genres_Art.House...International + genres_Documentary + genres_Drama +
                       genres_Kids...Family + genres_Musical...Performing.Arts +
                       genres_Mystery...Suspense + genres_Science.Fiction...Fantasy +
                       genres_Special.Interest + season_Winter + season_Summer +
                       season_Fall, 
                     data = training_set, 
                     method = "class", 
                     control = rpart.control(maxdepth = best_maxdepth_f1, cp = 0.01))


rpart.plot(best_fit_F1, main="CART Model for Movie Ratings", extra=102, under=TRUE, faclen=0) 

#test on the test set 

predictions <- predict(best_fit_F1, newdata = testing_set, type = "class") 

# Confusion Matrix

table(Predicted = predictions, Actual = testing_set$rating_category)

# Accuracy

accuracy <- sum(predictions == testing_set$rating_category) / nrow(testing_set)
accuracy

#recall and precision

# Confusion matrix values
TP <- 597  # True Positives: "high" predicted as "high"
FP <- 266  # False Positives: "low" predicted as "high"
FN <- 586  # False Negatives: "high" predicted as "low" 

# Calculate Precision

Precision <- TP / (TP + FP)
Precision #0.5384
# Calculate Recall

Recall <- TP / (TP + FN)
Recall #0.7149
# Calculate F1 Score

F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
F1_Score #0.6142 


#F1 score did not improve using best max depth so we will consider the model with the default max depth 
