
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
movies_cleaned <- movies_cleaned %>% 
  filter(content_rating != "NC17") #Remove NC17 ratings as these are related to TV shows and not movies 
movies_cleaned <- movies_cleaned %>% select(-movie_info) # Drop the movie_info column

#converting the data types of the columns 
movies_cleaned$runtime <- as.numeric(movies_cleaned$runtime)
movies_cleaned$audience_rating <- as.numeric(movies_cleaned$audience_rating)
movies_cleaned$audience_count <- as.numeric(movies_cleaned$audience_count)
movies_cleaned$original_release_date <- as.Date(movies_cleaned$original_release_date, format = "%Y-%m-%d")
movies_cleaned$streaming_release_date <- as.Date(movies_cleaned$streaming_release_date, format = "%Y-%m-%d")
movies_cleaned$genres <- as.factor(movies_cleaned$genres)
movies_cleaned$tomatometer_status <- as.actor(movies_cleaned$tomatometer_status, levels = c("Rotten", "Fresh", "Certified-Fresh")) #convert tomatometer_status to factor with levels for regression analysis
movies_cleaned$tomatometer_rating <- as.numeric(movies_cleaned$tomatometer_rating) #convert to numeric for regression analysis

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

#plot the distribution of the number of authors 
ggplot(movies_cleaned, aes(x = num_authors)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 30) +
  labs(title = "Distribution of Number of Authors",
       x = "Number of Authors", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

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


#genres are unbalanced so we will aggregate the genres into broader categories to balance them 

movies_genres_encoded$genres_Other <- 0 #create the other column with initial value of 0
genres_to_aggregate <- c("genres_Animation", "genres_Western", "genres_Television", 
                         "genres_Sports & Fitness", "genres_Cult Movies", "genres_Gay & Lesbian",
                         "genres_Faith & Spirituality", "genres_Anime & Manga") #aggregate these genres
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

#balance is better but still right skewed, we will consider this in the analysis and use LASSO regression to handle the imbalance


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

##WARNING - This function takes a very long time to run 
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

movies_cleaned$actor_popularity <- as.numeric(movies_cleaned$actor_popularity) #convert to numeric for regression analysis


#save the cleaned data set to a csv file
write.csv(movies_cleaned, "movies_cleaned_2.csv", row.names = FALSE)


####### EXPLORATORY ANALYSIS ######## 

#create a correlation matrix to check for multicollinearity and correlation between the numeric columns

numeric_columns <- movies_cleaned %>%
  select(runtime, audience_rating, audience_count, tomatometer_rating, 
         age_at_streaming, num_actors, num_authors, num_directors, 
         tomatometer_fresh_critics_count, actor_popularity) # Select only the numeric columns and exclude the dummy variables

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

#matrix shows no multicollinearity so we can proceed with the analysis but will check VIF after model building


#check stargazer for the summary statistics excluding the dummy variables 
stargazer(movies_cleaned %>% select(-starts_with("genres")), type = "text")

#plot the distribution of audience count had to use a log transformation to see the distribution 

ggplot(movies_cleaned, aes(x = audience_count)) + 
  geom_histogram(fill = "tomato", color = "navy", bins = 50) +  # Increased number of bins for more granularity
  labs(title = "Distribution of Audience Count",
       x = "Audience Count", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_x_log10(labels = scales::comma) +  # Apply a log10 transformation
  scale_y_continuous(labels = scales::comma)  # Ensure y-axis labels are not in scientific notation

#data is heavily right skewed so we will apply a log transformation to the audience count column. 
#note this changes the interpretation of the data to a percentage change in audience count 



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

#while not entirely clear there seems to be between an inconsistent relationship betweeen release year and tomatometer rating by season, we will need to run a regression to confirm and include the interaction term 


# Aggregating average ratings by year
annual_avg_rating <- movies_cleaned %>%
  group_by(release_year) %>%
  summarise(average_rating = mean(tomatometer_rating, na.rm = TRUE))

#fitting a linear regression model to the data
linear_model <- lm(average_rating ~ release_year, data = annual_avg_rating) 

#adding the fitted trend line to the plot
ggplot(annual_avg_rating, aes(x = release_year, y = average_rating)) +
  geom_point() +
  geom_line(aes(y = predict(linear_model)), color = "red") +
  theme_minimal() +
  labs(title = "Trend of Average Tomatometer Rating Over Years",
       x = "Release Year", y = "Average Tomatometer Rating")

# Fitting a polynomial regression model (2nd degree as an example)
fit <- lm(average_rating ~ poly(release_year, 2, raw=TRUE), data = annual_avg_rating)

# Adding the fitted trend line to the plot
ggplot(annual_avg_rating, aes(x = release_year, y = average_rating)) +
  geom_point() +
  geom_line(aes(y = predict(fit)), color = "red") +
  theme_minimal() +
  labs(title = "Trend of Average Tomatometer Rating Over Years",
       x = "Release Year", y = "Average Tomatometer Rating")

#ANOVA to check if the polynomial model is better than the linear model 

anova(fit, linear_model) 

#anova shows evidence of non-linearity so we may consider the polynomial model for the analysis 

####### HYPOTHESIS TESTING AND MODELS ########  



#Key Question: what factors influence the tomatometer rating of a movie on Rotten Tomatoes?   
movies_cleaned <- read.csv("/Users/chase/Documents/movies_cleaned_2.csv" , header = TRUE) 
movies_cleaned$content_rating <- factor(movies_cleaned$content_rating, levels = c("PG", "NR", "PG-13", "G", "R")) 

#shuffling the data set to ensure randomness in the training and testing sets 
movies_cleaned <- movies_cleaned[sample(nrow(movies_cleaned)), ] # Shuffle the data set

#splitting the data set into training and testing sets
set.seed(123) # Ensure reproducibility
training_indices <- createDataPartition(movies_cleaned$tomatometer_rating, p = 0.8, list = FALSE)
training_set <- movies_cleaned[training_indices, ]
testing_set <- movies_cleaned[-training_indices, ] 



lm_modelKS <- lm(tomatometer_rating ~ runtime + age_at_streaming + content_rating + actor_popularity + 
                  genres_Comedy + genres_Horror + genres_Art.House...International + 
                  genres_Documentary + genres_Drama + genres_Kids...Family + 
                  genres_Musical...Performing.Arts + genres_Mystery...Suspense + 
                  genres_Science.Fiction...Fantasy + genres_Special.Interest + 
                  audience_count + release_year + season_Winter + season_Summer +  
                  season_Fall + num_authors + num_directors + num_actors + release_month, data = training_set) #fit a linear regression model to the data

summary(lm_modelKS) #check the summary statistics)


#run stepwise regression to determine the best model 

stepwise_model <- step(lm_modelKS, direction = "both") #run stepwise regression
summary(stepwise_model) #check the summary statistics 
par(mfrow = c(2, 2))  # Set the layout to 2x2 plots 
plot(stepwise_model)





##### Hypothesis 2: The season in which a movie is released influences its tomatometer rating on Rotten Tomatoes. ##### 

season_model <- lm(tomatometer_rating ~ season_Fall + season_Spring + season_Summer, data = training_set) #fit a linear regression model to the data 
summary(season_model) #check the summary statistics
#model shows that winter and fall are significant predictors of tomatomer rating, with summer being less 

#signifigant, spring is not significant. This is suspect as the data set is balanced so we will consider interaction terms and additionl variables 

#overall model is signifigant but has an extremely low R2 

par(mforw = c(2, 2)) 
plot(season_model) #plot the diagnostic plots for the model 

#plot is highly unusual and suggests non-linearity and outliers 

#check the VIF to ensure no multicollinearity
vif(season_model) 


season_model2 <- lm(tomatometer_rating ~ season_Fall + season_Spring + season_Summer + release_year, data = training_set)
summary(season_model2)

#addition of release_year signifiantly improves the model, and changes significance of the seasons with season_summar no longer being significant. 
#model shows a decreasing trend in overall ratings but this is likely due to the increase in data over time and non-linear relationship 
#interaction between seasonlity and year is suspected so we will include an interaction term 
#R2 = 0.06319 


#check the plots 
par(mfrow = c(2, 2))  # Set the layout to 2x2 plots 
plot(season_model2)  # Plot the diagnostic plots for the model

#plots suggest non-linearity and slight homoscedasticity  

season_model3 <- lm(tomatometer_rating ~ season_Fall * release_year + season_Spring * release_year + season_Summer * release_year, data = training_set)
summary(season_model3) 

#new modles shows the rating for winter release is significantly higher than the other seasons, however the effect of this decreases slightly each year
#all variables are now significant and the R2 has increased to 0.06406 showing a very slight improvement 

plot(season_model3) #plot the diagnostic plots for the model
#plot still shows non-linearity and slight homoscedasticity so we will consider a polynomial model 

season_model4 <- lm(tomatometer_rating ~ poly(release_year, 2, raw = TRUE) + season_Fall * release_year + season_Spring * release_year + season_Summer * release_year, data = training_set)
summary(season_model4) #R2 = 0.08323 
plot(season_model4) #plot the diagnostic plots for the model 

#the model improves here suggesting a non-linear relationship between release year and tomatometer rating 
#all seasons and interaction effects are non-significant but the overall model is significant with an R2 of 0.0862 

#plots still suggest non-linearity and homoescadity not sure how to fix this 


