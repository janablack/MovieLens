#Capstone - MovieLens Project R script
#Janalin Black 

#############################################################################
# Install libraries if needed for this project

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
#for pdf document
tinytex::install_tinytex()
install.packages('tinytex') 

library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(ggrepel)
library(ggthemes)
library('scales')
library(readxl)
library(kableExtra)
library(knitr)

#############################################################################
#The following code used for this project is generated from the Capstone course for EdX - Data Science
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
#############################################################################

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#rm(dl, ratings, movies, test_index, temp, movielens, removed)

#end of course generated code

#############################################################################

#Wrangling

#############################################################################

#Separate edx into train and test with split at 10% for test set
#This step complies with not using Validation dataset until final RSME result

set.seed(1, sample.kind="Rounding")
test_index_test <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index_test,]
temp_1 <- edx[test_index_test,]

# Make sure userId and movieId in test set are also in train set

test <- temp_1 %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set

removed_1 <- anti_join(temp_1, test)
train <- rbind(train, removed_1)

# Make sure userId and movieId in validation set are also in train set
# test set is kept separate to avoid over training

validation <- temp_1 %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

#delete unnecessary objects

rm(dl, ratings, movies, test_index, temp, movielens, removed,
   test_index_test, temp_1, removed_1)

#Initial preview of train dataset shows 6 columns with over 9 million observations. 
#timestamp needs to be converted to readable form
#release year needs to be separated from title
#genres include multiple genre categories. 
#If genres are used for prediction, they may need to be split.
#there are no missing values
head(edx)
str(edx)
dim(edx)
summary(edx$rating)
anyNA(edx)

#Convert timestamp into readable format for train, test, and validation sets

class(train$timestamp) <- c('POSIXt','POSIXct')
class(test$timestamp) <- c('POSIXt','POSIXct')
class(validation$timestamp) <- c('POSIXt','POSIXct')

#Extract year, month,and hour from timestamp

#train dataset
train$rateYear <- format(train$timestamp,"%Y")
train$rateMonth <- format(train$timestamp,"%m")
train$rateHour <- format(train$timestamp,"%H")

#test dataset
test$rateYear <- format(test$timestamp,"%Y")
test$rateMonth <- format(test$timestamp,"%m")
test$rateHour <- format(test$timestamp,"%H")

#validation dataset
validation$rateYear <- format(validation$timestamp,"%Y")
validation$rateMonth <- format(validation$timestamp,"%m")
validation$rateHour <- format(validation$timestamp,"%H")

#Validate consistent entries for title - all indicate "identical"
#this step insures title entries are consistent throughout the dataset
#This section is kept as a comment to avoid re-running lengthy calculations
#when R crashes

#train_1 <- grepl("\\d{4})$", train$title)
#identical(sum(train_1),nrow(train))
#test_1 <- grepl("\\d{4})$", test$title)
#identical(sum(test_1),nrow(test))
#validation_1 <- grepl("\\d{4})$", validation$title)
#identical(sum(validation_1),nrow(validation))

#Separate release year from title

#train dataset
train <- train %>%
  mutate(title = str_trim(title),
         title = str_sub(title, end = -2)) %>%
  separate(title,c("movieTitle", "movieYear"), "\\((?=[^\\(]+$)")

#test dataset
test <- test %>%
  mutate(title = str_trim(title),
         title = str_sub(title, end = -2)) %>%
  separate(title,c("movieTitle", "movieYear"), "\\((?=[^\\(]+$)")

#validation dataset
validation <- validation %>%
  mutate(title = str_trim(title),
         title = str_sub(title, end = -2)) %>%
  separate(title,c("movieTitle", "movieYear"), "\\((?=[^\\(]+$)")

#Created genre datasets separating each genre(added additional rows)
# Additional rows added to genre datasets will be accounted for prior
# to final prediction using original Validation dataset

#Separate each genre into rows

train_genre <- train %>% separate_rows(genres,sep = "\\|")
test_genre <- test %>% separate_rows(genres, sep = "\\|")
validation_genre <- validation %>% separate_rows(genres, sep = "\\|")

#############################################################################

#Exploration of the Data

#############################################################################

#What is the span of ratings? .5 to 5.0
#There are 10 rating choices which means that a random chance of predicting the 
#right rating is 10%

length(unique(test$rating))
min(unique(test$rating))
max(unique(test$rating))
summary(test$rating)
mean(test$rating)
median(test$rating)
label_percent()(1/length(unique(test$rating)))

#This is to make the train set look pretty

train_pretty <- head(train) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "left",
                full_width = FALSE,
                font_size = 11)
train_pretty

#This is to make the train_genre set look pretty

train_pretty_genre <- head(train_genre) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "left",
                full_width = FALSE,
                font_size = 11)
train_pretty_genre

#How many movies are in the test set? 9701 How many users? 68159

numbers <- test %>% summarize(n_movies = n_distinct(movieId),
                   n_users = n_distinct(userId))
numbers$n_movies
numbers$n_users

#How many times is a movie rated?
#A few movies are rated a lot of times. Many movies are rated fewer than 5 times.

test %>% count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth= .3, color = "black",
                 fill = "darkseagreen",
                 show.legend = FALSE) +
  scale_x_log10() +
  theme(plot.title = element_text(color = "#660033"),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Number of Reviews",
    y = "Number of Movies",
    title = paste(
      "Movie Ratings"
    ) 
  )


#How many times does a user rate a movie?
#It appears that most users rate fewer than 100 movies

test %>% count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth= .3, color = "black", fill = "darkseagreen",
                 show.legend = FALSE) +
  scale_x_log10() +
  theme(plot.title = element_text(color = "#660033", hjust = 1),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Number of Reviews",
    y = "Number of Users",
    title = paste(
      "User Ratings"
    ) 
  )

#Is there a difference between ratings and genre?

#first genre plot is the dataset with unsplit genres.
#There is evidence of differences in ratings based on combined genres.

unsplit_genres <- train %>%
  group_by(genres) %>%
  summarize(n = n(),
            Average = mean(rating),
            se = sd(rating)/sqrt(length(rating))) %>%
  filter(n > 90000)%>%
  mutate(Genres = reorder(genres, Average)) %>%
  ggplot(aes(Genres, Average, ymin = Average- 2*se, ymax = Average + 2*se)) +
  geom_point(color = "#006633") +
  geom_errorbar(color = "#009933")+
  theme(axis.text.x= element_text(angle = 90, hjust = 1),
        plot.title = element_text(color = "#660033"),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Genres",
    y = "Average Rating",
    title = paste(
      "Average Rating for Movies by Unsplit Genres"
    ) 
  )

#Number of separate genres in dataset is 20

count_genres <- train_genre %>%
  group_by(genres) %>%
  count(genres) %>%
  nrow()

#Second genre plot is the dataset with split genres.
#There is evidence of differences in ratings based on each genre.

split_genre <- train_genre %>%
  group_by(genres) %>%
  summarize(n = n(),
            avg = mean(rating),
            se = sd(rating)/sqrt(length(rating))) %>%
  filter(n > 100)%>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(genres, avg, ymin = avg- 2*se, ymax = avg + 2*se)) +
  geom_point(color = "#006633") +
  geom_errorbar(color = "#009933")+
  theme(axis.text.x= element_text(angle = 90, hjust = 1),
        plot.title = element_text(color = "#660033"),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Genre",
    y = "Average Rating",
    title = paste(
      "Average Rating for Movies by Each Genre"
    ) 
  )


#Does the year the movie was released impact ratings?
#This plot reveals a potential relationship between
#year movie was released and rating. Loess smoothing was included to
#show a possible multiple regression approach for prediction.
#There is predictive power in the year the movie was released, 
#except the conditional probability is not linear.

train$movieYear <- as.Date(train$movieYear, format='%Y')
movie_year <- train %>%
  group_by(movieYear) %>%
  summarize(n = n(),
            avg = mean(rating))%>%
  filter(n > 200)%>%
  ggplot(aes(x=as.numeric(movieYear), y=avg)) +
  geom_point(color="#006633") +
  geom_smooth(method="loess", span = .15, method.args = list(degree=1)) +
  scale_x_discrete(breaks = seq(1918,2008,10)) +
  theme(axis.text.x= element_text(angle = 90, hjust = 1),
        plot.title = element_text(color = "#660033",hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Years 1918-2008",
    y = "Average Rating",
    title = paste("Movie Release Year and Rating")
    )
movie_year

#Is there a relationship between movie release year and year of rating?
#Ratings go down over the years for both. 
#Do ratings fluctuate depending on year of rating? 
#Ratings appear to decrease over years. 
#Does this relate to year movie was released?
#Further exploration should be considered.

rate_year <- train %>%
  group_by(rateYear) %>%
  summarize(n = n(),
            avg = mean(rating))%>%
  filter(n > 50)%>%
  ggplot(aes(x=rateYear, y=avg, group = 1)) +
  geom_point(color="#006633") +
  geom_line(color="#006633")+
  theme(axis.text.x= element_text(angle = 90,hjust = 1),
        plot.title = element_text(color = "#660033",hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Years 1996-2009",
    y = "Average Rating",
    title = paste("Year Rated and Rating"
    ) 
  )
rate_year

#This is rateYear rating averages

rate_year_avg <- train %>%
  group_by(rateYear) %>%
  summarize(n = n(),
            avg = mean(rating))%>%
  filter(n > 50)

#min and max averages of rateYear

min(rate_year_avg$avg)
max(rate_year_avg$avg)
rate_year_avg[which.min(rate_year_avg$avg),1]
rate_year_avg[which.max(rate_year_avg$avg),1]

#created a sample from train set for speed

set.seed(1, sample.kind="Rounding")
train_sample <- train %>% sample_n(1000000)

#Boxplot of year of rating
#This shows equal boxes for all years.
#Median rates are at 75% of all ratings for 1997-2002.

year_rate_plot <- filter(train_sample,rateYear > 1996) %>%
  ggplot(aes(x = rateYear, y = rating)) + 
  geom_boxplot()+
  theme(axis.text.x= element_text(angle = 90),
        plot.title = element_text(color = "#660033", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Year",
    y = "Rating",
    title = paste(
      "Year of Review and Rating"
    ) 
  )

#This adds a highlight at the median line in the above boxplot

dat <- ggplot_build(year_rate_plot)$data[[1]]

year_rate_plot + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                            y=middle, yend=middle),
                              colour="darkgreen", size=1.5)

#A histogram of ratings by year of review for several sample years has
#a left skewed distribution for years before 2003. For predictive analysis, 
#a normalizing transformation should be considered.

year_histo <- filter(train, rateYear%in%c(1997,1999,2001,2003,2005,2007)) %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 1, color = "black", fill = "darkseagreen") +
  facet_grid(. ~ rateYear)+
  theme(plot.title = element_text(color = "#660033", hjust = 1),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Rating",
    y = "Count",
    title = paste(""))
year_histo

#Do ratings fluctuate depending on hour of day? 
#The middle 50% of all boxplots from a sample of the train set
#are quite similar. However, median ratings
#fluctuate between 3.5 and 4 and only change after several hours.

hour_box <- train_sample %>%
  ggplot(aes(x = rateHour, y = rating)) + 
  geom_boxplot()+
  theme(axis.text.x= element_text(),
        plot.title = element_text(color = "#660033", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Hour",
    y = "Rating",
    title = paste(
      "Hour of Review and Rating"
    ) 
  )
#This adds a highlight at the median line in the above boxplot

dat_2 <- ggplot_build(hour_box)$data[[1]]

hour_box + geom_segment(data=dat_2, aes(x=xmin, xend=xmax,
                                            y=middle, yend=middle),
                              colour="darkgreen", size=1.5)

#Further exploration on hour of day as a potential predictor 
#shows a plot with mean and median ratings by hour of day.
#In most instances the mean is lower than the median. 
#This indicates a left or negatively skewed data.
#The most common values in the distribution might not be near the mean.
#Additionally, this skewed data can affect which types of analyses to perform.

r_hour <- train %>%
  select(rateHour, rating) %>%
  group_by(rateHour) %>%
  summarize(mean = mean(rating), median = median(rating)) %>%
  arrange(desc(mean))
#combining mean and median data into one plot
r_hour2 <- melt(data = r_hour, id.vars = "rateHour")
r_hour2 <- r_hour2 %>% mutate(value = format(round(value, 2), nsmall = 2))
ggplot(data = r_hour2, aes(x = rateHour, y = value,
                           colour = variable, group = 1)) +
  geom_point() +
  theme(axis.text.x= element_text(angle = 90,hjust = 1),
      axis.title.x = element_text(color = "#000066"),
      axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Hour of Day",
    y = "Rating"
  )

#Do ratings fluctuate depending on month of year?
#This plot shows higher ratings for months 10-12.

rate_month <- train %>%
  group_by(rateMonth) %>%
  summarize(n = n(),
            avg = mean(rating))%>%
  ggplot(aes(x=rateMonth, y=avg)) +
  geom_point(color="#006633") +
  #geom_smooth() +
  theme(axis.text.x= element_text(hjust = 1),
        plot.title = element_text(color = "#660033",hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Month",
    y = "Average Rating",
    title = paste(
      "Month Reviewed and Rating"
    ) 
  )
rate_month

#############################################################################

#Prediction

#############################################################################

#Increase decimal places for RMSE precision

options(pillar.sigfig = 12)

#This is the function to calculate RMSE as indicated by assignment expectations

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#This is just the average rating of all movies. This is the first step to prediction.

mu <- mean(train$rating)

#This calculates the RMSE for the average rating of all movies

base_rmse <- RMSE(mu,test$rating)

#This is the RMSE results table with just the average

rmse_results <-
  tibble(method = "Basic average",
         RMSE = base_rmse)

#Based on the movie/ratings plot, there is a movie effect.
#This calculates the movie effect(bi) using Least Squares approximation

movie_bi <- train %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - mu))

#Movie effect RMSE results

bi_rating <- test %>% 
  left_join(movie_bi, by='movieId') %>%
  mutate(movie_pred = mu + bi)%>%
  pull(movie_pred)
bi_rmse <- RMSE(bi_rating, test$rating)

#RMSE results table adding Movie effect

movie_rmse <- bind_rows(rmse_results,
                        data_frame(method="Movie effect",
                                   RMSE = bi_rmse ))

#Based on the user/ratings plot, there is a user effect.
#This calculates the user effect(bu) using Least Squares approximation.

user_bu <- train %>% 
  left_join(movie_bi, by='movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi))

#User and Movie effect RMSE results

bi_bu_rating <- test %>% 
  left_join(movie_bi, by='movieId') %>%
  left_join(user_bu, by='userId') %>%
  mutate(predict = mu + bi + bu) %>%
  pull(predict)
bu_rmse <- RMSE(bi_bu_rating, test$rating)

#RMSE results table including adding user effect

user_rmse <- bind_rows(movie_rmse,
                       data_frame(method="User effect",
                                  RMSE = bu_rmse ))

#Based on the genre/ratings plots, there is a genre effect.
#This calculates the genre effect(bg) using Least Squares approximation.
#mu(mean of train$rating) was used to compare with mu_g(mean of train_genre$rating)
#due to increased number of rows in train_genre table.
#RMSE results were the same using using mu_g over mu in train_genre.

genre_bg <- train_genre %>% 
  left_join(movie_bi, by='movieId') %>%
  left_join(user_bu, by='userId') %>%
  group_by(genres) %>%
  summarize(bg = mean(rating - mu - bi - bu))

#User, Movie, and Genre mu effect RMSE results

bi_bu_bg_ratings <- test_genre %>% 
  left_join(movie_bi, by='movieId') %>%
  left_join(user_bu, by='userId') %>%
  left_join(genre_bg, by='genres') %>%
  #opted to use mu for test set
  mutate(predict = mu + bi + bu + bg) %>%
  pull(predict)
bg_rmse <- RMSE(bi_bu_bg_ratings, test_genre$rating)

#RMSE results table adding genre mu effect

genre_rmse <- bind_rows(user_rmse,
                        data_frame(method="Genre mu effect",
                                   RMSE = bg_rmse ))

#Add Genre effect(bg)with mu_g using Least Squares approximation
#this step was added to compare to prior calculation using mu

mu_g <- mean(train_genre$rating)

genre_bg_mug <- train_genre %>% 
  left_join(movie_bi, by='movieId') %>%
  left_join(user_bu, by='userId') %>%
  group_by(genres) %>%
  summarize(bg = mean(rating - mu_g - bi - bu))

#User, Movie, and Genre effect RMSE results

bi_bu_bg_ratings_mug <- test_genre %>% 
  left_join(movie_bi, by='movieId') %>%
  left_join(user_bu, by='userId') %>%
  left_join(genre_bg_mug, by='genres') %>%
  mutate(predict = mu_g + bi + bu + bg) %>%
  pull(predict)
bg_rmse_mug <- RMSE(bi_bu_bg_ratings_mug, test_genre$rating)

#RMSE results table adding genre mu_g effect
#results indicate that mu_g produces a slightly better result for RMSE

genre_rmse_mug <- bind_rows(genre_rmse,
                            data_frame(method="Genre mu_g effect",
                                       RMSE = bg_rmse_mug ))

#The span of ratings is between .5 to 5.0 which includes 10 rating choices.
#A random chance of predicting the right rating is 10%
# What's the accuracy of our model so far? 25.23%
#Can we improve this accuracy with Regularization?

ac <- bi_bu_bg_ratings_mug
ac <- ceiling(ac / 0.5) * 0.5
ac[ac <= 0.5] <- 0.5
ac[ac >= 5] <- 5
summarization = confusionMatrix(as.factor(ac), as.factor(test_genre$rating))
summarization$overall

###########
#REGULARIZATION - penalizing least squares

#Finding the best penalty (lambda) for movie effect

#this lambda was selected based on exploration of different lambda numbers.
#this sequence produced acceptable calculation time. Further divisions took forever.

lambda <- seq(0, 10, 0.25)

movie_bi_r <- sapply(lambda, function(x){
  mu <- 
    mean(train$rating)
  m_bi <-
    train %>% 
    group_by(movieId) %>% 
    summarize(m_bi = sum(rating - mu)/(n()+x))
  
  predicted <-
    test %>% 
    left_join(m_bi, by='movieId') %>%
    mutate(movie_pred = mu + m_bi)%>%
    pull(movie_pred)
  
  return(RMSE(predicted, test$rating))
})

#Visualize best lambda for movie predictor indicates minimum lambda

qplot(lambda,movie_bi_r)

#Optimal penalty (lambda) for movie predictor is 1.5

lambda[which.min(movie_bi_r)] 

#Best RMSE 

bi_reg_rmse <- min(movie_bi_r)

#RMSE results table adding Regularized movie effect

reg_movie_rmse <- bind_rows(genre_rmse_mug,
                            data_frame(method="Regularized movie effect",
                                       RMSE = bi_reg_rmse ))

#Finding the best penalty (lambda) for user effect

user_bu_r <- sapply(lambda, function(x){
  mu <- 
    mean(train$rating)
  m_bi <-
    train %>% 
    group_by(movieId) %>% 
    summarize(m_bi = sum(rating - mu)/(n()+x))
  m_bu <-
    train %>%
    left_join(m_bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(m_bu = sum(rating - mu - m_bi)/(n()+x))
  
  predicted <-
    test %>% 
    left_join(m_bi, by='movieId') %>%
    left_join(m_bu, by="userId") %>%
    mutate(movie_pred = mu + m_bi + m_bu)%>%
    pull(movie_pred)
  
  return(RMSE(predicted, test$rating))
})

#Visualized best lambda for user effect indicates minimum lambda

qplot(lambda,user_bu_r)

#Optimal penalty (lambda) for user predictor is 5

lambda[which.min(user_bu_r)] 

#Best RMSE

bu_reg_rmse <- min(user_bu_r)

#RMSE results table adding regularized user effect

reg_user_rmse <- bind_rows(reg_movie_rmse,
                           data_frame(method="Regularized user effect",
                                      RMSE = bu_reg_rmse))

#Finding the best penalty (lambda) for genre effect with mu

genre_bg_r_mu <- sapply(lambda, function(x){
  mu <- 
    mean(train$rating)
  m_bi <-
    train %>% 
    group_by(movieId) %>% 
    summarize(m_bi = sum(rating - mu)/(n()+x))
  m_bu <-
    train %>%
    left_join(m_bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(m_bu = sum(rating - mu - m_bi)/(n()+x))
  m_bg <-
    train_genre %>%
    left_join(m_bi, by="movieId") %>%
    left_join(m_bu, by="userId") %>%
    group_by(genres) %>%
    summarize(m_bg = sum(rating - mu - m_bi - m_bu)/(n()+x))
  
  predicted <-
    test_genre %>% 
    left_join(m_bi, by='movieId') %>%
    left_join(m_bu, by="userId") %>%
    left_join(m_bg, by="genres") %>%
    mutate(movie_pred = mu + m_bi + m_bu + m_bg)%>%
    pull(movie_pred)
  
  return(RMSE(predicted, test_genre$rating))
})

#Visualized best lambda for genre effect indicates minimum lambda

qplot(lambda,genre_bg_r_mu)

#Optimal penalty (lambda) for genre predictor is 5

lambda[which.min(genre_bg_r_mu)] 

#Best RMSE

bg_reg_rmse_mu <- min(genre_bg_r_mu)

#RMSE results table adding Regularized mu genre effect
reg_genre_rmse_mu <- bind_rows(reg_user_rmse,
                               data_frame(method="Regularized mu genre effect",
                                          RMSE = bg_reg_rmse_mu ))

#Finding the best penalty (lambda) for genre effect with mu_g
#mu_g used for train_genre because RMSE was lower than mu

genre_bg_r <- sapply(lambda, function(x){
  mu <- 
    mean(train$rating)
  mu_g <-
    mean(train_genre$rating)
  m_bi <-
    train %>% 
    group_by(movieId) %>% 
    summarize(m_bi = sum(rating - mu)/(n()+x))
  m_bu <-
    train %>%
    left_join(m_bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(m_bu = sum(rating - mu - m_bi)/(n()+x))
  m_bg <-
    train_genre %>%
    left_join(m_bi, by="movieId") %>%
    left_join(m_bu, by="userId") %>%
    group_by(genres) %>%
    summarize(m_bg = sum(rating - mu_g - m_bi - m_bu)/(n()+x))
  
  predicted <-
    test_genre %>% 
    left_join(m_bi, by='movieId') %>%
    left_join(m_bu, by="userId") %>%
    left_join(m_bg, by="genres") %>%
    mutate(movie_pred = mu_g + m_bi + m_bu + m_bg)%>%
    pull(movie_pred)
  
  return(RMSE(predicted, test_genre$rating))
})

#Visualize best lambda for genre effect indicates minimum lambda

qplot(lambda,genre_bg_r)

#Optimal penalty (lambda) for genre predictor is 5

lambda[which.min(genre_bg_r)] 
bg_reg_rmse <- min(genre_bg_r)

#RMSE results table adding Regularized mu_g genre effect
#This result appears to be slightly better than mu genre
#effect for the final model.

reg_genre_rmse <- bind_rows(reg_genre_rmse_mu,
                            data_frame(method="Regularized mu_g genre effect",
                                       RMSE = bg_reg_rmse ))


#RMSE results table with all scores to use for RMD file

options(digits = 12)
train_rmse_table <- data_frame(Test_Prediction =
                                 c("Basic average","Movie effect",
                                   "User effect","Genre mu effect",
                                   "Genre mu_g effect","Regularized movie effect",
                                   "Regularized user effect","Regularized mu genre effect",
                                   "Regularized mu_g genre effect"),
                               RMSE = c(base_rmse, bi_rmse,
                                        bu_rmse, bg_rmse,
                                        bg_rmse_mug, bi_reg_rmse,
                                        bu_reg_rmse, bg_reg_rmse_mu,
                                        bg_reg_rmse))
train_rmse_table %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE,
                font_size = 14)

#This table compares mu with mu_g for genre effect with regularization

best_train_rmse_mu <-
  tibble(method = "Regularized Movie+User+Genre With mu",
         RMSE = bg_reg_rmse_mu)

best_train_rmse <- bind_rows(best_train_rmse_mu,
            data_frame(method="Regularized Movie+User+Genre With mu_g",
                       RMSE = bg_reg_rmse ))

best_train_rmse %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE,
                font_size = 11)

#Apply final model to Validation set

#assign best lambda for each prediction
movie_lambda <- lambda[which.min(movie_bi_r)] 
user_lambda <- lambda[which.min(user_bu_r)] 
genre_lambda <- lambda[which.min(genre_bg_r)]

#mu_g was used for genre prediction only. This was selected because
#it produced a slightly lower RMSE in test dataset compared to mu.
#However, calculations using mu only also produced results acceptable for RSME number.

#This is the final model

m_bi <-
  train %>% 
  group_by(movieId) %>% 
  summarize(m_bi = sum(rating - mu)/(n()+movie_lambda))
m_bu <-
  train %>%
  left_join(m_bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(m_bu = sum(rating - mu - m_bi)/(n()+user_lambda))
m_bg <-
  train_genre %>%
  left_join(m_bi, by="movieId") %>%
  left_join(m_bu, by="userId") %>%
  group_by(genres) %>%
  summarize(m_bg = sum(rating - mu_g - m_bi - m_bu)/(n()+genre_lambda))

#Here is the final prediction
#validation_genre dataset used here to account for extra rows in m_bg prediction.
#Individual genre predictions were averaged for each user/movie combination to test
#against untouched validation dataset for final RMSE score.
m_bg_mean <-
  validation_genre %>% 
  left_join(m_bi, by='movieId') %>%
  left_join(m_bu, by="userId") %>%
  left_join(m_bg, by="genres") %>%
  group_by(userId, movieId) %>%
  mutate(mean_bg = mean(m_bg))%>%
  select(userId, movieId, mean_bg) %>%
  distinct(mean_bg)%>%
#Here are final predictions to test against original validation dataset.
  left_join(m_bi, by='movieId')%>%
  left_join(m_bu, by="userId")%>%
  mutate(movie_pred = mu_g + m_bi + m_bu + mean_bg)%>%
  pull(movie_pred)

#Final RMSE result obtained from original validation dataset. 
#This RMSE passes assignment requirement for full credit.

final_rmse <- RMSE(m_bg_mean, validation$rating)

final_rmse_validation <-
  tibble(Conclusion = "Final Model Used On Validation Set",
         RMSE = format(final_rmse, digits = 5)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE,
                font_size = 20) %>%
  column_spec(2, bold = T, color = "white", background = "blue")

#Here is the RMSE used on validation set
final_rmse_validation

