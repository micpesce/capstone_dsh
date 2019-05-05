# The goal of this project is to predict movie ratings given a dataset provided with "rating" as label column,
#and "userId", "movieId" as factors to be used as variables to compute predictor. The process will be carried
#on gradually  to test the effect of each variable on RMSE.
#
#The Movielens dataset has been split in two separate dataframes: the "edx" train dataframe that contains 90% of data, 
#on which the supervised algorithm is trained and cross-validated; the "validation" that contains 10% which
#is used as test set.

library(tidyverse)
library(ggplot2)

###@@@@@@@@@@@@@@@@@@ DATA CLEANING ########################

dim(movielens)
summary(movielens)
############################################################


###@@@@@@@@@@@@@@@@@@ DATA EXPLORATION ############################


#######MOVIE EFFECTS visualization########
#Generating a movie distribution plot: there are more than ten thousand 
#movies, they have been grouped and numbered as consecutive to produce
#uniform graphic ( the original movieId would appear with a central no data area)

#movies distribution and effect
movie_domain <- movielens %>% 
  group_by(movieId) %>% summarize(votes=n(),rating = mean(rating) )
nMovies <- movie_domain %>%  nrow(.)
df1 <- data.frame(x=c(1:nMovies), y=movie_domain$votes)  #temp dataframe
df2 <- data.frame(x=c(1:nMovies), y=movie_domain$rating) #temp dataframe
ggplot(df1,aes(x=df1$x,y=df1$y)) +
  ggtitle(" Movie distribution")+ geom_point() + scale_y_log10() +
  scale_x_continuous(limits=c(0,nMovies))+xlab("Movies")+ ylab("Votes")
ggplot(df2,aes(x=df2$x,y=df2$y)) +
  ggtitle(" Movie vs rating")+ geom_point() +
  scale_x_continuous(limits=c(0,nMovies))+xlab("MovieItem")+ ylab("rating") +
  geom_smooth()
##########



#######USERS EFFECTS visualization########
#users distribution and effect. Two plots: the first shows  users distribution
#vs grouped users. The second users VS rating

user_domain <- movielens %>% 
  group_by(userId) %>% summarize(votes=n(), rating = mean(rating))
nUsers <- user_domain %>%  nrow(.)
df1 <- data.frame(x=c(1:nUsers), y=user_domain$votes ) #temp dataframe
df2 <- data.frame(x=c(1:nUsers), y=user_domain$rating) #temp dataframe


ggplot(df1,aes(x=df1$x,y=df1$y)) +
  ggtitle(" User distribution")+ geom_point() + scale_y_log10() +
  scale_x_continuous(limits=c(0,nUsers))+xlab("Users")+ ylab("Votes")
ggplot(df2,aes(x=df2$x,y=df2$y)) +
  ggtitle(" User VS rating")+ geom_point() +
  scale_x_continuous(limits=c(0,nUsers))+xlab("Users")+ ylab("rating") +
  geom_smooth(span = 0.1)

rm(df1, df2,user_domain,movie_domain) # removing  temporary dataframes

######


#######TIME EFFECTS visualization########
#The next two code blocks generates plots to show the time effects on rating. The first one is week based
#the second day based. Both illustrates a weak influence on rating, so the time will not be considered 
# as predictor
library(lubridate)
movielens %>% mutate(datetime = round_date(as_datetime(timestamp))) %>% mutate(rating_week=week(datetime))%>%
  group_by(rating_week) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(rating_week, rating)) +
  geom_point() +
  geom_smooth()


movielens %>% mutate(datetime = round_date(as_datetime(timestamp))) %>% mutate(rating_day=day(datetime))%>%
  group_by(rating_day) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(rating_day, rating)) +
  geom_point() +
  geom_smooth()

###############################


#######GENRE EFFECTS visualization########
#the next code is cumputed to build-up a numeric variabile called "genreId"  as a numeric
#counterpart of the string "genre" variable
genre_domain <- movielens %>% 
  group_by(genres) %>% summarize(n=n())
nGenres <- genre_domain %>%  nrow(.)
genre_domain <- mutate(genre_domain, genreId=c(1:nGenres)) #create a new numeric genreId column  

#The next three lines of code: creating a temporary df as join from movielens and the
#df with numeric genreId, and  then tO select only candidate variables to compute for correlation.
#Finally, the plot that shows graphics of correlations ( ignoring the character variable genres ) )
#As the very last operation, we get the data frame used for prediction
movielens_temp <- inner_join(movielens,genre_domain, by = "genres") %>% mutate(datetime = round_date(as_datetime(timestamp))) %>% mutate(rating_week=week(datetime))
movielens_temp <- subset( movielens_temp, select = c("userId","movieId","rating","genreId", "genres") )

movielens <- movielens_temp

rm(movielens_temp,genre_domain,nGenres) #remove the temporary dfs and variables

##genres distribution and effect
genre_domain <- movielens %>% 
  group_by(genres) %>% summarize(votes=n(),rating = mean(rating) )
nGenres <- genre_domain %>%  nrow(.)
df1 <- data.frame(x=c(1:nGenres), y=genre_domain$votes) #temp dataframe
df2 <- data.frame(x=c(1:nGenres), y=genre_domain$rating) #temp dataframe
ggplot(df1,aes(x=df1$x,y=df1$y)) +
  ggtitle(" Genre distribution")+ geom_point() + scale_y_log10() +
  scale_x_continuous(limits=c(0,nGenres))+xlab("Genre")+ ylab("Votes")
ggplot(df2,aes(x=df2$x,y=df2$y)) +
  ggtitle(" Genre vs rating")+ geom_point() +
  scale_x_continuous(limits=c(0,nGenres))+xlab("Genre")+ ylab("rating") +
  geom_smooth()

remove(df1,df2,genre_domain,movie_domain,movie_titles) #removibìng temporary dfs


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@CORRELATION@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# As analysis final step, the correlation graphics will be created. Using the GGally
#library it' possible to get a one shot plot the shows the full correlation relationships across
#factors

library(GGally)
ggcorr(movielens,label = TRUE, label_alpha = TRUE)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###################################################################################


#@@@@T@@@@@@@@@@@@@@@@@@@@@@@@@THE MODELING APPROACH###############################
# The first step is to compute RMSE, that can be considered as a standard deviation between the true value in the test-set
#and the predicted value
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#####EDX AND VALIDATION SET#########
# Validation set obtained as 10% of MovieLens data
library(caret)
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# To make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm( ratings, movies, test_index, temp, movielens, removed)
#########################

# 1) Regularization and the user+movie effect approach 

#The rating distribution through movies, tells that some movies are rated more than others.
#Next code produces graphics that show movies versus ratings


library(ggplot2)
nMovies <- edx %>% count(movieId) %>% nrow(.) #total movies
edx %>% count(movieId)   %>% mutate( RateShare=ifelse(.$n>5,"moreThanFive","lessThanFive")) %>%
  ggplot(aes(x=c(1:nMovies),y=.$n, color=RateShare)) +
  ggtitle(" Ratings Distribution by movie")+ geom_point() + scale_y_log10() +
  scale_x_continuous(limits=c(0,nMovies))+xlab("Movies")+ ylab("Total ratings")
#The graphic is split in two parts: the upper in blu referring to the movies with  more than five ratings
#the lower in red to that movies with five or lower number of  ratings. The red section, just as an example,
#show that movies with few users ratings is a minor but a significant part of data
# This are noisy data, that should be processed by regularization

#The next step is to evaluate to implement the regularization principle centered on the lambda parameter.
#To minimize the   few rating noise effects , the lambda parameter is adopted. In the general
#mathematical formula lambda acts as a penalty when the n amount of ratings for a given movie
#is low, viceversa it is ignored when the n is high: b_i=sum(rating - mu)/(n+lambda) and b_u=sum(rating - mu-b_i)/(n+lambda)

#The optimal lambda must be chosen after tuning processing  that consists of a CROSS VALIDATION procedure
#For this, we have to further split the edx data frame in two data frames:
#The edx_train that is used to calculate b_u(lambda) and b_i(lambda) and the edx_test that is used 
#to calculated RMSEs


#@@@@@@@@@@@@@@@ Training and test set for CV##########
set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in validation set are also in edx set

edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm( test_index, temp, removed)

#######################################################




# In the general mathematical formula lambda acts as a penalty when the n amount of ratings for a given movie
#is low, viceversa it is ignored when the n is high
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test$rating))
})

ggplot2::qplot(lambdas, rmses)  

best_lambda <- lambdas[which.min(rmses)] #the lambda that minimize RMSE on the edx_test set



#Next, let's apply the best_lambda on the target edx and validation sets to 
#finally calculate predicted ratings

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+best_lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+best_lambda))

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

#the user+movie effect rmse 

rmse_ui_reg  <- RMSE(predicted_ratings, validation$rating)


# 2) KNN approach  

library(caret)
library(dplyr)

#######6% Sample of entire edx df########### 
edx_sample <- sample_n(edx, 600000)
set.seed(1)
test_index <- createDataPartition(y = edx_sample$rating, times = 1, p = 0.1, list = FALSE)
edx_sample_train <- edx_sample[-test_index,]
temp <- edx_sample[test_index,]

# Make sure userId and movieId in validation set are also in edx_sample set

edx_sample_test <- temp %>% 
  semi_join(edx_sample, by = "movieId") %>%
  semi_join(edx_sample, by = "userId")

# Add rows removed from validation set back into edx_sample set

removed <- anti_join(temp, edx_sample_test)
edx_sample_train <- rbind(edx_sample_train, removed)

rm( test_index, temp, removed)

####end sample#######################################################


# Fit the model on the edx_sample training set
Sys.time() #start time for evaluating computing time 
set.seed(123)
y <- edx_sample_train$rating
data=subset(edx_sample_train,select=c("movieId","userId")) #selecting only the predictors
model <- train(
  y=y, x=data,
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  data=data, method = "knn",
  tuneGrid = data.frame(k = seq(60, 100, 5))
)
# Plot model error RMSE vs different values of k
plot(model)
# Best tuning parameter k that minimize the RMSE
model$bestTune
# Make predictions on the test data
predictions <- model %>% predict(edx_sample_test)
head(predictions)
# Compute the prediction error RMSE
rmse_knn <-RMSE(predictions, edx_sample_test$rating)
Sys.time() #stop time for evalueting computing time 


#@@@@@@@@@@ Fit the KNN model on the whole edx training set
#not to be run on the 10M Dataframe on desktop system!!
Sys.time() 
set.seed(123)
y <- edx$rating
data=subset(edx,select=c("movieId","userId")) #selecting only the predictors
model <- train(
  y=y, x=data,
  trControl = trainControl("cv", number = 5),
  preProcess = c("center","scale"),
  data=data, method = "knn",
  tuneGrid = data.frame(k = seq(70, 95, 5))
)
# Plot model error RMSE vs different values of k
plot(model)
# Best tuning parameter k that minimize the RMSE
model$bestTune
# Make predictions on the test data
predictions <- model %>% predict(validation)

# Compute the prediction error RMSE
rmse_knn <-RMSE(predictions, validation$rating)
Sys.time()

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#save rmses in a dataframe and in a file
a <- c("knn","uireg")
b <- c(rmse_knn,rmse_ui_reg)
computed_rmses <- data.frame(a,b)
names(computed_rmses) <- c("method","rmse")
saveRDS(computed_rmses,"rmses.rds")








