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


###@@@@@@@@@@@@@@@@@@ DATA EXPLORATION ########################

library(matrixStats)

#Generating a movie distribution plot: there are more than ten thousand 
#movies, they have been grouped and numbered as consecutive to produce
#uniform graphic ( the original movieId would appear with a central no data area)

#movies distribution and effect
movie_domain <- movielens %>% 
  group_by(movieId) %>% summarize(votes=n(),rating = mean(rating) )
nMovies <- movie_domain %>%  nrow(.)
df1 <- data.frame(x=c(1:nMovies), y=movie_domain$votes)
df2 <- data.frame(x=c(1:nMovies), y=movie_domain$rating)
ggplot(df1,aes(x=df1$x,y=df1$y)) +
  ggtitle(" Movie distribution")+ geom_point() + scale_y_log10() +
  scale_x_continuous(limits=c(0,nMovies))+xlab("Movies")+ ylab("Votes")
ggplot(df2,aes(x=df2$x,y=df2$y)) +
  ggtitle(" Movie vs rating")+ geom_point() +
  scale_x_continuous(limits=c(0,nMovies))+xlab("MovieItem")+ ylab("rating") +
  geom_smooth()
##########




#users distribution and effect
user_domain <- movielens %>% 
  group_by(userId) %>% summarize(votes=n(), rating = mean(rating))
nUsers <- user_domain %>%  nrow(.)
df1 <- data.frame(x=c(1:nUsers), y=user_domain$votes )
df2 <- data.frame(x=c(1:nUsers), y=user_domain$rating)

range(user_domain$votes)

ggplot(df1,aes(x=df1$x,y=df1$y)) +
  ggtitle(" User distribution")+ geom_point() + scale_y_log10() +
  scale_x_continuous(limits=c(0,nUsers))+xlab("Users")+ ylab("Votes")
ggplot(df2,aes(x=df2$x,y=df2$y)) +
  ggtitle(" User VS rating")+ geom_point() +
  scale_x_continuous(limits=c(0,nUsers))+xlab("Users")+ ylab("rating") +
  geom_smooth(span = 0.1)
###
rm(df1, df2,user_domain,movie_domain) # removing  temporary dataframes


##genre distribution

                        #######TIME EFFECTS########
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
df1 <- data.frame(x=c(1:nGenres), y=genre_domain$votes)
df2 <- data.frame(x=c(1:nGenres), y=genre_domain$rating)
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


# 1) KNN approach  

library(caret)
library(dplyr)

#######Sample for testing computing time
edx_sample <- sample_n(edx, 200000)
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
Sys.time()
set.seed(123)
y <- edx_sample_train$rating
data=subset(edx_sample_train,select=c("movieId","userId","genreId")) #selecting only the predictors
model <- train(
  y=y, x=data,
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  data=data, method = "knn",
  tuneGrid = data.frame(k = seq(80, 150, 5))
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
Sys.time()


#@@@@@@@@@@ Fit the KNN model on the whole edx training set
Sys.time()
set.seed(123)
y <- edx$rating
data=subset(edx,select=c("movieId","userId","genreId")) #selecting only the predictors
model <- train(
  y=y, x=data,
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  data=data, method = "knn",
  tuneGrid = data.frame(k = seq(70, 130, 5))
)
# Plot model error RMSE vs different values of k
plot(model)
# Best tuning parameter k that minimize the RMSE
model$bestTune
# Make predictions on the test data
predictions <- model %>% predict(validation)
head(predictions)
# Compute the prediction error RMSE
rmse_knn <-RMSE(predictions, validation$rating)
Sys.time()

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#As the simplest and naive approach we can predict the label only by computing the average rating of the train-set
mu <- mean(edx$rating)
mu
#[1] 3.512465

#The first naive RMSE is computed as follows using mu_hat as the predicted value
first_rmse <- RMSE(validation$rating, mu_hat)
first_rmse
#[1] 1.061202
# The first_rmse is the result of a simplest and poorest approach. It can be improved if we consider user and movie effects.

# As further step, we can consider the effect of movieId in order to make stronger our prediction; there is
# strong variability among movies, genres. The true rating, so far,
#can be thought as the sum of three components: the average, the movie effects,and a random error centered
#on zero:Y=μ+b_i+εiu. The movie effects bi variability can be seen observing the following graphics,
# computed by grouping movieIds and calculating distribution. 
mu <- mean(edx$rating) 
movies_variability <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
hist(movies_variability$b_i, main="Histogram of movies variability", xlab = "movieId" )

# Now, let' s consider a more accurate approach by calculating a new predicted rating as Y_hat= mu+b_i
# First of all, we must filter only the  b_i's referred to the movieId in the training AND test sets
#to compute further the predictor Y_hat_i ( the "i" as movieId effect), and then compute the new RMSE. Finally, a table to show comparisons between 
#RMSEs so far calculated
b_i <- validation %>%  left_join(movies_variability, by='movieId') %>% .$b_i
Y_hat_i= mu+b_i
second_rmse <- RMSE(Y_hat_i, validation$rating)
rmse_results <- data_frame(method = "Average rating", RMSE = first_rmse)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = second_rmse))
rmse_results %>% knitr::kable()

#|method             |      RMSE|
#|:------------------|---------:|
#|Average rating     | 1.0612018|
#|Movie Effect Model | 0.9439087|


#########################USER EFFECT######################
#Next, using the same approach, let's add users effects

#The following code compute histogram that shows variability among users, so that we can add this effect
#to the general model: Yui=mu+b_i+b_u+εiu
users_variability <- edx %>% group_by(userId) %>%  summarize(b_u = mean(rating))
hist(users_variability$b_u, main="Histogram of users variability", xlab = "userId" )

#Then the approximated b_u can be calculated as the difference between the rating in the train set and the sum 
#of mu and approximated b_i computed early: b_u= Yui-(b_i+mu). So, the new approximated predictor defined as Y_hat_ui
#is obtained as follows: Y_hat_ui=b_i+b_u+mu

users_effect <- edx %>% left_join(movies_variability, by='movieId') %>%   group_by(userId) %>%
summarize(b_u = mean(rating - mu - b_i))

Y_hat_ui <- validation %>%  left_join(movies_variability, by='movieId') %>%  left_join(users_effect, by='userId') %>%
  mutate( predictor = mu + b_i + b_u) %>% .$predictor

third_rmse <- RMSE(Y_hat_ui, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE =third_rmse))
rmse_results %>% knitr::kable()

#|method                     |      RMSE|
#|:--------------------------|---------:|
#|Average rating             | 1.0612018|
#|Movie Effect Model         | 0.9439087|
#|Movie + User Effects Model | 0.8653488|

######################### MODEL IMPROVEMENT BY REGULARIZATION ######################
#In order to improve the RMSE, It's worth to deepen data analysis. In particular, if we see the 
#rating distribution through movies, we can observe that some movies are rated more than others.



#@@@@@@@inserire qui il grafico@@@@@@@@

#Next,the code that produces a table that shows the relationship between some high ratings movies
#and the number of users ratings. In order to predict ratings, these are not reliable data because of
#the very few number of ratings

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

edx %>% count(movieId) %>% 
  left_join(movies_variability) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 


#@@@@@@@@@@@Inserire qui tabella@@@@@@@@



