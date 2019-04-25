
library(tidyverse)
library(caret)

edx_reduced <-  edx[sample(nrow(edx), 100), ]
validation_reduced <-  validation[sample(nrow(validation), 10), ]




#
# start by naming my method to pass to train
#
movieCV <- list(type = "Regression",
              library = NULL,
              loop = NULL)
#
# define the tuning parameters
#

parm <- data.frame(parameter = "lambda",
                  class = "numeric",
                  label = "Lambda")

movieCV$parameters <- parm

movieCVGrid <- function(x, y, len = NULL, search = "grid") {
  if(search == grid) {
    out <- expand.grid( lambda = seq(1, 10))
  } else {
    stop('random search not yet implemented')
  }
  out
}

movieCV$grid <- movieCVGrid

movieCVFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  
  y<-as.numeric(y)
  mu <- mean(y)
  
  
  bi <- x %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(y - mu)/(n()+param)) 
  b_i <- bi$b_i
  
  bu <- x %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(y- b_i - mu)/(n()+param))
  b_u <- bu$b_u
  predicted_ratings <- 
    x %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
  
  
  return(predicted_ratings)
  
  
}

movieCV$fit <- movieCVFit

movieCVPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata)
}

movieCV$predict <- movieCVPred
fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 10,
                           ## repeated 3 times
                           repeats = 3)

movieCVGrid <- expand.grid(lambda = seq(1, 10))
movieCVProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
 predict(modelFit, newdata, type = "probabilities")
movieCV$prob <- movieCVProb                       

set.seed(825)
movieCVTune <- train(x = edx_reduced[, 1:3],
                  y = edx_reduced[, 3], data=edx_reduced,
                  method = movieCV,
                  trControl = fitControl,
                  tuneGrid = movieCVGrid )

predict(movieCVTune, newdata = edx_reduced$rating)




RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


pred_ratings <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  bi <- edx_reduced %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l)) 
    b_i <- bi$b_i
       
  b_u <- edx_reduced %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_reduced %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
  
  
  return(predicted_ratings)
})

t <- data.frame(parameter = c(5,7,0) ,
                class = c("a","b","c"),
                label = c("Lambda","",""))
