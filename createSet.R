#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(data.table)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- "C:\\newFiles\\R_PROJECTS\\capstone\\temp"
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                      col.names = c("userId", "movieId", "rating", "timestamp"))
colNames = c("userId", "movieId", "rating", "timestamp")
ratings <- fread(input="ml-10M100K/ratings.dat", sep= "auto")
ratings <- ratings %>% select(V1,V3,V5,V7)
names(ratings) <-colNames



movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
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

rm( ratings, movies, test_index, temp, movielens, removed)


library(gdata)
library(dplyr)
library(stringr)
library(tidyr)
#q1
dim(edx)
#q2
threes <- edx %>% filter(rating=="3") %>% count()
zeros <- edx %>% filter(rating=="0") %>% count()
#q3
totalMovies <-edx %>% summarise(count = n_distinct(movieId))
#q4
totalUsers <- edx %>% summarise(count = n_distinct(userId))



#q5
edx <- edx %>% mutate(genres=str_replace(genres,"Sci-Fi","SciFi"))
validation <- validation %>% mutate(genres=str_replace(genres,"Sci-Fi","SciFi"))
gen <- separate_rows(edx, genres ,  convert = FALSE) %>% group_by(genres) %>%count()

#q6gead
#Which movie has the greatest number of ratings?

#mia risposta OK
edx %>% group_by(rating) %>% count(title) %>% group_by(title) %>% summarise(sum=sum(n))  %>% arrange(desc(sum))
#GRADER
edx %>% group_by(movieId, title) %>% summarize(count = n()) %>%  arrange(desc(count))

#q7

#What are the five most given ratings in order from most to least?
#mia risposta OK
edx %>% group_by(rating) %>% count() %>%  arrange(desc(n))
#GRADER
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%  arrange(desc(count)) 


#q8

edx %>%  group_by(rating) %>%  summarize(count = n()) %>%  ggplot(aes(x = rating, y = count)) +
  geom_line()


part <- movielens %>% slice(1:550)

library(ggplot2)

part %>% ggplot( aes(movieId, rating, color=ifelse(userId>5,"red","black")) ) + geom_point()
head(part)
range(part$userId)
