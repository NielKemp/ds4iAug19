library(tidyverse)
library(NNLM)

# load in a ratings dataset
load("data/movielens-small.Rdata")

# take a subset to make it faster for class

# find out how many movies each user has seen
users_frq <- ratings %>% group_by(userId) %>% summarize(count = n()) %>% arrange(desc(count))
# take a subset of users (users who have seen the most movies, just to get lots of data)
my_users <- users_frq$userId[1:500]

# find out how many users have seen each movie
movies_frq <- ratings %>% group_by(movieId) %>% summarize(count = n()) %>% arrange(desc(count))
# take a subset of movies (movies with plenty of viewers)
my_movies <- movies_frq$movieId[101:150]

# extract ratings for the subset of users and movies, our "reduced" dataset
ratings_red <- ratings %>% filter(userId %in% my_users, movieId %in% my_movies) 

# check how many users and movies we have - 
# and note some users have been dropped because they haven't seen any of the movies
length(unique(ratings_red$userId))
length(unique(ratings_red$movieId))

# fill in the missing ratings with NAs, using complete
ratings_all <- complete(ratings_red, userId, movieId)


# reshape from long to wide format for use with NNLM package
ratings_wide <- ratings_all %>% select(-timestamp) %>% 
  spread(key = movieId, value = rating) %>% as.matrix()

# this matrix isn't quite in the format we want -- why not?
head(ratings_wide)

# get userId's from first column 
my_users <- ratings_wide[,1]

# get movieId's from column names
colnames(ratings_wide) # don't want the first one
my_movies <- colnames(ratings_wide)[-1] # [-1] leaves out the first entry

# drop the first column with the userId's
ratings_wide <- ratings_wide[, -1]

## fast matrix factorization with nnmf (?nnmf)

# this gives a warning because for users with only one rating the 
# system of equations is underdetermined (multiple solutions possible)
decomp <- nnmf(A = ratings_wide,
               method = "scd",
               k = 3,
               max.iter = 5000)

# regularization fixes it (see ?nnmf)
decomp <- nnmf(A = ratings_wide,
               method = "scd",
               k = 3,
               alpha = c(0.001,0,0), # coeff of 0.001 on L2 regularization
               beta = c(0.001,0,0),
               max.iter = 10000)

# return completed ratings matrix
pred_scores <- decomp$W %*% decomp$H


# accuracy as measured by rmse
mf_train_errors <- (pred_scores - ratings_wide)^2
mf_train_accuracy <- sqrt(mean(mf_train_errors[!is.na(ratings_wide)]))
mf_train_accuracy

# mse also returned as model output 
mse = tail(decomp$mse, n = 1)
sqrt(mse)

# make recommendations. Lots of different ways to do this, here's one

# change mf_observed back into a data frame 
pred_scores <- pred_scores %>% as.data.frame() 

# add back the column with userId's
pred_scores$userId <- my_users

# reshape back into long format
pred_scores_long <- pred_scores %>% gather(key = "movieId", value = "pred_rating", -userId)

# extract scores for a particular user
user_to_recommend_for <- 67
user_pred_scores <- pred_scores_long %>% 
  filter(userId == user_to_recommend_for) %>% arrange(desc(pred_rating))

# use left_join with ratings to see which movies user has already rated (and hence seen)
user_pred_scores %>% left_join(ratings, by = c("userId", "movieId"))

# there's an error -- why?
str(pred_scores_long)
str(ratings)

# convert movieid variable back to integer type
pred_scores_long <- pred_scores_long %>% mutate(movieId = as.integer(movieId))

# try again
user_pred_scores <- pred_scores_long %>% 
  filter(userId == user_to_recommend_for) %>% arrange(desc(pred_rating))
user_pred_scores <- user_pred_scores %>% left_join(ratings, by = c("userId", "movieId"))



# another left_join to bring in movie titles 
user_pred_scores <- user_pred_scores %>% left_join(movies, by = c("movieId")) %>% select(-genres)

# have a look at how the predictions did for this user
user_pred_scores %>% filter(!is.na(rating))

# drop the movies already seen to get the final recommendations
user_pred_scores %>% filter(is.na(rating))


####################### some more advanced stuff

## adding in bias terms

# nnmf does this in a particular way -- you need to set up an additional column
# of 1s for the H matrix, and a additional row of 1s for the W matrix

# note that nnmf decomposes A as W H + W_0 H_1 + W_1 H_0

init = list(
  H0 = matrix(1, nrow = 1, ncol = ncol(ratings_wide)),
  W0 = matrix(1, nrow = nrow(ratings_wide), ncol = 1)
)



# matrix factorization with bias in
decompBias <- nnmf(A = ratings_wide,
               method = "scd",
               k = 3,
               alpha = c(0.001,0,0),
               beta = c(0.001,0,0),
               init = init, # bias terms
               max.iter = 10000)

# results
predScoresBias <- decompBias$W %*% decompBias$H  # includes bias terms in it (see ?nnmf)
mseBias = tail(decompBias$mse, n = 1)
sqrt(mseBias)

#### TASK FOR TODAY
# 1) now do rest as before -- compare the recommendations with and without bias
# 2) experiment with values of "k" -- what happens?
# 3) does average error decrease if you increase the number of users/data you have? 
# (keep the number of movies constant, or you're comparing the same thing)

#TASK 1

#convert to datafram and add userIds
predScoresBias <- as.data.frame(predScoresBias)
predScoresBias$userId <- my_users

#selct user
userToScore <- 67

#transform data to long form
predScoresLongBias <- predScoresBias %>% gather(key = "movieId", value = "pred_rating", -userId)
head(predScoresLongBias)

#pull scores for selected user
userScoresBias <-predScoresLongBias %>%filter(userId ==userToScore)%>%arrange(desc(pred_rating))


head(ratings)
userScoresBias%>%left_join(ratings,by=c('movieId','userId'))

predScoresLongBias <- predScoresLongBias%>%mutate(movieId=as.integer(movieId))

userScoresBias <-predScoresLongBias %>%filter(userId ==userToScore)%>%arrange(desc(pred_rating))   


addedSeen <- userScoresBias %>% left_join(ratings, by=c('movieId','userId'))
addedSeed <- addedSeen %>%left_join(movies,by='movieId')%>%select(-genres)

addedSeed%>% filter(!is.na(rating))

addedSeed %>%filter(is.na(rating))


# have a look at how the predictions did for this user
addedSeed %>%filter(!is.na(rating))
user_pred_scores %>% filter(!is.na(rating))

# drop the movies already seen to get the final recommendations
user_pred_scores %>% filter(is.na(rating))

max(addedSeed$pred_rating)


trash <- addedSeed%>%group_by(userId)%>%mutate(maxi = max(pred_rating), totalRate = sum(pred_rating), relRating = pred_rating/sum(pred_rating))

sum(trash$relRating)
                                            