###############################################
### WRANGLING EXERCISES
###############################################

library(tidyverse)

# load in a ratings dataset
load("data/movielens-small.Rdata")

# What movies get a higher rating on average, comedies or scifi?

# left_join ratings to movies
ratings <- ratings %>% left_join(movies, by = "movieId")

head(ratings)

# use "filter" to isolate comedies and scifis
ratings_com_sci <- ratings %>% filter(genres %in%c('Comedy','Sci-Fi'))


# use grouped_by and summarize to extract mean ratings for each
ratings_com_sci %>% group_by(genres) %>% summarize(meanRating = mean(rating))

# Everyone has a favourite movie, 2nd favourite, etc. What is the most common 5th favourite
# movie

# use "group_by" and "mutate" to rank each user's movie ratings from best to worst
# break ties at random (see ?rank)
ranked_user_ratings <- ratings %>% group_by(userId)%>%
  mutate(user_ranks = rank(-rating,ties.method = "random"))

?rank
# check to see its working!
ranked_user_ratings %>% arrange(userId, user_ranks)

# now extract everyone's 5th favourite movie with "filter"
ranked_user_ratings_5th <- ranked_user_ratings %>% filter(user_ranks ==5)

# make a frequency table using group_by and summarize
freq_table_5th <- ranked_user_ratings_5th %>% group_by(title) %>% summarize(count = n())
freq_table_5th

# sort from most to least common 5th movie
freq_table_5th %>% arrange(desc(count))

                           