library(tidyverse)

#prep the data from lecture 1 (recommender.RData)
load('data/movielens-small.RData')

ratings <-as.tibble(ratings)
ratings <- left_join(ratings,movies)

#select 15 users, (with moderately frequent viewing habits)
usersFrq <- ratings %>%group_by(userId)%>%summarize(count=n())%>%arrange(desc(count))
myUsers <- usersFrq$userId[101:115]

moviesFrq <- ratings%>%group_by(movieId)%>%summarize(count=n())%>%arrange(desc(count))
myMovies <- moviesFrq$movieId[101:120]

#now make a dataset with only these 15 users and 20 movies
ratings_red <- ratings%>%filter(userId %in%myUsers,movieId%in%myMovies)

#all the movie titles are being kept, fix this (?!?!?!?!)
ratings_red <- droplevels(ratings_red)
levels(ratings_red$title)

#reshape the data
ratings_red %>%select(userId,title,rating) %>%spread(key=title,value=rating)

viewed_movies <-ratings_red%>%
  complete(userId,title)%>%
  mutate(seen=ifelse(is.na(rating),0,1))%>%
  select(userId,title,seen)%>%
  spread(key=title,value=seen)

#another way of getting the data in the format
table(ratings_red$userId,ratings_red$title)

#save data for later use (in Lecture 2)
dir.create("output")
save(ratings_red,viewed_movies,file="output/recommender.RData")
