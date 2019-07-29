## ***************** DATA EXPLORATION ************************


## recommenderlab to build recommender systems and ggplot2 to visualize their results

library("recommenderlab")
library("ggplot2")
data(MovieLense)
class(MovieLense)

## extract realRatingMatrix object MovieLense size 
dim(MovieLense)

## 943 users and 1664 movies

## Components of the objects are contained in MovieLense slots
## See all the slots using slotNames
## It displays all the data stored within an object
slotNames(MovieLense)

## MovieLense contains a data slot. Explore it
class(MovieLense@data)
dim(MovieLense@data)

## MovieLense@data belongs to the dgCMatrix class that inherits from Matrix
## Explore the data using this slot

## ***** LET US EXPLORE VALUES OF THE RATINGS ****** 

## convert the matrix into a vector and explore its values:
vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)

## The ratings are integers in the range 0-5
## count the occurrences of each of them
table_ratings <- table(vector_ratings)

table_ratings

## Rating equal to 0 represents a missing value, so we can remove them from vector_ratings
vector_ratings <- vector_ratings[vector_ratings != 0]

## build a frequency plot of the ratings. Convert them into categories using factor
vector_ratings <- factor(vector_ratings)

## build a quick chart
qplot(vector_ratings) + ggtitle("Distribution of the ratings")

## Any comments on chart above? 

## ****** EXPLORE MOST VIEWED MOVIES  ******

## colCounts: This is the number of non-missing values for each column


## Which are the most viewed movies? First count views of each movie
views_per_movie <- colCounts(MovieLense)

## sort the movies by number of views
table_views <- data.frame(
  movie = names(views_per_movie),
  views = views_per_movie
)
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]

## Visualize the first six rows and build a histogram

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Number of views of the top movies")

## Any Comments on above plot ?

## *************** EXPLORE THE AVERAGE RATINGS *************

## colMeans: This is the average value for each column

average_ratings <- colMeans(MovieLense)

qplot(average_ratings) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

## Any Comments on above plot ?

## Remove the movies whose number of views is below a defined threshold
## Here we take threshold as 100 

average_ratings_relevant <- average_ratings[views_per_movie > 100]

qplot(average_ratings_relevant) +
  stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))

## Any Comments on above plot ? 

## *************** VISUALIZE THE MATRIX *************

## recommenderlab package redefined the method image for realRatingMatrix objects
## heat map whose colors represent the ratings

image(MovieLense, main = "Heatmap of the rating matrix")

## white area in the top-right region as row and columns are sorted. hard to read
## build another chart zooming in on the first rows and columns

image(MovieLense[1:10, 1:15],
      main = "Heatmap of the first rows and columns")

## Some users saw more movies than the others
## What if we want to see heat map for avid viewers and most viewed movies
## Steps to follow 
## a. Determine the minimum number of movies per user
## b. Determine the minimum number of users per movie
## c. Select the users and movies matching these criteria

##  visualize the top percentile of users and movies
min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)
min_n_movies
min_n_users

## visualize the rows and columns matching the criteria
image(MovieLense[rowCounts(MovieLense) > min_n_movies,
                 colCounts(MovieLense) > min_n_users],
      main = "Heatmap of the top users and movies")

## Most of them have seen all the top movies
## Some columns that are darker than the others
## These columns represent the highest-rated movies
## Conversely, darker rows represent users giving higher ratings
## There is need to normalize the data 
