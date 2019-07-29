## *************** DATA PREPARATION ***************************

library("recommenderlab")
library("ggplot2")
data(MovieLense)

## a. Select the relevant data
## b. Normalize the data

## **** SELECT THE RELEVANT DATA ********
## During Data Exploration Observations Were 
## Some Movies have been viewed only a few times, their ratings might be biased because of 
## lack of data. Users who rated only a few movies, their ratings might be biased

## Need to determine the minimum number of users per movie and vice versa
## The correct solution comes from an iteration of the entire process of preparing the data,
## building a recommendation model, and validating it
## For first time use a rule of thumb. After building model come back & modify data prep

## ratings_movies contains users who have rated at least 50 movies
## and Movies that have been watched at least 100 times

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies

## Any Comments on above output as compared to original matrix data size 

## **** EXPLORE THE RELEVANT DATA ********

## Visualize the top 2 percent of users and movies in the new matrix

min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)
image(ratings_movies[rowCounts(ratings_movies) > min_movies,
                     colCounts(ratings_movies) > min_users],
      main = "Heatmap of the top users and movies")

## Any Comments on above output


## distribution of the average rating by user 

average_ratings_per_user <- rowMeans(ratings_movies)

## Visualize the distribution 

qplot(average_ratings_per_user) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

## Any Comments ? 

## **** NORMALIZE THE DATA ********

## Users who give high (or low) ratings to all their movies might bias the results
## To remove this effect normalize the data to make average rating of each user is 0 

## prebuilt function normalie does that 

ratings_movies_norm <- normalize(ratings_movies)

## Check now the average ratings by users 

sum(rowMeans(ratings_movies_norm) > 0.00001)

# visualize the normalised matrix
image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies,
                          colCounts(ratings_movies_norm) > min_users],
      main = "Heatmap of the top users and movies")

## Any Comments ? 

## ********* CONVERTING DATA TO BINARY *************

## Some recommendation models work on binary data so we might want to binarize our data
## Task is to define a table containing only 0s and 1s
## The 0s will be either treated as missing values or as bad ratings

## Two options depending on a context you can choose 
## a. Define a matrix having 1 if the user rated the movie & 0 otherwise
## we are losing the information about the rating
## b. Define a matrix having 1 if the rating is >= definite threshold (e.g. 3) & 0 otherwise
## giving a bad rating to a movie is equivalent to not having rated it

## function to binarize the data is binarize
ratings_movies_watched <- binarize(ratings_movies, minRating = 1)

## Now we will have black and white chart for visualization so let us see 5% of users & movie

min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)
image(ratings_movies_watched[rowCounts(ratings_movies) >
                               min_movies_binary,
                             colCounts(ratings_movies) >
                               min_users_binary],
      main = "Heatmap of the top users and movies")

## Any comments ??

## Use the second approach to binarize based on threshold  of let say = 3

ratings_movies_good <- binarize(ratings_movies, minRating = 3)
image(ratings_movies_good[rowCounts(ratings_movies) >
                            min_movies_binary,
                          colCounts(ratings_movies) >
                            min_users_binary],
      main = "Heatmap of the top users and movies")

## Any Comments ? 

