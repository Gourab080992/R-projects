## ************* USER-BASED COLLABORATIVE FILTERING UBCF ************

## ******** Building the recommendation model


library("recommenderlab")
library("ggplot2")
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies_norm <- normalize(ratings_movies)

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_movies),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters

## Relevant parameters are method: This shows how to compute the similarity between users
## nn: This shows the number of similar users

## recommender model leaving the parameters to their defaults
recc_model <- Recommender(data = recc_data_train,
                          method = "UBCF")
recc_model

## extract some details about the model using getModel
model_details <- getModel(recc_model)

## look at the components of the mode
names(model_details)

## Apart from the description and parameters of model, model_details contains a data slot
model_details$data

## The model_details$data object contains the rating matrix
## The reason is that UBCF is a lazy-learning technique
## which means that it needs to access all the data to perform a prediction

## Determine the top six recommendations for each new user

n_recommended <- 6
recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)
recc_predicted

## Recommendations as 'topNList' with n = 6 for X users

## define a matrix with the recommendations to the test set users

recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)

##  look at the first four users
recc_matrix[, 1:4]

## compute how many times each movie got recommended and build the related frequency histogram
number_of_items <- factor(table(recc_matrix))
chart_title <- "Distribution of the number of items for UBCF"
qplot(number_of_items) + ggtitle(chart_title)

## Compared with the IBCF the distribution has a longer tail
## This means that there are some movies that are 
## recommended much more often than the others
## Compare the maximum with IBCF

## look at the top titles
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(
  names(number_of_items_top),
  number_of_items_top)

table_top

## Comparing the results of UBCF with IBCF helps in understanding the algorithm better
## UBCF needs to access the initial data, so it is a lazy-learning model
## Since it needs to keep the entire database in memory
## it doesn't work well in the presence of a big rating matrix
## Also building the similarity matrix requires a lot of computing power and time
## However UBCF's accuracy is proven to be slightly more accurate than IBCF
## so it's a good option if the dataset is not too big
