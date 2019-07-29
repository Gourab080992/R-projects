
## **********  BUILDING COLLABORATIVE FILTERING MODELS ********

## ********** FIRST IBCF ******************* Look class notes for algorithm steps 

## From MovieLense Data Set We Define 
## Training set: This set includes users from which the model learns
## Test set: This set includes users to whom we recommend movies

library("recommenderlab")
library("ggplot2")
data(MovieLense)

## ********* DEFINE THE TRAINING & TEST SETS *********

## ratings_movies in the previous step as the subset of MovieLense users
## who have rated at least 50 movies & movies that have been rated at least 100 times

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies_norm <- normalize(ratings_movies)

## Define the which_train vector that is TRUE for users in the training set and FALSE for the others
## Set the probability in the training set as 80 %
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_movies),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
head(which_train)

## Define the training and the test sets
recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

## To recommend items to each user we could just use the k-fold
## a. Split the users randomly into five groups
## b. Use a group as a test set and the other groups as training sets
## c. Repeat it for each group

## which_set <- sample(x = 1:5,
##                       size = nrow(ratings_movies),
##                       replace = TRUE)
## for(i_model in 1:5) {
##   which_train <- which_set == i_model
##  recc_data_train <- ratings_movies[which_train, ]
##   recc_data_test <- ratings_movies[!which_train, ]
##   # build the recommender
## }

## To split the data into training and test sets automatically use evaluationScheme function

## ********* BUILD THE RECOMMENDATION MODEL *********

## Model is IBCF and Function to build model is recommender 
## Inputs are 
## a. Data: This is the training set
## b. Method: This is the name of the technique
## c.Parameters: These are some optional parameters of the technique

##  look at IBCF parameters
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters

## relevant parameters are as follows:
## k: In the first step, the algorithm computes the similarities among each pair of items
## Then, for each item, it identifies its k most similar items and stores it
## method: This is the similarity function. Default Cosine. Another option is pearson 

## Using Default model 
recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(k = 30))

## Recommender of type 'IBCF' for 'realRatingMatrix' ## learned using some users
recc_model
class(recc_model)

## recc_model class is an object of the Recommender class containing the model

## ******* EXPLORING THE RECOMMENDER MODEL *************

## Using getModel extract some details about the model e.g. description & parameters
model_details <- getModel(recc_model)
model_details$description
model_details$k

## The model_details$sim component contains the similarity matrix
## Check its structure

class(model_details$sim)
dim(model_details$sim)

## Any Comments ? 

## ------ model_details$sim is a square matrix whose size is equal to the number of items

## Explore further using part of it 
n_items_top <- 20
image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")

## Any Comments ? 

## --- Most of the values are equal to 0. The reason is that each row contains only k elements

## Check further 
model_details$k
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)

## Each row has 30 elements greater than 0
## However, the matrix is not supposed to be symmetric
## In fact, the number of non-null elements for each column 
## depends on how many times the corresponding movie was included in the top k of another movie

## Check the distribution of the number of elements by column

col_sums <- colSums(model_details$sim > 0)

## Plot the distribution chart  

qplot(col_sums) +
  stat_bin(binwidth = 1) +
  ggtitle("Distribution of the column count")

## Any Comments ? 


## see which are the movies similar with the most elements
which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]

## ********** APPLYING THE RECOMMENDER MODEL ON THE TEST SET **********

## Now we can recommend movies to the users in the test set
## Define n_recommended that specifies the number of items to recommend to each user
## Here shown is most popular approach using a weighted sum

n_recommended <- 6

## For each user, the algorithm extracts its rated movies
## For each movie, it identifies all its similar items, starting from the similarity matrix
## Then the algorithm ranks each similar item in this way
## Extract the user rating of each purchase associated with this item. Rating is used as weight
## Extract the similarity of the item with each purchase associated with this item
## Multiply each weight with the related similarity
## Sum everything up

## algorithm identifies the top n recommendations

recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)
recc_predicted

## recc_predicted object contains the recommendations
## Look at its structure 
class(recc_predicted)
slotNames(recc_predicted)

## Slots are items: This is the list with the indices of the recommended items for each user
## itemLabels: This is the name of the items
## n: This is the number of recommendations

## Recommendation for first user
recc_predicted@items[[1]]

## Extract the recommended movies from recc_predicted@item labels
recc_user_1 <- recc_predicted@items[[1]]
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]
movies_user_1

## define a matrix with the recommendations for each user

recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)

## Visualize the recommendations for the first four users

recc_matrix[, 1:4]

## Now we can identify the most recommended movies

## For this purpose, we will define a vector with all the recommendations
## we will build a frequency plot
number_of_items <- factor(table(recc_matrix))
chart_title <- "Distribution of the number of items for IBCF"

## Plot the distribution
qplot(number_of_items) + ggtitle(chart_title)

## Any Comments ? 

## To see which are the most popular movies

number_of_items_sorted <- sort(number_of_items,
                               decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(
  names(number_of_items_top),
  number_of_items_top)

table_top

## IBCF recommends items on the basis of the similarity matrix
## For each item, the model stores the k-most similar
## so the amount of information is small once the model is built
## This is an advantage in the presence of lots of data
## In addition, this algorithm is efficient and scalable
## It works well with big rating matrices
## Its accuracy is rather good, compared with other recommendation models