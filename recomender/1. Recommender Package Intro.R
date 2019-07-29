
## ***************** recommenderlab PACKAGE INTRO ************************

## install recommenderlab if not done so 
if(!"recommenderlab" %in% rownames(installed.packages())){
  install.packages("recommenderlab")
}

##  load the package
library("recommenderlab")
##  Read the documentation if needed
## help(package = "recommenderlab")

## recommenderlab contains some datasets 
data_package <- data(package = "recommenderlab")
data_package$results[, "Item"]

## Here we use the MovieLense Dataset 
## Data is about movies. The table contains the ratings that the users give to movies
data(MovieLense)
MovieLense

## 943 x 1664 rating matrix of class 'realRatingMatrix' with 99392 ratings
## Each row of MovieLense corresponds to a user, and each column corresponds to a movie 
## There are more than 943 x 1664 = 1,500,000 combinations between a user and a movie 
## Therefore, storing the complete matrix would require more than 1,500,000 cells 
## However, not every user has watched every movie 
## Therefore, there are fewer than 100,000 ratings, and the matrix is sparse
## The recommenderlab package allows us to store it in a compact way

## Explore MovieLense
class(MovieLense)

##The realRatingMatrix class is defined by recommenderlab 
## and ojectsojectsb contains sparse rating matrices 
## View the methods that we can apply on the objects of this class

methods(class = class(MovieLense))

## Example dim to extract the number of rows and columns
## colSums to compute the sum of each column

## Let us compare the matrix size of realRatingMatrix with corresponding R matrix 
object.size(MovieLense)
object.size(as(MovieLense, "matrix"))

## compute how many times the recommenderlab matrix is more compact
object.size(as(MovieLense, "matrix")) / object.size(MovieLense)

## Any Comments for above ? 

## recommenderlab contains the similarity function
## The supported methods to compute similarities are cosine, pearson, and jaccard

## Calculate similarity of first four users using cosine method
similarity_users <- similarity(MovieLense[1:4, ], method = "cosine", which = "users")

## Explore the similarity_users
class(similarity_users)

## We can use hclust to build a hierarchic clustering model as class is distance
## Similarily different way we can use the distance object

## convert similarity_users into a matrix
as.matrix(similarity_users)

## Using image, we can visualize the matrix 
## Each row and each column corresponds to a user
## and each cell corresponds to the similarity between two users

image(as.matrix(similarity_users), main = "User similarity")

## more red the cell is, the more similar two users are 
## Note that the diagonal is red, since it's comparing each user with itself

## Calculate similarity of first four items using cosine method
similarity_items <- similarity(MovieLense[, 1:4], method = "cosine", which = "items")

as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")

## The recommenderlab package contains some options for the recommendation algorithm
## Display the model applicable to the realRatingMatrix objects using recommenderRegistry$get_entries

recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

## Let us view the models 
names(recommender_models)

## look at their descriptions
lapply(recommender_models, "[[", "description")

## Here we will use IBCF & UBCF 

## Information about parameters 
## recommender_models$IBCF_realRatingMatrix$parameters
