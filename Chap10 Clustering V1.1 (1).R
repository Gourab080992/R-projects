options(scipen=999)
options(digits=3)

###############################################################################

#Cluster Analysis

#Cluster analysis or clustering is the task of grouping a set of objects
#in such a way that objects in the same group (called a cluster) are more 
#similar (in some sense or another) to each other than to those in 
#other groups (clusters).

#It is a unsupervised machine learning techique. Cluster analysis is a 
#task that can be accomplished in many different clustering algorithms

#In this session We shall look at three types of clustering
#1# k-means clustering
#2# hierarchical clustering
#3# DBSCAN
#4# PAM

#Stages in performing a cluster analysis
#Stage I     : Data Prepartion
#Stage II    : Executing the clustering Algorithm
#Stage III   : Evaluating the clusters

#############################################################################

#Data Source : Abreu, N. (2011). Analise do perfil do cliente Recheio e desenvolvimento de 
#um sistema promocional. Mestrado em Marketing, ISCTE-IUL, Lisbon 

#URL : https://archive.ics.uci.edu/ml/datasets/Wholesale+customers

#Attribute Information
#1)	FRESH: annual spending (m.u.) on fresh products (Continuous); 
#2)	MILK: annual spending (m.u.) on milk products (Continuous); 
#3)	GROCERY: annual spending (m.u.)on grocery products (Continuous); 
#4)	FROZEN: annual spending (m.u.)on frozen products (Continuous) 
#5)	DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous) 
#6)	DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous); 
#7)	CHANNEL: customers‚???T Channel - Horeca (1- Hotel/Restaurant/Caf√©) or (2-Retail) channel (Nominal) 
#8)	REGION: customers‚???T Region ‚???" (1-Lisnon, 2-Oporto or 3-Other) (Nominal) 

####################################################################

#Basics
#We would need the following Packages and Libraries
#Package installation
install.packages("psych")
install.packages("fpc")
library(psych)
library(fpc)
library(cluster)

####################################################################

#Stage I : Data Preparation

#Step IA : Convert Factor into Dummy Codes
#First run str command to identify factor variables

#Dataset Used : telecomclus
data <- Wholesale.customers.data
str(data)
View(data)

#Factor Variable : age, gender, devicetype, locality, region
data$Channel <- as.factor(data$Channel)
data$Region  <- as.factor(data$Region)

#Dummycoding Factor Variables
dummychannel          <- as.data.frame(dummy.code(data$Channel))
dummyregion           <- as.data.frame(dummy.code(data$Region))
names(dummychannel)   <- c('hotel','retail')
names(dummyregion)    <- c('Lisnon','Oporto','Others')
dummy                 <- data.frame(dummychannel,dummyregion)

#Merge dummy columns with dataframe "data"
rm(clusdata)
clusdatabase <- data.frame(data,dummy)
str(clusdatabase)
View(clusdatabase) 

#Selecting the variables for clusteranalysis
#Identify numeric or integer variables from clusdata
#Easy way to choose our variables instead of typing them one by one

vars.to.use <- NULL
for (Var in names(clusdatabase)) {
  if(class(clusdatabase[,Var]) == 'integer' | class(clusdatabase[,Var]) == 'numeric') 
    {
    vars.to.use <- c(vars.to.use,Var)
    }
}
#Remove "id" field
vars.to.use <- vars.to.use[-1]

####################################################################

#Step IB : Data SCaling and Centraling

#Note that the different attributes have widely varying magnitudes 
#Attributes of widely varying magnitudes meaning they have different scales
#Sometimes such scales distort the results
#Handle such attributes by a concept called scaling and centraling
#What we acheive as a result of scaling and centraling is transform the
#Varibles such that their mean is zero and variance is one.

#With Scaling and Centering
clusdata <-scale(clusdatabase[,vars.to.use])
View(clusdata)
View(sapply(describe(clusdata),round,2))

####################################################################
#Stage II : Execute the Clustering Algorithm

#Cluster Analysis using k-means#

#In k-means, you have to indicate the number of clusters to kmeans function

kmeans.c0 = 3 #set kmeans.c0 = 3 to get a solution with 3 clusters
kmeans.c1 = 4 #set kmeans.c1 = 4 to get a solution with 4 clusters
kmeans.c2 = 5 #set kmeans.c2 = 5 to get a solution with 5 clusters

#Execute and store cluster solution
kmeans_a <- kmeans(clusdata,kmeans.c0,nstart=100,iter.max=100)
kmeans_b <- kmeans(clusdata,kmeans.c1,nstart=100,iter.max=100)
kmeans_c <- kmeans(clusdata,kmeans.c2,nstart=100,iter.max=100)

#Extract the cluster label from each cluster solution
groups_a  = kmeans_a$cluster
groups_b  = kmeans_b$cluster
groups_c  = kmeans_c$cluster

#Attach the labels solution-wise to the dataset 
clusdatabase <- data.frame(clusdatabase,groups_a,groups_b,groups_c)
clusdatabase$groups_a <- as.factor(clusdatabase$groups_a)
clusdatabase$groups_b <- as.factor(clusdatabase$groups_b)
clusdatabase$groups_c <- as.factor(clusdatabase$groups_c)

#Now Extract and understanding Cluster Centers

clustercenters_an <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$groups_a),mean,na.rm=TRUE))
clustercenters_af <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$groups_a),sum,na.rm=TRUE))
clustercenters_a  <- as.data.frame(rbind(clustercenters_an[-1,],clustercenters_af[-1,]))
names(clustercenters_a) <- c("groupa_c1","groupa_c2","groupa_c3")

clustercenters_bn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$groups_b),mean,na.rm=TRUE))
clustercenters_bf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$groups_b),sum,na.rm=TRUE))
clustercenters_b  <- as.data.frame(rbind(clustercenters_bn[-1,],clustercenters_bf[-1,]))
names(clustercenters_b) <- c("groupb_c1","groupb_c2","groupb_c3","groupb_c4")

clustercenters_cn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$groups_c),mean,na.rm=TRUE))
clustercenters_cf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$groups_c),sum,na.rm=TRUE))
clustercenters_c  <- as.data.frame(rbind(clustercenters_cn[-1,],clustercenters_cf[-1,]))
names(clustercenters_c) <- c("groupc_c1","groupc_c2","groupc_c3","groupc_c4","groupc_c5")

View(clustercenters_a) #3 cluster solution
View(clustercenters_b) #4 cluster solution
View(clustercenters_c) #5 cluster solution

#Infer the cluster characteristics by viewing the cluster solutions

########################################################################

#Stage III : K-means Evaluation#

#While visual aids provide a basic understanding, its difficult
#to arrive at a conclusion. There are some statistical tests that will
#help us in the evaluation of cluster solutions

#Method I : Calinski-harabasz index
kmeans_ch<-kmeansruns(clusdata,krange=1:5,criterion="ch")
kmeans_ch$bestk #Indicates Best Cluster Size
kmeans_ch$crit  #CH-Index
#Calinski-harabasz index : the higher the better : (BSS/(k-1))/(WSS(k)/(n-k))
#It is roughly the ratio of between sum of squares (BSS) to within sum of squares
#BSS indicates how different two clusters are.
#WSS indicates how different is a cluster internally.
#Naturally, we would like to maximize this ratio

#Method II : ASW
kmeans_asw<-kmeansruns(clusdata,krange=1:5,criterion="asw")
kmeans_asw$bestk  #Indicates Best Cluster Size
kmeans_asw$crit   #Indicates ASW Value
#Average Silhoutee Width : (dissimilarity to neighbouring cluster - dissimilarity to the cluster / max of numerator)
#ASW is between +1 and -1, higher the better

#Method III: Screeplot
wss <- (nrow(clusdata)-1)*sum(apply(clusdata,2,var));
for (i in 2:20) wss[i] <- sum(kmeans(clusdata,centers=i)$withinss);
options(jupyter.plot_mimetypes = "image/svg+xml");
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#Look got a characteristic elbow in the plot.

#Reconcile the cluster evaluation results with
#What you have inferred from looking at cluster centres
#Recommendation : Difficult to make up mind at this point.
#Note : above are only heuristics. They are not a replacement for your judgement


#########################################################################
#Stage II : Execute Clustering Algorithm : Hierarchical Clustering#

#Execution#

#Basics#
#Key concept - (1) Hierarchical Clustering Uses Different types of Distances
#              (2) Agglomeration method

#Primary distances are    
#Note distance methods are between members
    #euclidean Distance (direct line distance)
    #manhattan Distance (taxi cab distance)
    #maximum Distance   (largest single attribute distance)

#Primary Agglomeration methods are
#Agglomeration methods are between clusters
    #ward.D   (Variance method: minimum increase in variance)
    #average  (Linkage method : distance between 2 clusters is the average distance between members) 
    #mcquitty (Linkage method :distance between 2 clusters is the weighted average distance between members)
    #complete (Linkage method : distance between 2 cluster is the farthest distance between their members)

#Variance methods focus on minimizing within cluster variance
#In general Ward and average methods have shown better results compared to other methods

#Execution

#Step I   : Calculate Distance Matrix
#Step II  : Execute Clustering using an Agglomeration Method
#Step III : Extract groups (clusters)

#Step I   : Calculate Distance Matrix

distance_euclid<-dist(clusdata, method="euclidean")
distance_manhat<-dist(clusdata, method="manhattan")
distance_maximum<-dist(clusdata, method="maximum")

#Step II   : Execute Clustering using an Agglomeration Method
#There are 12 combinations of distance and agglomeration methods

#Euclidean and ward.D, average, mcquitty, complete
hier_a<-hclust(distance_euclid,method="ward.D")    #Case 1
hier_b<-hclust(distance_euclid,method="average")   #Case 2
hier_c<-hclust(distance_euclid,method="complete")  #Case 3
hier_d<-hclust(distance_euclid,method="mcquitty")  #Case 4

#Manhattan and ward.D, average, mcquitty, complete
hier_e<-hclust(distance_manhat,method="ward.D")    #Case 5
hier_f<-hclust(distance_manhat,method="average")   #Case 6
hier_g<-hclust(distance_manhat,method="complete")  #Case 7
hier_h<-hclust(distance_manhat,method="mcquitty")  #Case 8

#maximum and ward.D, average, mcquitty, complete
hier_i<-hclust(distance_maximum,method="ward.D")    #Case 9
hier_j<-hclust(distance_maximum,method="average")   #Case 10
hier_k<-hclust(distance_maximum,method="complete")  #Case 11
hier_l<-hclust(distance_maximum,method="mcquitty")  #Case 12

#Step III : Extract groups (clusters)
#Unlike k-means, in hierarchical clusters of various size can be extracted from the cluster object

group_a3 <- cutree(hier_a,k=3) #Extract 3 Clusters
group_a4 <- cutree(hier_a,k=4) #Extract 4 Clusters

group_b3 <- cutree(hier_b,k=3) #Extract 3 Clusters
group_b4 <- cutree(hier_b,k=5) #Extract 4 Clusters

group_c3 <- cutree(hier_c,k=3) #Extract 3 Clusters
group_c4 <- cutree(hier_c,k=4) #Extract 4 Clusters

group_d3 <- cutree(hier_d,k=3) #Extract 3 Clusters
group_d4 <- cutree(hier_d,k=4) #Extract 4 Clusters

group_e3 <- cutree(hier_e,k=3) #Extract 3 Clusters
group_e4 <- cutree(hier_e,k=4) #Extract 4 Clusters

group_f3 <- cutree(hier_f,k=3) #Extract 3 Clusters
group_f4 <- cutree(hier_f,k=4) #Extract 4 Clusters

group_g3 <- cutree(hier_g,k=3) #Extract 3 Clusters
group_g4 <- cutree(hier_g,k=4) #Extract 4 Clusters

group_h3 <- cutree(hier_h,k=3) #Extract 3 Clusters
group_h4 <- cutree(hier_h,k=4) #Extract 4 Clusters

group_i3 <- cutree(hier_i,k=3) #Extract 3 Clusters
group_i4 <- cutree(hier_i,k=4) #Extract 4 Clusters

group_j3 <- cutree(hier_j,k=3) #Extract 3 Clusters
group_j4 <- cutree(hier_j,k=4) #Extract 4 Clusters

group_k3 <- cutree(hier_k,k=3) #Extract 3 Clusters
group_k4 <- cutree(hier_k,k=4) #Extract 4 Clusters

group_l3 <- cutree(hier_l,k=3) #Extract 3 Clusters
group_l4 <- cutree(hier_l,k=4) #Extract 4 Clusters

##############################################################################
#Stage III : Evaluation of Hierarchical Clusters#

#Evaluation
  #Step I   : Shortlist
  #Step II  : Execute ClusterBoot
  #Step III : Investigate Dissolution and Similarity Parameters

#Evaluation Step I   : Shortlist

#If possible, narrow down the choices from 24 groups above
#Apply thumb rules 
#Size of largest cluster is not more than 4-5 times the smallest cluster

table(group_a3) #looks ok
table(group_a4) #looks ok
table(group_b3) #looks nok
table(group_b4) #looks nok
table(group_c3) #looks nok
table(group_c4) #looks nok
table(group_d3) #looks nok
table(group_d4) #looks nok
table(group_e3) #looks ok
table(group_e4) #looks nok
table(group_f3) #looks nok
table(group_f4) #looks nok
table(group_g3) #looks nok
table(group_g4) #looks nok
table(group_h3) #looks nok
table(group_h4) #looks nok
table(group_i3) #looks ok
table(group_i4) #looks ok
table(group_j3) #looks nok
table(group_j4) #looks nok
table(group_k3) #looks nok
table(group_k4) #looks nok
table(group_l3) #looks nok
table(group_l4) #looks nok

#Trees a3,a4,e3,i3,i4
#Looking at cluster size is not enough
#On second thoughts check e4 as well..it was close
#We have to look at cluster stability as well
#For this we proceed to step II below

#Step II  : Execute ClusterBoot
#Let us evaluate the luster solutions

Eval_groupa3<-clusterboot(distance_euclid,clustermethod=hclustCBI,method="ward.D",k=3)
Eval_groupa4<-clusterboot(distance_euclid,clustermethod=hclustCBI,method="average",k=4)

Eval_groupe3<-clusterboot(distance_manhat,clustermethod=hclustCBI,method="ward.D",k=3)
Eval_groupe4<-clusterboot(distance_manhat,clustermethod=hclustCBI,method="ward.D",k=4)

Eval_groupi3<-clusterboot(distance_maximum,clustermethod=hclustCBI,method="ward.D",k=3)
Eval_groupi4<-clusterboot(distance_maximum,clustermethod=hclustCBI,method="ward.D",k=4)

#Step III  : Investigate Dissolution and Similarity Parameters

#Look for Jaccard Coefficient using $bootmean option
#Look for how many times a cluster is dissolved using $bootbrd

#Let us accumalate the Jaccard Coeffient and Dissolution Indicator
#For a good clustering solution
#Jaccard Coefficient must be as high as possible
#Dissolution indicator must be as low as possible

hierbootmean <- c(Eval_groupa3$bootmean,Eval_groupa4$bootmean,
                  Eval_groupe3$bootmean,Eval_groupe4$bootmean,
                  Eval_groupi3$bootmean,Eval_groupi4$bootmean)

hierbootbrd <- c(Eval_groupa3$bootbrd,Eval_groupa4$bootbrd,
                 Eval_groupe3$bootbrd,Eval_groupe4$bootbrd,
                 Eval_groupi3$bootbrd,Eval_groupi4$bootbrd)

hiercase    <- c("a3","a3","a3","a4","a4","a4",
                 "a4","e3","e3","e3","e4","e4",
                 "e4","e4","i3","i3","i3","i4",
                 "i4","i4","i4")

hiercluster <- c(1,2,3,1,2,3,4,1,2,3,1,2,3,4,1,2,3,1,2,3,4)

rm(hiersummary)
hiersummary <- data.frame('case'=hiercase,'cluster'=hiercluster,'bootmean'=hierbootmean,'bootbrd'=hierbootbrd)
View(hiersummary)
View(subset(hiersummary,bootmean>=0.85 & bootbrd <=15))

#Cluster Solutions a3, e3, e4 look stable

#Inference
#Let us move solutions a3, e3, e4 to telecomclusters

#Attach the labels solution-wise to the dataset 
clusdatabase <- data.frame(clusdatabase,group_a3,group_e3,group_e4)
clusdatabase$group_a3 <- as.factor(clusdatabase$group_a3)
clusdatabase$group_e3 <- as.factor(clusdatabase$group_e3)
clusdatabase$group_e4 <- as.factor(clusdatabase$group_e4)

#Now Extract and understanding Cluster Centers

clustercenters_gan <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$group_a3),mean,na.rm=TRUE))
clustercenters_gaf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$group_a3),sum,na.rm=TRUE))
clustercenters_ga  <- as.data.frame(rbind(clustercenters_gan[-1,],clustercenters_gaf[-1,]))
names(clustercenters_ga) <- c("groupa3_c1","groupa3_c2","groupa3_c3")

clustercenters_gbn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$group_e3),mean,na.rm=TRUE))
clustercenters_gbf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$group_e3),sum,na.rm=TRUE))
clustercenters_gb  <- as.data.frame(rbind(clustercenters_gbn[-1,],clustercenters_gbf[-1,]))
names(clustercenters_gb) <- c("groupe3_c1","groupe3_c2","groupe3_c3")

clustercenters_gcn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$group_e4),mean,na.rm=TRUE))
clustercenters_gcf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$group_e4),sum,na.rm=TRUE))
clustercenters_gc  <- as.data.frame(rbind(clustercenters_gcn[-1,],clustercenters_gcf[-1,]))
names(clustercenters_gc) <- c("groupe4_c1","groupe4_c2","groupe4_c3","groupe4_c4")

View(clustercenters_ga) #3 cluster solution
View(clustercenters_gb) #3 cluster solution
View(clustercenters_gc) #4 cluster solution

#Infer the cluster characteristics by viewing the cluster solutions

####################################################################

#Clustering Algorithm : DBSCAN #

#Density-based spatial clustering of applications with noise (DBSCAN)#

#Execution#
#Specification of eps and MinPts is a challenge in DBSCAN#
#Start with eps=1 and keep running trials
#Once you find a reasonable set of clusters, then keep varying MinPts

scan_a <- dbscan(distance_euclid,method="dist",eps=3,MinPts=50,scale=FALSE,showplot=1)
table(scan_a$cluster)

scan_b <- dbscan(distance_manhat,method="dist",eps=3,MinPts=50,scale=FALSE,showplot=1)
table(scan_b$cluster)

scan_c <- dbscan(distance_maximum,method="dist",eps=2,MinPts=50,scale=FALSE,showplot=1)
table(scan_c$cluster)

#Lets add the three solutions to dataset
#Let us move solutions a3, e3, e4 to telecomclusters

#Attach the labels solution-wise to the dataset 
clusdatabase <- data.frame(clusdatabase,"scan_a"=scan_a$cluster,"scan_b"=scan_b$cluster,"scan_c"=scan_c$cluster)
clusdatabase$scan_a <- as.factor(clusdatabase$scan_a)
clusdatabase$scan_b <- as.factor(clusdatabase$scan_b)
clusdatabase$scan_c <- as.factor(clusdatabase$scan_c)

#Now Extract and understanding Cluster Centers

clustercenters_dan <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$scan_a),mean,na.rm=TRUE))
clustercenters_daf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$scan_a),sum,na.rm=TRUE))
clustercenters_da  <- as.data.frame(rbind(clustercenters_dan[-1,],clustercenters_daf[-1,]))
names(clustercenters_da) <- c("scana_c1","scana_c2","scana_c3","scana_c4")

clustercenters_dbn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$scan_b),mean,na.rm=TRUE))
clustercenters_dbf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$scan_b),sum,na.rm=TRUE))
clustercenters_db  <- as.data.frame(rbind(clustercenters_dbn[-1,],clustercenters_dbf[-1,]))
names(clustercenters_db) <- c("scanb_c1","scanb_c2","scanb_c3","scanb_c4")

clustercenters_dcn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$scan_c),mean,na.rm=TRUE))
clustercenters_dcf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$scan_c),sum,na.rm=TRUE))
clustercenters_dc  <- as.data.frame(rbind(clustercenters_dcn[-1,],clustercenters_dcf[-1,]))
names(clustercenters_dc) <- c("scanc_c1","scanc_c2","scanc_c3","scanc_c4")

View(clustercenters_da) #4 cluster solution
View(clustercenters_db) #4 cluster solution
View(clustercenters_dc) #4 cluster solution

###################################################################################

###4 PAM###

###Note k-means cannot take distance metric...It operates only on data ###
###k-means operates on euclidean distance ###
###Euclidean may not be quite useful when you have many categorical variables ###
###Such cases could be handled by Gower Distance###

###Let us first create Gower Distance Matrix for our dataset###

#Step 1: Let us move the data into a new dataframe called gower data
gowerdata <- data
str(gowerdata)

#Step 2: Calculate Gower Distance Matrix for our data
#Daisy is a advanced version of k-means
#Standardisation is taken care of internally

gowerdistance <- daisy(gowerdata,metric="gower",stand=TRUE)

#Step 3: Use pam to generate clusters
gower_a <- pam(gowerdistance,k=3) #3 centres
gower_b <- pam(gowerdistance,k=4) #4 centres
gower_c <- pam(gowerdistance,k=5) #5 centres

groups_ga <- gower_a$clustering
groups_gb <- gower_b$clustering
groups_gc <- gower_c$clustering

#Attach the labels solution-wise to the dataset 
clusdatabase <- data.frame(clusdatabase,groups_ga,groups_gb,groups_gc)
clusdatabase$groups_ga <- as.factor(clusdatabase$groups_ga)
clusdatabase$groups_gb <- as.factor(clusdatabase$groups_gb)
clusdatabase$groups_gc <- as.factor(clusdatabase$groups_gc)

#Now Extract and understanding Cluster Centers

clustercenters_zan <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$groups_ga),mean,na.rm=TRUE))
clustercenters_zaf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$groups_ga),sum,na.rm=TRUE))
clustercenters_za  <- as.data.frame(rbind(clustercenters_zan[-1,],clustercenters_zaf[-1,]))
names(clustercenters_za) <- c("gowera_c1","gowera_c2","gowera_c3")

clustercenters_zbn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$groups_gb),mean,na.rm=TRUE))
clustercenters_zbf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$groups_gb),sum,na.rm=TRUE))
clustercenters_zb  <- as.data.frame(rbind(clustercenters_zbn[-1,],clustercenters_zbf[-1,]))
names(clustercenters_zb) <- c("gowerb_c1","gowerb_c2","gowerb_c3","gowerb_c4")

clustercenters_zcn <- t(aggregate(clusdatabase[,3:8],by=list(clusdatabase$groups_gc),mean,na.rm=TRUE))
clustercenters_zcf <- t(aggregate(clusdatabase[,9:13],by=list(clusdatabase$groups_gc),sum,na.rm=TRUE))
clustercenters_zc  <- as.data.frame(rbind(clustercenters_zcn[-1,],clustercenters_zcf[-1,]))
names(clustercenters_zc) <- c("gowerb_c1","gowerb_c2","gowerb_c3","gowerb_c4","gowerb_c5")

View(clustercenters_za) #3 cluster solution
View(clustercenters_zb) #4 cluster solution
View(clustercenters_zc) #5 cluster solution

#######################################################################

#Decide the solution by studying the outputs of various hierarchical clustering
