library(cluster)

rest3 <- read.csv("Menu.csv")
rest3n <- rest3[1:123,] # Copy of the dataset

#### Data Processing: Part 1 ####
rest3n <- rest3n[-c(18,68,78,85,119:123),] # Remove Drinks 
rest3clean <- rest3n[,-c(1,9:12)] # Remove categorical variables
rest3clean <- na.omit(rest3clean) # Remove NA rows

# other steps can also be dobe, for example make a new column called duration for first time and last time appeared 
##### Part 2: HCLUST ####

dish4 <- scale(rest3clean[,-1]) # Scale the data


ddist1 <- dist(dish4[,-1], method="euclidean") # Distance matrix
ddist1

dhc <- hclust(ddist1) # Complete linkage 
plot(dhc, hang=-1, labels = rest3clean$name.x) # Output
clustCut3<-cutree(dhc, k=3)

table(clustCut3) # check membership number
rect.hclust(dhc, k=3, border= "red") # check for other Ks
rect.hclust(dhc, k=4, border= "green")
rect.hclust(dhc, k=5, border= "blue")
rect.hclust(dhc, k=6, border= "orange")


dhca <- hclust(ddist1, method = "average") # Average Linkge 
plot(dhca, hang=-1, label=rest3clean$name.x) # Output 


dhcs <- hclust(ddist1, method = "single") # Single Linkage 
plot(dhcs, hang=-1, label=rest3clean$name.x) # Output 

library(NbClust) # Optimal Number of HCLUSTERS
nb<- NbClust(rest3clean[,-1], distance = "euclidean", min.nc = 2,max.nc=10, method = "complete", index ="all")
library(factoextra)
fviz_nbclust(nb) # optimal number of clusters 


sild = plot(silhouette(cutree(dhc, k=5), ddist1)) # Cluster Membership, reference to silhouette width 




library(factoextra)
fviz_nbclust(nb)

#final choice depend on reference to members of clusters + range from Nbclust optimum 

#### Part 3: K-Means  ####
# justification for scaled 


set.seed(2)
kout3 <- kmeans(scale(rest3clean[,-1]),centers=3,nstart=10)
kout3 # characterize cluster using mean 
table(rest3clean$name.x, kout3$cluster) # number of members in each cluster
o=order(kout3$cluster)
o
data.frame(rest3clean$name.x[o], kout3$cluster[o])

set.seed(2)
kout5 <- kmeans(rest3clean[,-1],centers=5,nstart=10)
kout5

set.seed(2)
kout7 <- kmeans(rest3clean[,-1],centers=7,nstart=10)

kout7

set.seed(2)
kout9 <- kmeans(rest3clean[,-1],centers=9,nstart=10)
kout9

nb<- NbClust(rest3clean[,-1], distance = "euclidean", min.nc = 2,max.nc=12, method = "kmeans", index ="all") #choice of clusters
library(factoextra) 
library(ggplot2)
fviz_nbclust(nb) 

# Membership in each cluster produced: 

library(factoextra)
fviz_cluster(kout3, rest3clean[,-1] , geom = c("point","text"))

aggregate(scale(rest3clean[,-1]),list(kout3$cluster),mean) # Least dense cluster

# Characterize using cluster means, which 2 variables contributed most? this will vary. 
# for 3v select 2 variables for each cluster with highest mean

A <- ggplot(data=rest3clean[,-1], aes(highest_price,menus_appeared))
A+geom_point(aes(col=factor(kout3$cluster)))

B<-A+geom_point(aes(col=factor(kout3$cluster)),size=3)+
  geom_text(label=rownames(rest3clean[,-1]),size=2.5,vjust=-1) #adjust this code 
B #3 v

# 
PRICE.c <- tapply(rest3clean[,-1]$highest_price,kout3$cluster, mean)
MENU.c <- tapply(rest3clean[,-1]$menus_appeared,kout3$cluster, mean)
centers <- data.frame(PRICE.c, MENU.c)

centers
C<- B+geom_point(data=centers,aes(PRICE.c, MENU.c),color="purple",pch=8,size=8)+
  ggtitle("Clusters")
C  #3 Part b



