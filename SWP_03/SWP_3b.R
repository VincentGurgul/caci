
### SWP 3 Part B

library(data.table)
library(tidyverse)
library(magrittr)
library(ggpubr)
library(tibble)
library(MASS)
library(psych)
library(ggplot2)
library(MASS)

setwd("/Users/VincentGurgul/Documents/BWL/msc_03/caci/swp_03")

sum <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}


### Contents

# 1. Data Preparation
# 2. Hierarchical Clustering
# 3. K-Means Clustering
# 4. Gaussian Mixtures Modelling
# 5. DBSCAN Clustering
# 6. Comparing Results / Evaluation


### 1. Data preparation

b <- read.csv("indivData.csv")

b1 <- b[ , -c(27,29,30,32,34,36)] # removed non-numeric variables
b2 <- b[, -c(1, 4:23, 25:34, 36)] # reduced variables for MDS / clustering 
b3 <- b1[, -c(4:11, 17:21)] # reduced variables for meaningful comparison


## Multidimensional Scaling

dist <- dist(apply(b2, 2, scale)) 

fit <- cmdscale(dist, k = 2)

ggplot(as.data.frame(fit), aes(V1, V2)) +
  labs(title = "Metric MDS") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# it becomes clear that we want 3 main clusters


### 2. Hierarchical Clustering

## With hclust

library(cluster)

hc <- hclust(dist, method ="ward.D2")
plot(hc)

hc3 <- cutree(hc, k=3) # 3 clusters
hc4 <- cutree(hc, k=4) # 4 clusters

sum(b3, hc3)
sum(b3, hc4)

phc3 <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(hc3)), aes(V1, V2, color = cluster)) +
  labs(title = "Hierarchical Clustering with 3 Clusters") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

phc4 <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(hc4)), aes(V1, V2, color = cluster)) +
  labs(title = "Hierarchical Clustering with 4 Clusters") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggarrange(phc3, phc4)


## With eclust

library(factoextra)

ehc3 <- eclust(b2, "hclust", k=3)
pehc3 <- fviz_cluster(ehc3, 
             main = "Hierarchical Clustering with 3 Clusters", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)
sehc3 <- fviz_silhouette(ehc3)

ehc4 <- eclust(b2, "hclust", k=4)
pehc4 <- fviz_cluster(ehc4, 
             main = "Hierarchical Clustering with 4 Clusters", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)
sehc4 <- fviz_silhouette(ehc4)

ggarrange(pehc3, pehc4, sehc3, sehc4)

### 3. K-Means Clustering

#kma <- eclust(b2, "kmeans")
#fviz_gap_stat(kma$gap_stat)

## With kmeans

# with 3 clusters

set.seed(300)
km3 <- kmeans(b2 , centers =3)

pkm3 <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(km3$cluster)), aes(V1, V2, color = cluster)) +
  labs(title = "K-Means Clustering with 3 Clusters") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

sum(b3, km3$cluster)

# with 4 clusters

km4 <- kmeans(b2 , centers =4)

pkm4 <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(km4$cluster)), aes(V1, V2, color = cluster)) +
  labs(title = "K-Means Clustering with 4 Clusters") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

sum(b3, km4$cluster)

ggarrange(pkm3, pkm4)

## With eclust

ekm3 <- eclust(b2, "kmeans", k=3)
pekm3 <- fviz_cluster(ekm3, 
             main = "K-Means Clustering with 3 Clusters", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)
sekm3 <- fviz_silhouette(ekm3)

ekm4 <- eclust(b2, "kmeans", k=4)
pekm4 <- fviz_cluster(ekm4, 
             main = "K-Means Clustering with 4 Clusters", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)
sekm4 <- fviz_silhouette(ekm4)

ggarrange(pekm3, pekm4, sekm3, sekm4)


### 4. Gaussian Mixture Modelling

library(mclust)

# with 3 clusters 

gmm3 <- Mclust(b2 , G=3)

pgmm3 <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(gmm3$class)), aes(V1, V2, color = cluster)) +
  labs(title = "Gaussian Mixture Modelling with 3 Clusters") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

sum(b3, gmm3$class)

# with 4 clusters

gmm4 <- Mclust(b2 , G=4) 

pgmm4 <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(gmm4$class)), aes(V1, V2, color = cluster)) +
  labs(title = "Gaussian Mixture Modelling with 4 Clusters") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

sum(b3, gmm4$class)

ggarrange(pgmm3, pgmm4)


### 5. DBSCAN Clustering

library(dbscan)

# choose optimal eps
dbscan::kNNdistplot(fit, k =  5)
abline(h = 0.5, lty = 2)

set.seed(123)

# compute DBSCAN using dbscan package
db <- dbscan::dbscan(fit, 0.5, 3)

library(fpc)

# compute DBSCAN using fpc package
fpc.db <- fpc::dbscan(fit, eps = 0.5, MinPts = 3)

# make sure the results of both packages are identical
all(fpc.db$cluster == db$cluster)

print(db)

ggplot(as.data.frame(fit) %>% add_column(cluster = factor(db$cluster)), aes(V1, V2, color = cluster)) +
  labs(title = "DBSCAN with 3 Clusters and Outliers") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

fviz_cluster(db, 
             b2, 
             main = "DBSCAN with 3 Clusters and Outliers", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)

sum(b3, db$cluster)


### 6. Comparing Results / Evaluation

# graphical comparison with ggplot

phc <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(hc3)), aes(V1, V2, color = cluster)) +
  labs(title = "Hierarchical Clustering") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

pkm <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(km3$cluster)), aes(V1, V2, color = cluster)) +
  labs(title = "K-Means Clustering") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

pgmm <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(gmm3$class)), aes(V1, V2, color = cluster)) +
  labs(title = "Gaussian Mixture Modelling") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

pdb <- ggplot(as.data.frame(fit) %>% add_column(cluster = factor(db$cluster)), aes(V1, V2, color = cluster)) +
  labs(title = "DBSCAN") +
  geom_point() + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggarrange(phc, pkm, pgmm, pdb)

# graphical comparison with factoextra

pehc <- fviz_cluster(ehc3,
             main = "Hierarchical Clustering", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)

pekm <- fviz_cluster(km3, 
             b2,
             main = "K-Means Clustering", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)

pegmm <- fviz_cluster(gmm3, 
             b2, 
             main = "Gaussian Mixture Modelling", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)

pedb <- fviz_cluster(db, 
             b2, 
             main = "DBSCAN", 
             xlab = FALSE, 
             ylab = FALSE, 
             ellipse = FALSE, 
             pointsize = 2, 
             labelsize = 0, 
             show.clust.cent = FALSE)

ggarrange(pehc, pekm, pegmm, pedb)

# numerical comparison

sum(b3, hc3)
sum(b3, hc4)

sum(b3, km3$cluster)
sum(b3, km4$cluster)

sum(b3, gmm3$class)
sum(b3, gmm4$class)

sum(b3, db$cluster) # DBSCAN: best result and therefore only important one for the analysis!

# Clusters:

# 0: Ignorant stingy billionaires (aka Outliers) -- 6 members, highest income, very little knowledge, mostly don't own, high importance of price
# 1: Scrimpers -- 251 members, mostly don't own and don't want to buy, overall average, little knowledge, care about price
# 2: Owners -- 205 members, own and don't want to buy, high knowledge, care about sound, highest age and high income
# 3: Poor Graduates -- 131 members, don't own but want to buy, youngest, lowest income, highest education

