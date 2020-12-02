library(tidyverse)
library(cluster)
library(factoextra)
library(haven)
library(fclust)
library(ggplot2)
library(readr)
library(Rtsne)
library(sjlabelled)
library(fpc)
library(reshape2)
library(dendextend)
library(klaR)
library(cba)



# Data Cleaning -----------------------------------------------------------

surveyData <- read_sav("survey_data.sav")

singleResponse <- surveyData %>% 
  select_if(function(x){!any(is.na(x))}) %>% 
  mutate(s_age = as.numeric(as.factor(s_age))) %>% 
  select(d_education, s_hhincome, s_problem,
         contains("philosophy"), contains("attitudes"),
         hours, interst_cbt, interest_coach, d_h_hnumber,
         d_political_view, response_id) %>% 
  mutate_if(is.numeric, scale) %>% 
  column_to_rownames(var = "response_id") %>% 
  na.omit()


# K-means clustering ------------------------------------------------------

distance <- get_dist(singleResponse)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(singleResponse, centers = 2, nstart = 25)

set.seed(123)
fviz_nbclust(singleResponse, kmeans, method = "wss")

set.seed(123)
fviz_nbclust(singleResponse, kmeans, method = "silhouette")

set.seed(123)
gap_stat <- clusGap(singleResponse, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 500, iter.max=100)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#final
set.seed(123)
finalClustering <- kmeans(singleResponse, 2, nstart = 25)
fviz_cluster(finalClustering, data = singleResponse, geom = "point")


# Robust Clustering of Mixed Data Types -----------------------------------

surveyDataCategorical <- surveyData %>% 
  replace(is.na(.), 0) %>% 
  select(-weights) %>% 
  column_to_rownames(var = "response_id") %>% 
  mutate_all(as.factor) %>% 
  remove_all_labels(.)
  

gowerDist <- daisy(surveyDataMixed, metric = "gower")
gowerMat <- as.matrix(gowerDist)

surveyDataMixed[which(gowerMat == max(gowerMat[gowerMat != max(gowerMat)]), arr.ind = TRUE)[1, ], ]

silWidth <- c(NA)
for(i in 1:8){  
  pamFit <- pam(gowerDist, diss = TRUE, k = i)  
  silWidth[i] <- pamFit$silinfo$avg.width  
}

plot(1:8, silWidth,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, silWidth)

k <- 2

pamFit <- pam(gowerDist, diss = TRUE, k)

pamResults <- surveyDataMixed %>%
  mutate(cluster = pamFit$clustering) %>%
  group_by(cluster) %>%
  do(theSummary = summary(.))

pamResults$theSummary

tsneObj <- Rtsne(gowerDist, is_distance = TRUE)

tsneData <- tsneObj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pamFit$clustering))

ggplot(aes(x = X, y = Y), data = tsneData) +
  geom_point(aes(color = cluster))



# Hierarchical Clustering -------------------------------------------------

set.seed(123)

gowerDist <- daisy(surveyDataCategorical, metric = "gower")

divisiveClust <- diana(as.matrix(gowerDist), 
                        diss = TRUE, keep.diss = TRUE)

plot(divisiveClust, main = "Divisive")

agglClustC <- hclust(gowerDist, method = "complete")

plot(agglClustC,
     main = "Agglomerative, complete linkages")

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gowerDist, divisiveClust, 7)
stats.df.divisive

stats.df.aggl <- cstats.table(gowerDist, agglClustC, 7) #complete linkages looks like the most balanced approach
stats.df.aggl

ggplot(data = data.frame(t(cstats.table(gowerDist, divisiveClust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gowerDist, divisiveClust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gowerDist, agglClustC, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gowerDist, agglClustC, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

dendro <- as.dendrogram(agglClustC)
dendro.col <- dendro %>%
  set("branches_k_color", k = 2, value =   c("darkslategray", "darkslategray4")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 2")


# K-Modes -----------------------------------------------------------------

kmodesModel <- kmodes(surveyDataCategorical, 3)

totalWithinDiff <- data.frame("n" = vector(length = 9), "total within diff" = vector(length = 9))
for(i in 2:10){
  totalWithinDiff$n[i - 1] <- i
  
  totalWithinDiff$total.within.diff[i - 1] <- sum(kmodes(surveyDataCategorical, i)$withindiff)
}

p <- ggplot(aes(x = n, y = total.within.diff), data = silDF) +
  geom_point() +
  geom_line()

kmodesModelFinal <- kmodes(surveyDataCategorical, 4, iter.max = 400)


# ROCK --------------------------------------------------------------------


