# Packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(radiant)
library(conjoint)
library(dplyr)
library(data.table)
library(ranger)
library(rsample)
library(randomForest)
library(cluster)
library(factoextra)

# Conjoint Analysis Using the conjoint package ----------------------------

#profiles

attributes <- colnames(expData)[3:8]

fromCharToNum <- function(x){
  result <- as.factor(as.numeric(as.factor(x)))
  
  return(result)
}

fixRows <- function(x){
  result <- as.character(unlist(x))
  return(result)
}

expDataProfs <- expData %>%
  mutate_at(attributes, fromCharToNum) %>%
  dplyr::select(-response_id, -task, -answer) %>%
  distinct() %>% 
  rowid_to_column("profile")

profilesFinal <- expDataProfs %>% 
  dplyr::select(-profile)

expDataPrefs <- expData %>%
  mutate_at(attributes, fromCharToNum) %>%
  left_join(expDataProfs) %>% 
  mutate(profile = paste0("profile", profile)) %>% 
  dplyr::select(response_id, profile, answer) %>% 
  pivot_wider(names_from = profile, values_from = answer, values_fill = NA) %>% 
  distinct() 

responseIDs <- expDataPrefs$response_id

expDataPrefs <- expDataPrefs %>% 
  column_to_rownames("response_id") %>% 
  mutate_all(as.character)

for(i in 1:ncol(expDataPrefs)){
  expDataPrefs[expDataPrefs[,i] == "NULL", i] <- NA
  print(i)
}

expDataPrefs <- expDataPrefs %>% 
  mutate_all(as.numeric)

# for(i in 1:ncol(expDataPrefs)){
#   expDataPrefs[is.na(expDataPrefs[,i]), i] <- mean(expDataPrefs[,i], na.rm = TRUE)
# }

preferencesFinal <- expDataPrefs

levelsList <- list()

for (i in 1:6) {
  levelsList[i] <- expData %>% 
    dplyr::select(contains(c(attributes[i], toupper(attributes[i])))) %>% 
    distinct() %>% 
    arrange(toupper(attributes[i])) %>% 
    dplyr::select(attributes[i]) %>% 
    as.list()
}

levelsFinal <- as.data.frame(unlist(levelsList))

preferencesFinal <- data.frame(preferencesFinal)
profilesFinal <- data.frame(profilesFinal)
levelsFinal <- data.frame(levelsFinal)

individualUtilitiesSparse <- caPartUtilities(y=preferencesFinal, x=profilesFinal, z=levelsFinal)
test <- caUtilities(y=preferencesFinal, x=profilesFinal, z=levelsFinal)
conjointModel <- Conjoint(y=preferencesFinal, x=profilesFinal, z=levelsFinal)
caSegmentation(y=preferencesFinal, x=profilesFinal, c=3)


# Clustering based on price and duration utilities ------------------------

durationPrice <- as.data.frame(individualUtilitiesSparse) %>% 
  bind_cols(responseIDs) %>% 
  mutate("ID" = ...26) %>% 
  dplyr::select(ID, intercept, `3 months`:`12 months`, `$30/month`:`$20/month`) %>%
  na.omit()

rownames(durationPrice) <- c()
  
durationPrice <- durationPrice %>%
  column_to_rownames(var = "ID") %>% 
  scale()

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(durationPrice, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

fviz_nbclust(durationPrice, kmeans, method = "wss")

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(durationPrice, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(durationPrice))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")         

fviz_nbclust(durationPrice, kmeans, method = "silhouette")

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(durationPrice, FUN = kmeans, nstart = 25,
                    K.max = 8, B = 50, iter.max = 30)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

set.seed(123)
final <- kmeans(durationPrice, 4, nstart = 25)
print(final)

fviz_cluster(final, data = durationPrice, geom = "point")
