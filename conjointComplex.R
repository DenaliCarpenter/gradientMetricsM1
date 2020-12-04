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

expData <- read_sav("experiment_data.sav")

surveyData <- read_sav("survey_data.sav")

fullData <- expData %>% 
  full_join(surveyData) %>% 
  as.data.frame()

for(i in 1:ncol(fullData)){
  fullData[is.na(fullData[,i]), i] <- 0
  print(i)
}

set.seed(123)
split <- initial_split(fullData, prop = .8)
train <- training(split)
test  <- testing(split)

modelOne <- ranger(answer ~ . -response_id - task, data = train)
results <- predict(modelOne, test)$prediction

resultsDF <- test %>% 
  dplyr::select(response_id, answer) %>% 
  bind_cols(prediction = results)

cor(resultsDF$answer, resultsDF$prediction)^2

finalModel <- ranger(answer ~ . -response_id - task, data = fullData)

noTask <- expData %>% 
  dplyr::select(-task)

cleanSurveyData <- surveyData

for(i in 1:ncol(cleanSurveyData)){
  cleanSurveyData[is.na(cleanSurveyData[,i]), i] <- 0
  print(i)
}

fullExperiment <- expand.grid(response_id = unique(expData$response_id),
                              duration = unique(expData$duration),
                              offer = unique(expData$offer),
                              outcome = unique(expData$outcome),
                              price = unique(expData$price),
                              rtb = unique(expData$rtb),
                              social_proof = unique(expData$social_proof)) %>% 
  full_join(noTask) %>% 
  full_join(cleanSurveyData)

noAnswer <- fullExperiment %>% 
  filter(is.na(answer))

answer <- fullExperiment %>% 
  filter(!is.na(answer))

predictedAnswer <- predict(finalModel, noAnswer)$prediction

newAnswer <- noAnswer %>% 
  dplyr::select(-answer) %>% 
  bind_cols("answer" = predictedAnswer)

fullExperinmentResults <- newAnswer %>% 
  bind_rows(answer) %>% 
  dplyr::select(colnames(expData %>% dplyr::select(-task)))

# Conjoint Analysis Using the conjoint package ----------------------------

#profiles

attributes <- colnames(fullExperinmentResults)[2:7]

fromCharToNum <- function(x){
  result <- as.factor(as.numeric(as.factor(x)))
  
  return(result)
}

fixRows <- function(x){
  result <- as.character(unlist(x))
  return(result)
}

fullExpProfs <- fullExperinmentResults %>%
  mutate_at(attributes, fromCharToNum) %>%
  dplyr::select(-response_id, -answer) %>%
  distinct() %>% 
  rowid_to_column("profile")

profilesFinal <- fullExpProfs %>% 
  dplyr::select(-profile)

fullExpPrefs <- fullExperinmentResults %>%
  mutate_at(attributes, fromCharToNum) %>%
  left_join(fullExpProfs) %>% 
  mutate(profile = paste0("profile", profile)) %>% 
  dplyr::select(response_id, profile, answer) %>% 
  pivot_wider(names_from = profile, values_from = answer, values_fill = NA) %>% 
  distinct() %>% 
  column_to_rownames("response_id") %>% 
  mutate_all(as.character)

fullExpPrefs <- fullExpPrefs %>% 
  mutate_all(as.numeric)

# for(i in 1:ncol(expDataPrefs)){
#   expDataPrefs[is.na(expDataPrefs[,i]), i] <- mean(expDataPrefs[,i], na.rm = TRUE)
# }

preferencesFinal <- fullExpPrefs

levelsList <- list()

for (i in 1:6) {
  levelsList[i] <- fullExperinmentResults %>% 
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

individualUtilities <- caPartUtilities(y=preferencesFinal, x=profilesFinal, z=levelsFinal)
conjointModel <- Conjoint(y=preferencesFinal, x=profilesFinal, z=levelsFinal)
caSegmentation(y=preferencesFinal, x=profilesFinal, c=3)

responseIDs <- unique(expData$response_id)


kMeanDF <- as.data.frame(individualUtilities) %>% 
  bind_cols("response_id" = responseIDs) %>%
  na.omit()

rownames(kMeanDF) <- c()

kMeanDF <- kMeanDF %>%
  column_to_rownames(var = "response_id") %>% 
  scale()

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(kMeanDF, k, nstart = 10 )$tot.withinss
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

fviz_nbclust(kMeanDF, kmeans, method = "wss")

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(kMeanDF, centers = k, nstart = 25, iter.max = 50)
  ss <- silhouette(km.res$cluster, dist(kMeanDF))
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

fviz_nbclust(kMeanDF, kmeans, method = "silhouette")

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(kMeanDF, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50, iter.max = 30)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

set.seed(123)
final <- kmeans(kMeanDF, 3, nstart = 25, iter.max = 30)
print(final)

fviz_cluster(final, data = kMeanDF, geom = "point")

