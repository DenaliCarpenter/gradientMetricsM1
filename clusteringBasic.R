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
library(radiant)

# Data Cleaning -----------------------------------------------------------

surveyData <- read_sav("survey_data.sav")

surveyDataCategorical <- surveyData %>% 
  replace(is.na(.), 0) %>% 
  dplyr::select(-weights) %>% 
  column_to_rownames(var = "response_id") %>% 
  mutate_all(as.factor) %>% 
  remove_all_labels(.)

# K-Modes -----------------------------------------------------------------

set.seed(123)

totalWithinDiff <- data.frame("n" = vector(length = 9), "total within diff" = vector(length = 9))
for(i in 2:10){
  totalWithinDiff$n[i - 1] <- i
  totalWithinDiff$total.within.diff[i - 1] <- sum(kmodes(surveyDataCategorical, i)$withindiff)
}

p <- ggplot(aes(x = n, y = total.within.diff), data = totalWithinDiff) +
  geom_point() +
  geom_line()

p

kmodesModelFinal <- kmodes(surveyDataCategorical, 4, iter.max = 400)

surveyDataClustered <- surveyData %>% 
  bind_cols(kmodesModelFinal$cluster) %>% 
  rename("cluster" = ...101) %>% 
  group_by(cluster) %>% 
  dplyr::select(-response_id) %>% 
  mutate_all(mode)
  