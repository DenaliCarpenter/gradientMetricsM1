library(tidyverse)
library(haven)
library(svMisc)
library(conjoint)
library(dplyr)
library(ranger)
library(rsample)
library(cluster)
library(factoextra)

fromCharToFactorNum <- function(x){
  result <- as.factor(as.numeric(as.factor(x)))
  
  return(result)
}

getProfs <- function(attributes, dataFrame){
  
  result <- dataFrame %>% 
    dplyr::mutate_at(attributes, fromCharToFactorNum) %>% 
    dplyr::select(attributes) %>% 
    dplyr::distinct()
  
  return(result)
  
}

getAllProfs <- function(attributes, idColumnName, dataFrame, replaceNames = TRUE){
  
  namedLevelList <- list()
  
  for(i in 1:length(attributes)){
    namedLevelList[toString(attributes[i])] <- unique(dataFrame[,toString(attributes[i])])
  }
  
  if(replaceNames == TRUE) {
    
  result <- expand.grid(namedLevelList) %>% 
    mutate_at(attributes[attributes != idColumnName], fromCharToNum) %>%
    distinct()
  
  } else {
    result <- expand.grid(namedLevelList) %>% 
      distinct()
  }
  
  return(result)
  
}

getPrefs <- function(attributes, profs, idColumnName, answerColumnName, dataFrame){

  profsWithRowID <- profs %>% 
    tibble::rowid_to_column("profile")
  
  
  result <- dataFrame %>%
    dplyr::mutate_at(attributes, fromCharToNum) %>%
    left_join(profsWithRowID) %>% 
    mutate(profile = paste0("profile", profile)) %>% 
    dplyr::select(idColumnName, profile, answerColumnName) %>% 
    tidyr::pivot_wider(names_from = profile, values_from = answerColumnName, values_fill = NA) %>% 
    dplyr::distinct() %>% 
    tibble::column_to_rownames(idColumnName) %>% 
    dplyr::mutate_all(as.character)
  
  print("Fixing missing values")
  
  for(i in 1:ncol(resul)){
    
    result[result[,i] == "NULL", i] <- NA
    
    svMisc::progress(i)
    if(i == ncol(result)){
      cat("Done!\n")
    }
  }
  
  result <- result %>% 
    dplyr::mutate_all(as.numeric)
  
  return(result)
  
}

getPredictedPrefs <- function(attributes, allProfs, idColumnName, answerColumnName, dataFrameExp, dataframeSurvey){

  #create a dataframe without RespondentIDs and create a number for each profile
  profileNums <- allProfs %>% 
    select(-idColumnName) %>% 
    distinct() %>% 
    rowid_to_column("profile")
    
  
  fullData <- dataFrameExp %>% 
    full_join(dataframeSurvey) %>% 
    as.data.frame()
  
  for(i in 1:ncol(fullData)){
    
    fullData[is.na(fullData[,i]), i] <- 0
    
  }
  
  cleanSurveyData <- dataframeSurvey
  
  for(i in 1:ncol(cleanSurveyData)){
    
    cleanSurveyData[is.na(cleanSurveyData[,i]), i] <- 0
    
  }
  
  #building random forest model for prediction
  set.seed(123)
  split <- initial_split(fullData, prop = .8)
  train <- training(split)
  test  <- testing(split)
  
  formula <- paste0(answerColumnName, " ~ .")
  
  modelOne <- ranger(formula, data = train)
  results <- predict(modelOne, test)$prediction
  
  resultsDF <- test %>% 
    dplyr::select(idColumnName, answerColumnName) %>% 
    bind_cols(prediction = results)
  
  rSqd <- cor(resultsDF[answerColumnName], resultsDF$prediction)^2
  
  print(paste0("Testing R-Squared = ", rSqd))
  print("Training Random Forest model on full dataset")
  finalModel <- ranger(formula, data = fullData)
  print("Final model created.")
  
  #above is good
  
  allProfs <- allProfs %>% 
    full_join(dataFrameExp) %>% 
    full_join(cleanSurveyData)
  
  noAnswer <- allProfs %>%
    filter(is.na(allProfs[answerColumnName]))
  
  answerDF <- allProfs %>% 
    filter(!is.na(allProfs[answerColumnName]))
  
  print("Predicting missing entires.")
  predictedAnswer <- predict(finalModel, noAnswer)$prediction
  
  newAnswer <- noAnswer %>% 
    dplyr::select(-answerColumnName) %>% 
    bind_cols(answer = predictedAnswer)
  
  fullExperinmentResults <- newAnswer %>% 
    bind_rows(answerDF)
  
  allProfs <- allProfs %>% 
    full_join(profileNums) %>% 
    dplyr::select(attributes, profile) %>% 
    mutate_at(attributes[attributes != idColumnName], fromCharToFactorNum)
  
  result <- fullExperinmentResults %>%
    mutate_at(attributes[attributes != idColumnName], fromCharToFactorNum) %>%
    left_join(allProfs) %>% 
    mutate(profile = paste0("profile", profile)) %>% 
    dplyr::select(idColumnName, profile, answer) %>% 
    pivot_wider(names_from = profile, values_from = answer, values_fill = NA) %>% 
    distinct() %>% 
    column_to_rownames(idColumnName) %>% 
    mutate_all(as.character) %>% 
    mutate_all(as.numeric)
  
  return(result)
}

testPrefs <- getPredictedPrefs(attributes = attributes, 
                               testProfs, 
                               idColumnName = "response_id", 
                               answerColumnName = "answer", 
                               dataFrameExp = expData %>% select(-task), 
                               dataframeSurvey = surveyData)

attributes <- colnames(expData[c(1, 3:8)])

# [1] "response_id"  "duration"     "offer"        "outcome"      "price"        "rtb"          "social_proof"

getLevels <- function(attributes, dataFrame){
  
  result <- list()
  
  for (i in 1:length(attributes)) {
    
    result[i] <- dataFrame %>% 
      dplyr::select(dplyr::contains(c(attributes[i], toupper(attributes[i])))) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(toupper(attributes[i])) %>% 
      dplyr::select(attributes[i]) %>% 
      as.list()
    
    }
  
  result <- as.data.frame(unlist(result))
  
  return(result)
}

