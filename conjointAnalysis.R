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

# Read in the Data --------------------------------------------------------

expData <- read_sav("experiment_data.sav")

# Conjoint Analysis Model -------------------------------------------------

conjointAnalysisOne <- conjoint(expData, 
                                rvar = "answer", 
                                evar = colnames(expData)[3:8])

# Conjoint Analysis Summary -----------------------------------------------

conjointSummary <- summary(conjointAnalysisOne)

# Part-Worth Plots --------------------------------------------------------

pwPlots <- plot(conjointAnalysisOne, 
                custom = TRUE)

# Importance-Weight Plot --------------------------------------------------

iwPlot <- plot(conjointAnalysisOne, 
               plots = "iw")

# Order Analysis ----------------------------------------------------------

orderModel <- summary(
  lm(answer ~ task, 
     expData))


# Conjoint Analysis Using the conjoint package ----------------------------

#profiles

attributes <- colnames(expData)[3:8]

fromCharToNum <- function(x){
  result <- as.factor(as.numeric(as.factor(x)))
  
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
  distinct() %>% 
  column_to_rownames("response_id") %>% 
  mutate_all(as.character)

for(i in 1:ncol(expDataPrefs)){
  expDataPrefs[expDataPrefs[,i] == "NULL", i] <- NA
  print(i)
}

expDataPrefs <- expDataPrefs %>% 
  mutate_all(as.numeric)

for(i in 1:ncol(expDataPrefs)){
  expDataPrefs[is.na(expDataPrefs[,i]), i] <- mean(expDataPrefs[,i], na.rm = TRUE)
}


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

duration <- list()


  duration <- expData %>% 
    dplyr::select(contains(c("price", toupper("price")))) %>% 
    distinct() %>% 
    arrange(toupper(duration))

duration <- expData %>% 
  select(duration) %>% 
  mutate(durationTwo = as.factor(as.numeric(as.factor(duration)))) %>% 
  distinct() %>% 
  arrange(durationTwo)

offer <- expData %>% 
  select(offer) %>% 
  mutate(offerTwo = as.factor(as.numeric(as.factor(offer)))) %>% 
  distinct() %>% 
  arrange(offerTwo)

outcome <- expData %>% 
  select(outcome) %>% 
  mutate(outcomeTwo = as.factor(as.numeric(as.factor(outcome)))) %>% 
  distinct() %>% 
  arrange(outcomeTwo)

price <- expData %>% 
  select(price) %>% 
  mutate(priceTwo = as.numericas.factor(as.numeric(as.factor(price)))) %>% 
  distinct() %>% 
  arrange(priceTwo)

rtb <- expData %>% 
  select(rtb) %>% 
  mutate(rtbTwo = as.factor(as.numeric(as.factor(rtb)))) %>% 
  distinct() %>% 
  arrange(rtbTwo)

social_proof <- expData %>% 
  select(social_proof) %>% 
  mutate(social_proofTwo = as.factor(as.numeric(as.factor(social_proof)))) %>% 
  distinct() %>% 
  arrange(social_proofTwo)

levelsFinal <- as.data.frame(unlist(levelsList))

preferencesFinal <- data.frame(preferencesFinal)
profilesFinal <- data.frame(profilesFinal)
levelsFinal <- data.frame(levelsFinal)

individualUtilities <- caPartUtilities(y=preferencesFinal, x=profilesFinal, z=levelsFinal)
conjointModel <- Conjoint(y=preferencesFinal, x=profilesFinal, z=levelsFinal)
caSegmentation(y=preferencesFinal, x=profilesFinal, c=3)


# Random Forest Model to Predict Responses --------------------------------

rFData <- expData %>% 
  select(-response_id) %>% 
  mutate_all(as.factor)

set.seed(123)
expSplit <- initial_split(rFData, prop = .7)
expTrain <- training(expSplit)
expTest  <- testing(expSplit)

rangerOne <- ranger(answer ~ ., data = expTrain)
results <- predict(rangerOne, expTest)$predictions


