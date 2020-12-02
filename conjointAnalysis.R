# Packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(radiant)
library(conjoint)
library(dplyr)

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
  distinct() %>% 
  column_to_rownames("response_id")

expDataPrefs$profile1 <- lapply(expDataPrefs$profile1, unlist)
  
for(i in 1:ncol(expDataPrefs)){
  expDataPrefs[expDataPrefs[,i] == "NULL", i] <- 0
}

for(i in 1:ncol(expDataPrefs)){
  expDataPrefs[i,] <- unlist(as.character(expDataPrefs[i,]))
}


for(i in 1:ncol(expDataPrefs)){
  expDataPrefs[expDataPrefs[,i] == 0, i] <- NA
}


# for(i in 1:ncol(expDataPrefs)){
#   expDataPrefs[is.na(expDataPrefs[,i]), i] <- mean(expDataPrefs[,i], na.rm = TRUE)
# }

preferencesFinal <- expDataPrefs

duration <- expDataLevels %>% 
  dplyr::select(contains(c(attributes[1], toupper(attributes[1])))) %>% 
  distinct() %>% 
  arrange(toupper(attributes[1])) %>% 
  dplyr::select(attributes[1]) %>% 
  as.list()

levelsList <- list()

for (i in 1:6) {
  levelsList[i] <- expDataLevels %>% 
    dplyr::select(contains(c(attributes[i], toupper(attributes[i])))) %>% 
    distinct() %>% 
    arrange(toupper(attributes[i])) %>% 
    dplyr::select(attributes[i]) %>% 
    as.list()
}

levelsFinal <- as.data.frame(unlist(levelsList))

caModel(y=expDataPrefs[1,], x=expDataProfs)

individualUtilities <- caPartUtilities(y=preferencesFinal, x=profilesFinal, z=levelsFinal)

