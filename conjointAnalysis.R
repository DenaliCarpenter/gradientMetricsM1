# Packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(radiant)

# Read in the Data --------------------------------------------------------

expData <- read_sav("experiment_data.sav")
surData <- read_sav("survey_data.sav")

# Conjoint Analysis Model -------------------------------------------------

conjointAnalysisOne <- conjoint(expData, 
                                rvar = "answer", 
                                evar = colnames(expData)[3:8])

# Conjoint Analysis Summary -----------------------------------------------

conjointSummary <- summary(conjointAnalysisOne)

# Part-Worth Plots --------------------------------------------------------

pwPlots <- plot(conjointAnalysisOne, custom = TRUE)

# Importance-Weight Plot --------------------------------------------------

iwPlot <- plot(conjointAnalysisOne, plots = "iw")

# Order Analysis ----------------------------------------------------------

orderModel <- summary(lm(answer ~ task, expData))


