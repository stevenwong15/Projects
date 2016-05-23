setwd('~/Desktop/Analytics/Titanic')

#=================================================================================
# preloads essential libraries to the current directory

library(colorout)
library(MASS)  # standard statistical package
library(scales)  # allows for $, %, etc.
library(readr)  # alternative ways to load data
library(tidyr)  # alternative to reshape2()
library(ggplot2)  # ggplot2.org/book
library(e1071)  # svm
library(stringr)
library(caret)
library(xgboost)
library(tree)
library(randomForest)
library(dplyr)  # splitting, applying and combining data, replacing plyr
library(gam)
library(glmnet)
library(parallel)

#=================================================================================
# preloads essential functions to the current directory

source("code/starter/ggplot2_theme.r")
source("code/starter/save_as.r")