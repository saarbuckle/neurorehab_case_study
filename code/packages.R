##########################################################
# Project: NeuroRehab Case Study 
# This file: packages needed for complete code functionality
# 05/2023
# Spencer Arbuckle
# R version 4.2.2
##########################################################

packages <- c("here", # file path tools
              "dplyr", # data frame filtering & summarizing
              "plotly", # interactive visualizations
              "ggplot2", # plotting
              "gridExtra" # subplots
              )
for (p in packages){
  # install package if needed
  if (!require(p, character.only = TRUE)){install.packages(p, character.only = TRUE)};
  # load package to environment
  library(p, character.only = TRUE)
}