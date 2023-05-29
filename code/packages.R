##########################################################
# Project: NeuroRehab Case Study 
# This file: packages needed for complete code functionality
# 05/2023
# Spencer Arbuckle
# R version 4.2.2
##########################################################

packages <- c("sf", # simple features geospatial tools
              "here", # file path tools
              "dplyr", # data frame filtering & summarizing
              "plotly", # interactive visualizations
              "forcats", # for data frame reordering
              "ggplot2", # plotting
              "gridExtra", # subplots
              "patchwork", # more plotting tools
              "geosphere" # geospatial distance tools
              )
for (p in packages){
  # install package if needed
  if (!require(p, character.only = TRUE)){install.packages(p, character.only = TRUE)};
  # load package to environment
  library(p, character.only = TRUE)
}