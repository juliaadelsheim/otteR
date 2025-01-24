
## Graphing TEE with error bars calculated from Sensitivity Analysis


##Start-----------------------------------------------------------------------

#Clean environment
rm(list = ls())

# set working directory 
setwd("~/Documents/Thesis/otteR/Results")

# Packages--------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

# Data------------------------------------------------------------------
SA_tee <- read.csv(file ='SA_results_TEE_lit2_param_all.csv') 

TEE_plot <- 
  TEE_plot <- 
  ggplot(SA_tee %>% filter(parameter == "total_energy"),
         aes(x = Age, y = value_mean, color = otter_type)) +
  scale_color_viridis_d(name = NULL, labels = c("Female without pup", "Female with a pup", "Male")) +
  ggtitle("Sea Otter Energy Expenditure") +
  ylab("Energy Expenditure (kJ/day)") +
  xlab("Age (yrs)") +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci)) +
  geom_point() +
  geom_line() +
  theme_bw()+
  theme(legend.position = "top")

print(TEE_plot)
