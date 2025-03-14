# Model Otter from Excel into R

#Date Created: Dec 2024

#Author: Julia Adelsheim
#Collaborators: Andreas Novotny 

# This version is after Julia deleted all the shit from Github
# 1-24-2025

rm(list = ls())
# setwd("~/Documents/Thesis/Otter Data/Model Data/R codes/Model to R/Sensitivity Analysis")
setwd("~/Documents/Thesis/otteR/Data")

# Packages--------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

#Constants--------------------------------------------------------------
cost_pup <- 3931 # TODO to change
total_min_per_day <- 1440

# Data------------------------------------------------------------------
masses <- read.csv(file ='mass_growth.csv') 
# filter(!is.na(Age)) # remove empty rows

act_budgets <- read.csv(file = 'ActivityBudgets.csv') 
# filter(!is.na(MR)) # remove empty rows

age_convert <- read.csv(file = 'age_lifestage.csv')

# Functions ------------------------------------------------------------

# Converts raw csv files of masses, growth rates, metabolic rates, and costs of behaviors into
#  a merged dataframe.
merge_data <- function(masses, act_budgets, age_convert) {
  # Join data frames to create a table with MASS, GROWTH, and METABOLIC RATES per behavior
  mass_lifestage <- merge(masses, age_convert, by.x = c('Age', 'Sex')) #, all.x = TRUE) # would make blank rows
  mass_lifestage_budget <- merge(mass_lifestage, act_budgets, by.x = c('Sex', 'Lifestage'))  %>% 
    #Calculate minutes per day from % of day
    mutate(min_per_day = perc_time * total_min_per_day) %>% 
    # Calculate metabolic rates
    mutate(metabolic_rates = Av_mass * MR) %>% 
    # Calculate actual (additional) metabolic costs
    mutate(actual_costs = metabolic_rates * min_per_day)
  return(mass_lifestage_budget)
}

# Re-sort data into a wider pivot table to columns specific to behavior: 
#   MR_*, metabolic_rates_*, min_per_day_*, perc_time_*, actual_costs_*
make_wide_data <- function(mass_lifestage_budget, cost_pup) {
  mass_lifestage_budget_wide <- mass_lifestage_budget %>%
    # Make dataframe in a wide format
    pivot_wider(names_from = Behaviour, values_from=c(perc_time, min_per_day, MR, metabolic_rates, actual_costs)) %>% 
    mutate(resting_cost_wout_foraging = metabolic_rates_rest * (min_per_day_rest + min_per_day_forage)) %>% 
    # Calculate sum of actual costs
    mutate(sum_actual_costs = actual_costs_rest + actual_costs_activity + actual_costs_forage) %>% 
    # Modify pup costs
    mutate(pup_cost = ifelse(with.pup == 'yes', cost_pup, 0)) %>% 
    # Calculate ROR
    mutate(ROR = (sum_actual_costs + pup_cost + Growth)/(min_per_day_forage)) %>%
    # Make ROR repeat for adults
    mutate(ROR = ifelse(Lifestage != "adult", ROR,
                        ifelse(Sex == "M", pull(filter(.data = ., Sex == "M", Age == 3), ROR),
                               ifelse(with.pup == "no", pull(filter(.data = ., Sex == "F", with.pup=="no", Age == 2), ROR),
                                      pull(filter(.data = ., Sex == "F", with.pup=="yes", Age == 2), ROR)
                               ))))
}


# Make an ROR function so that I don't have to repeat the code every time I change conditions
# Data is the wide_data table with behavior specific values, returns a dataframe that calculated
#  from profits, time_needed_to_forage, and new_perc_time_forage
calculate_foraging_time <- function(mass_lifestage_budget_wide) {
  
  mass_lifestage_budget_wide <- mass_lifestage_budget_wide %>% 
    
    # Calculate Profits
    mutate(profits = ROR - (metabolic_rates_forage - metabolic_rates_rest)) %>% 
    
    # Calculate foraging time needed
    mutate(time_needed_to_forage = (resting_cost_wout_foraging + actual_costs_activity + Growth + pup_cost)/ profits) %>% 
    
    #Calc % of day foraging
    mutate(new_perc_time_forage = time_needed_to_forage/total_min_per_day) %>% 
    
    select(Sex, Age, Lifestage, with.pup, ROR, profits, time_needed_to_forage, new_perc_time_forage)
  
}

# This function builds parts of the model. Takes in a wide dataframe of activity specific rates and costs.
#  Returns new costs of foraging.
build_model <- function(budget_data, ROR){  
  
  # Put in columns that are not going to change with ROR
  model_pt1 <- budget_data %>%
    select(c(Sex, Age, Lifestage, with.pup, Growth, pup_cost, perc_time_activity, min_per_day_activity, actual_costs_activity, metabolic_rates_forage, metabolic_rates_rest)) 
  
  # Attach ROR table that you want to use
  model_setup <- merge(model_pt1, ROR, by = c('Sex', 'Age', 'Lifestage', 'with.pup')) %>% 
    # Add new foraging % of day and minutes per day values
    select(c(Sex, Age, Lifestage, with.pup, pup_cost, Growth, perc_time_activity, min_per_day_activity, actual_costs_activity, new_perc_time_forage, metabolic_rates_forage, time_needed_to_forage, metabolic_rates_rest)) %>%
    arrange(Age, Sex)
  
  
  #calculate new cost of foraging
  model_setup <- model_setup %>% 
    mutate(foraging_cost = metabolic_rates_forage * time_needed_to_forage) %>% 
    
    # Calculate % resting
    mutate(new_perc_time_rest = (1- (perc_time_activity + new_perc_time_forage))) %>% 
    
    # Calculate min per day resting
    mutate(new_min_per_day_rest= (total_min_per_day - (min_per_day_activity + time_needed_to_forage))) %>% 
    
    # Calculate resting cost
    mutate(new_resting_cost= new_min_per_day_rest * metabolic_rates_rest)
  
  return(model_setup)
}


run_model <- function(mod_setup){
  mod_setup <- mod_setup %>% 
    
    #Calculate total energy requirements
    mutate(total_energy = pup_cost + Growth + actual_costs_activity + foraging_cost + new_resting_cost)
  
  return(mod_setup)
}

# Runs through the entire model. 
#   Receives the raw data frame of model variables.

#   Returns a dataframe of the calculated total energy expenditure.
otter_model <- function(masses, act_budgets, age_convert, cost_pup) {
  mass_lifestage_budget <- merge_data(masses = masses,
                                      act_budgets = act_budgets,
                                      age_convert = age_convert)
  wide_data <- make_wide_data(mass_lifestage_budget, cost_pup)
  
  # Changing conditions of ROR
  ROR_og <- calculate_foraging_time(wide_data) # Original ROR
  
  # Build the model using the calculated wide df and the ROR
  model_setup <- build_model(budget_data = wide_data, ROR = ROR_og)
  
  # Run the model to get total_energy
  model_results <- run_model(mod_setup = model_setup) %>% 
    relocate(total_energy)
  
  return(model_results)
}

# Model----------------------------------------------------------

# This is the default model run with literature mean values for the parameters.
model.run.1 <- otter_model(masses = masses,
                           act_budgets = act_budgets,
                           age_convert = age_convert,
                           cost_pup = 3931)


## Save out as csv ##
folder_path <- "~/Documents/Thesis/otteR/Results"
filename <- paste0(folder_path,"original_model_run.csv")
# Save the results to CSV
write.csv(model.run.1, file = filename, row.names = FALSE)
