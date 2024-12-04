#Otter Model -- Basic

#Date Created: Nov 29, 2023

#Author: Julia Adelsheim

#Platform: Mac
#Purpose of Script: Running model to help regenerate data while writing data chapters 

## Coding Questions-----------------------------------------------------


# To Do ------------------------------------------------------------------
# Export model_run_og as tables (M, F yes and F no)
#add labels to percents? add title to grouped bar graph

## Start-----------------------------------------------------------------------
#File Name: Otter Model_Writing

rm(list = ls())
setwd("~/Documents/Thesis/otteR/")

# Packages--------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)

#Constants--------------------------------------------------------------
cost_pup <- 3931
total_min_per_day <- 1440

# Data------------------------------------------------------------------
# Body masses and tissue growth
masses <- read.csv(file ='./Data/mass_growth.csv')

# How each unit spend their time
act_budgets <- read.csv(file = './Data/ActivityBudgets.csv')

# Defining life stages per year and sex
age_convert <- read.csv(file = './Data/age_lifestage.csv')

# Functions ------------------------------------------------------------
# Make an ROR function so that I don't have to repeat the code every time I change conditions
# Data is the wide_data table with behavior specific values, returns a dataframe that calculated
#  from profits, time_needed_to_forage, and new_perc_time_forage
calculate_foraging_time <- function(data) {
  
  output <- data %>% 
    
    # Calculate Profits
    mutate(profits = ROR - (metabolic_rates_forage - metabolic_rates_rest)) %>% 
    
    # Calculate foraging time needed
    mutate(time_needed_to_forage = (actual_costs_activity + resting_cost_wout_foraging)/ profits) %>% 
    
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
  
  #  Yellow cells in excel for these variables 
  
  # set up the model table- start calculating new values for foraging
  # model_setup <- merge(model_setup, foraging_met_rate)
  
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

# Converts raw csv files of masses, growth rates, metabolic rates, and costs of behaviors into
#  a merged dataframe.
merge_data <- function(masses, act_budgets, age_convert) {
  # Join data frames to create a table with MASS, GROWTH, and METABOLIC RATES per behavior
  mass_lifestage <- merge(masses, age_convert, by.x = c('Age', 'Sex')) #, all.x = TRUE) # would make blank rows
  mass_lifestage_budget <- merge(mass_lifestage, act_budgets, by.x = c('Sex', 'Lifestage'))  %>% 
    # Calculate metabolic rates
    mutate(metabolic_rates = Av_mass * MR) %>% 
    # Calculate actual (additional) metabolic costs
    mutate(actual_costs = metabolic_rates * min_per_day)
  return(mass_lifestage_budget)
}

# Re-sort data into a wider pivot table to columns specific to behavior: 

make_wide_data <- function(mass_lifestage_budget) {
  mass_lifestage_budget_wide <- mass_lifestage_budget %>%
    # Make dataframe in a wide format
    pivot_wider(names_from = Behaviour, values_from=c(perc_time, min_per_day, MR, metabolic_rates, actual_costs)) %>% 
    mutate(resting_cost_wout_foraging = metabolic_rates_rest * (min_per_day_rest + min_per_day_forage)) %>% 
    # Calculate sum of actual costs
    mutate(sum_actual_costs = actual_costs_rest + actual_costs_activity + actual_costs_forage) %>% 
    # Modify pup costs
    mutate(pup_cost = ifelse(with.pup == 'yes', cost_pup, 0)) %>% 
    # Calculate Original ROR
    mutate(ROR = (sum_actual_costs + pup_cost + Growth)/(min_per_day_forage)) %>%
    # Make ROR repeat for adults
    mutate(ROR = ifelse(Lifestage != "adult", ROR,
                        ifelse(Sex == "M", pull(filter(.data = ., Sex == "M", Age == 3), ROR),
                               ifelse(with.pup == "no", pull(filter(.data = ., Sex == "F", with.pup=="no", Age == 2), ROR),
                                      pull(filter(.data = ., Sex == "F", with.pup=="yes", Age == 2), ROR)
                               ))))
}

# Runs through the entire otter bioenergetics model. 
#   Receives the raw data frame of model variables.
#   Returns a dataframe of the calculated total energy expenditure.

otter_model <- function(masses, act_budgets, age_convert) {
  mass_lifestage_budget <- merge_data(masses = masses,
                                      act_budgets = act_budgets,
                                      age_convert = age_convert)
  wide_data <- make_wide_data(mass_lifestage_budget)
  
  # Changing conditions of ROR
  ROR_og <- calculate_foraging_time(wide_data) # Original ROR
  
  # Build the model using the calculated wide df and the ROR
  model_setup <- build_model(budget_data = wide_data, ROR = ROR_og)
  
  # Run the model to get total_energy
  model_results <- run_model(mod_setup = model_setup) %>% 
    relocate(total_energy)
  
  return(model_results)
}

# Build Model---------------------------------------------------------------------------------


model_run_og <- otter_model(masses = masses,
                            act_budgets = act_budgets,
                            age_convert = age_convert)



model_run_og %>% 
  mutate(Group = paste(Sex, with.pup)) %>% 
  ggplot() +
  geom_line(aes(Age, total_energy, color = Group))

# Body mass per age sex and life stage - growth rate
# Metabolic rates
# Percent time of day (Activity budget)
# Cost of having pup

#---- Graphs ----

#---- Activity Budget Graphs---- 

# Pups/Juveniles/Subadults

actbudget_young <- model_run_og %>% 
  filter(Lifestage != "adult") %>% 
  select(Sex, Age, Lifestage, with.pup, perc_time_activity, new_perc_time_forage, new_perc_time_rest) %>% 
  pivot_longer(cols=c(perc_time_activity, new_perc_time_forage, new_perc_time_rest),
               names_to='Behaviour',
               values_to='PercentTime') %>% 
  mutate(Behaviour = recode(Behaviour, perc_time_activity = 'Activity', 
                            new_perc_time_forage = 'Foraging', new_perc_time_rest = 'Resting')) 

#---- Stacked Bar Graph----
library(data.table)
actbudget_young <- data.table(actbudget_young)
actbudget_young <- actbudget_young[,  mean(PercentTime), by=.(Behaviour, Lifestage)] 

actbudget_young$Lifestage <- factor(actbudget_young$Lifestage)

actbudget_young$Lifestage <- ordered(actbudget_young$Lifestage, levels = c("pup", "juvenile", "subadult"))

(actbudget_young_stackedplot <- ggplot(actbudget_young,
                                       aes(x = Lifestage, y = V1, fill=Behaviour)) +
    geom_bar(stat='identity') +
    theme_classic() +
    ylab("Percent of Day")+
    scale_x_discrete(labels = c("Pup", "Juvenile", "SubAdult"))
)
actbudget_young <- ggsave("Actbudget_young_stacked.jpeg", width = 4, height = 2.6)

#---- Grouped Bar Graph----

#Data
actbudget_young <- model_run_og %>% 
  filter(Lifestage != "adult") %>% 
  select(Sex, Age, Lifestage, with.pup, perc_time_activity, new_perc_time_forage, new_perc_time_rest) %>% 
  pivot_longer(cols=c(perc_time_activity, new_perc_time_forage, new_perc_time_rest),
               names_to='Behaviour',
               values_to='PercentTime') %>% 
  mutate(Behaviour = recode(Behaviour, perc_time_activity = 'Activity', 
                            new_perc_time_forage = 'Foraging', new_perc_time_rest = 'Resting')) 

actbudget_young$Lifestage <- factor(actbudget_young$Lifestage)

actbudget_young$Lifestage <- ordered(actbudget_young$Lifestage, levels = c("pup", "juvenile", "subadult"))
#Graph

(actbudget_young_bars <- ggplot(actbudget_young,
                                aes(x = Behaviour, y = PercentTime, fill=Behaviour)) +
    geom_bar(position = "dodge", stat= "identity", show.legend = "FALSE") +
    facet_grid(.~Lifestage) +
    theme_classic() +
    ylab("Percent of Day") 
)


actbudget_young_bars <- ggsave("Actbudget_young.jpeg", width = 6, height = 3.6)







