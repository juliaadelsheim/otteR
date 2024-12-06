#Sensitivity Analysis for Growth

#Date Created: Feb 8, 2023

#Author: Julia Adelsheim
#Collaborators: Patrick Pata, Izzy Morgante, Andreas Novotny

#Platform: Mac
#Purpose of Script: Code a sensitivity analysis my otter model

## Coding Questions-----------------------------------------------------

# Heat increment of feeding in otters, digestive losses- are they included?

# To Do------------------------------------------------------------------

# Remove +1 original, run models and bind the original one after

##Start-----------------------------------------------------------------------

rm(list = ls())
setwd("~/Documents/Thesis/otteR/Data")

# Packages--------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

#Constants--------------------------------------------------------------
cost_of_pup <- 3931
total_min_per_day <- 1440

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
    mutate(total_energy = cost_of_pup$pup_cost + Growth + actual_costs_activity + foraging_cost + new_resting_cost)
  
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
#   MR_*, metabolic_rates_*, min_per_day_*, perc_time_*, actual_costs_*
make_wide_data <- function(mass_lifestage_budget) {
  mass_lifestage_budget_wide <- mass_lifestage_budget %>%
    # Make dataframe in a wide format
    pivot_wider(names_from = Behaviour, values_from=c(perc_time, min_per_day, MR, metabolic_rates, actual_costs)) %>% 
    mutate(resting_cost_wout_foraging = metabolic_rates_rest * (min_per_day_rest + min_per_day_forage)) %>% 
    # Calculate sum of actual costs
    mutate(sum_actual_costs = actual_costs_rest + actual_costs_activity + actual_costs_forage) %>% 
    # Modify pup costs
    mutate(pup_cost = ifelse(with.pup == 'yes', cost_of_pup, 0)) %>% 
    # Calculate ROR
    mutate(ROR = (sum_actual_costs + cost_of_pup$pup_cost + Growth)/(min_per_day_forage)) %>%
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

## Sensitivity Analysis Functions--------------------

# Change_var is the column you want to change, df is the data frame that holds that column (either wide_data or ROR_og)

make_change_var_df <- function(change_var, df){ 
  change_df <- df %>% select(Age, Sex, Lifestage, with.pup, Change_var = all_of(change_var))
  
  return(change_df)
}

# Set up dataframe to store all the random values (rep = 1 will be original data)
make_random_samples <- function(sample_size, sd_change = 0.1, change_df){
  
  change_all_reps <- change_df
  change_all_reps$rep <- 1 
  
  #  Adds the random values to data frame based on normal distribution 
  for (i in 1:dim(change_df)[1]){
    # Randomly sample from normal distribution 
    changed_vals <- data.frame(Change_var = rnorm(sample_size, 
                                                  mean = change_df$Change_var[i], 
                                                  sd = change_df$Change_var[i]*sd_change))
    changed_vals$rep <- 2:(sample_size+1)
    changed_vals$Age <- change_df$Age[i]
    changed_vals$Sex <- change_df$Sex[i]
    changed_vals$Lifestage <- change_df$Lifestage[i]
    changed_vals$with.pup <- change_df$with.pup[i]
    
    change_all_reps <- rbind(change_all_reps, changed_vals)
  }
  
  return(change_all_reps)
}

# Replace the old column for the new values based on current replicate

run_model_with_reps <- function(change_all_reps, df, sample_size, change_var){
  
  final_data <- data.frame()
  
  for (i in 1:(sample_size+1)){
    
    new_df <- df %>% 
      select(-all_of(change_var)) %>% 
      merge((change_all_reps %>% 
               filter(rep == i) %>% 
               select(-rep))) %>% 
      rename(!!change_var := Change_var)
    
    # Run model for given values
    model_setup <- build_model(budget_data = new_df, ROR = ROR_og)
    output <- run_model(model_setup)
    output$rep <- i
    
    final_data <- rbind(final_data, output)
  }
  
  return(final_data)
}

# Data------------------------------------------------------------------
masses <- read.csv(file ='mass_growth.csv') %>% 
  filter(!is.na(Age)) # remove empty rows

act_budgets <- read.csv(file = 'ActivityBudgets.csv') %>% 
  filter(!is.na(MR)) # remove empty rows

age_convert <- read.csv(file = 'age_lifestage.csv')

# Model----------------------------------------------------------
model.run.1 <- otter_model(masses = masses,
                           act_budgets = act_budgets,
                           age_convert = age_convert)


# Sensitivity Analysis of Model Parameters---------------------------------------

# Variables that can change
#  1. pupcost - a constant variable
#  2. growth - in masses dataframe
#  3. mass - in masses dataframe
#  4. foraging activity? - in act_budgets dataframe
#  5. metabolic rate? - in act_budgets dataframe


# ** Setup SA -> Select which variable to run **
set.seed(222)

# Set variable that you are changing 
change_var <- cost_of_pup

# set number of random samples 
sample_size <- 1000

#Variation
var <- 0.05

model_all_reps <- data.frame()
for (rep_num in c(1:sample_size)) {
 
  cost_of_puppies = data.frame(cost_of_pup = change_var)
  cost_of_pup <- cost_of_puppies %>% 
              mutate(pup_cost = rnorm(1, change_var, sd = 0.05)) 
  
  # Uncomment if changing the act_budgets df
  # act_budgets_rep <- act_budgets %>% 
  #   group_by(Sex, Lifestage, with.pup, Behaviour) %>% 
  #   mutate(MR = rnorm(1, MR, sd = 0.1)) %>% 
  #   ungroup()
  model_rep <- otter_model(masses = masses,
                           act_budgets = act_budgets,
                           age_convert = age_convert) %>% 
    mutate(rep_num = rep_num) %>% 
    relocate(rep_num)
  
  model_all_reps <- bind_rows(model_all_reps, model_rep)
}

model_all_reps$pup_cost <- as.numeric(model_all_reps$pup_cost)
# make the pup.cost column a number not a list
# write.csv(model_all_reps, paste0("model_all_reps_", change_var, "_", var, ".csv"))

# TODO: create loops for other variables to test
# TODO: make SA metrics work (RMSE)

# ----------------------- Analyze SA results -----------------
# Set up for RMSE

#Original data is set as rep 1
data.true <- model_all_reps %>% 
  filter(rep_num == 1) %>% 
  select(Age, Sex, with.pup, total_energy)

#All replicates except original in one table
data.reps <- model_all_reps %>% 
  filter(rep_num != 1) %>% 
  select(Age, Sex, with.pup, total_energy, rep_num)

# select one rep (can be a group_by later)
data.sample <- data.reps #%>% 

calc_rmse <- function(data.true, data.sample) {
  rmse <- sqrt( mean((data.true - data.sample)^2) )
  return(rmse)
}

# -- DO this per life stage via group_by
calc_rmse(data.true$total_energy, data.sample$total_energy)

#Can group by age and see what ages the model is most accurate for (Patrick's work)
#total variation in output and by age./sex to see where variation is highest
#Do this after i look at each parameter

# ---- Graphs ----
ggplot(filter(model_all_reps, Sex == "M"), 
       aes(x = Age, y = total_energy,
           color = as.factor(rep_num))) +
  geom_line() +
  theme(legend.position = "none")


#  Plot to explore 
final_data$rep <- as.factor(final_data$rep)

growth_plot <- ggplot(filter(final_data, Sex == "M" ), 
                      aes(x = Age, y = total_energy, col = rep)) + 
  geom_point() +
  theme(legend.position = "none")+
  #geom_errorbar()- Needs ymin, ymax,
  
  growth_plot


# ---- old codes----
# provide original dataset
df <- wide_data

# isolate variable to change
change_df <- make_change_var_df(change_var, df = df)

# create replicate simulations of the changed variable
change_all_reps <- make_random_samples(sample_size, sd_change = 0.1, change_df)

# run the model
final_data <- run_model_with_reps(change_all_reps = change_all_reps,  df = df, 
                                  sample_size = sample_size,
                                  change_var = change_var)



# confirm if rep sampling worked
A <- change_all_reps %>% 
  pivot_wider(names_from = rep, values_from = Change_var)
AB <- final_data %>% 
  select(Age, Sex, Lifestage, with.pup, rep, total_energy) %>% 
  pivot_wider(names_from = rep, values_from = total_energy)

