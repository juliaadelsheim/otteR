#Sensitivity Analysis for Growth

#Date Created: 

#Author: Julia Adelsheim
#Collaborators: Patrick Pata, Andreas Novotny, Izzy Morgante

##Start-----------------------------------------------------------------------

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

# TODO change values for standard deviation later
# stdev_MR_perc_time <- read.csv(file= 'Stdev_MR_perc_time.csv') %>%
#   filter(!is.na(stdev_perc_time)) %>%
#   filter(!is.na(stdev_MR))

# Functions ------------------------------------------------------------

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
# This is the defaul model run with literature mean values for the parameters.
model.run.1 <- otter_model(masses = masses,
                           act_budgets = act_budgets,
                           age_convert = age_convert,
                           cost_pup = 3931)

# Sensitivity Analysis of Model Parameters---------------------------------------

# Variables that can change
#  1. growth - in masses dataframe
#  2. mass - in masses dataframe
#  3. foraging and resting - in act_budgets dataframe
#  4. metabolic rate? - in act_budgets dataframe

# ** Setup SA -> Select which variable to run **
set.seed(22222)

# PP Notes Dec 16
# 1. For each iteration, randomly generate values for the variables in the sensitivity analysis
# 2. Run the model for these values 
# 3. Then store the outputs to a data frame that marks the run number
# 4. Do 2 versions of SA: 1-all parameters vary at the same time; 2-each parameter varies independently
# 5. For each parameter, do 4 versions of the stdev (5%, 10%, 20%, and literature value)


# V1 and V3. SA change values based on literature by changing values one at a time
# PP: note that here, the stdev values merged to the averages might be changed later
#     Also note that in randomly sampling the percent time, the total of the 
#     activities can be not equal to 1. 
#     ! critical, percent time can be negative after doing the stdev
sens_analysis_lit <- function(sample_size, 
                              param2vary) {
  
  model_all_reps <- data.frame()
  for (rep_num in c(1:sample_size)) {
    
    # When a parameter is not being varied, use default values
    masses_rep <- masses 
    act_budgets_rep <- act_budgets %>% 
      left_join(stdev_MR_perc_time, by = c('Sex', 'Lifestage', 'with.pup', "Behaviour"))
    
    if ("growth" %in% param2vary){
      masses_rep <- masses_rep %>%  
        group_by(Age, Sex) %>%
        mutate(Growth = rnorm(1, Growth, sd = stdev_growth)) %>%
        ungroup()
    }
    
    if ("mass" %in% param2vary){
      masses_rep <- masses_rep %>%  
        group_by(Age, Sex) %>%
        mutate(Av_mass = rnorm(1, Av_mass, sd = stdev_mass)) %>%
        ungroup()
    }
    
    if ("MR" %in% param2vary){
      act_budgets_rep <- act_budgets_rep %>%
        group_by(Sex, Lifestage, with.pup, Behaviour) %>%
        mutate(MR =  rnorm(1, mean = MR, sd = stdev_MR)) %>% 
        ungroup() 
    }
    
    if("perc_time" %in% param2vary){
      act_budgets_rep <- act_budgets_rep %>%
        group_by(Sex, Lifestage, with.pup, Behaviour) %>%
        mutate(perc_time =  rnorm(1, mean = perc_time, sd = stdev_perc_time)) %>% 
        ungroup() 
    }
    
    
    # Make sure percent time totals to 1 and remove unncessary columns
    act_budgets_rep <- act_budgets_rep %>%
      group_by(Sex, Lifestage, with.pup) %>%
      mutate(total_perc_time = sum(perc_time)) %>%
      ungroup() %>% 
      mutate(perc_time = perc_time / total_perc_time) %>% 
      dplyr::select(all_of(colnames(act_budgets)))
    

    # This runs the model with the randomly varied parameters
    model_rep <- otter_model(masses = masses_rep,
                             act_budgets = act_budgets_rep,
                             age_convert = age_convert,
                             cost_pup = 3931) %>%
      mutate(rep_num = rep_num) %>%
      relocate(rep_num)
    
    model_all_reps <- bind_rows(model_all_reps, model_rep)
    
  }
  return(model_all_reps)
}


# V2 and V4. SA by changing % of the mean for each parameter at a time or all together
# The value for perc_sd are fractions (0.05, 0.1, 0.20)
# values for param2vary are: c(growth, mass, MR, perc_time). 
#   If varying all, include all there values in the array.
sens_analysis_perc_mean <- function(sample_size, perc_sd,
                                           param2vary) {
  
  model_all_reps <- data.frame()
  for (rep_num in c(1:sample_size)) {
    
    # When a parameter is not being varied, use default values
    masses_rep <- masses 
    act_budgets_rep <- act_budgets %>% 
      left_join(stdev_MR_perc_time, by = c('Sex', 'Lifestage', 'with.pup', "Behaviour"))
    
    if ("growth" %in% param2vary){
      masses_rep <- masses_rep %>%  
        group_by(Age, Sex) %>%
        mutate(Growth = rnorm(1, Growth, sd = Growth*perc_sd)) %>%
        ungroup()
    }
    
    if ("mass" %in% param2vary){
      masses_rep <- masses_rep %>%  
        group_by(Age, Sex) %>%
        mutate(Av_mass = rnorm(1, Av_mass, sd = Av_mass*perc_sd)) %>%
        ungroup()
    }
    
    if ("MR" %in% param2vary){
      act_budgets_rep <- act_budgets_rep %>%
        group_by(Sex, Lifestage, with.pup, Behaviour) %>%
        mutate(MR =  rnorm(1, mean = MR, sd = MR*perc_sd)) %>% 
        ungroup() 
    }
    
    if("perc_time" %in% param2vary){
      act_budgets_rep <- act_budgets_rep %>%
        group_by(Sex, Lifestage, with.pup, Behaviour) %>%
        mutate(perc_time =  rnorm(1, mean = perc_time, sd = perc_time*perc_sd)) %>% 
        ungroup() 
    }
    
    if("cost_pup" %in% param2vary) {
      cost_pup <- rnorm(1, mean = 3931, sd = 3931*perc_sd)
    } else {
      cost_pup <- 3931
    }
   
    # Make sure percent time totals to 1 and remove unnecessary columns
    act_budgets_rep <- act_budgets_rep %>%
      group_by(Sex, Lifestage, with.pup) %>%
      mutate(total_perc_time = sum(perc_time)) %>%
      ungroup() %>% 
      mutate(perc_time = perc_time / total_perc_time) %>% 
      dplyr::select(all_of(colnames(act_budgets)))
    
    # This runs the model with the randomly varied parameters
    model_rep <- otter_model(masses = masses_rep,
                             act_budgets = act_budgets_rep,
                             age_convert = age_convert,
                             cost_pup = cost_pup) %>%
      mutate(rep_num = rep_num) %>%
      relocate(rep_num)
    
    model_all_reps <- bind_rows(model_all_reps, model_rep)
    
  }
  return(model_all_reps)
}

# Run SA ---------------

# V1. Literature stdevs, vary all params at the same time
testAll <- sens_analysis_lit(sample_size = 1000,
                             param2vary = c("growth","mass","MR","perc_time", "cost_pup"))

# V2. Percent of mean stdevs, vary all params at the same time
testAll <- sens_analysis_perc_mean(sample_size = 100, perc_sd = 0.10,
                                   param2vary = c("growth","mass","MR","perc_time", "cost_pup"))

# V3. Literature stdevs, vary one param at a time
testAll <- sens_analysis_lit(sample_size = 99,
                             param2vary = c("growth"))

# V4. Percent of mean stdevs, vary one param at a time
testAll <- sens_analysis_perc_mean(sample_size = 99, perc_sd = 0.2,
                                   param2vary = c("cost_pup"))

# ***Process SA results***
# TODO calculate the average and standard error across replicates
SA_results <- testAll %>% 
  pivot_longer(cols = c(total_energy, Growth:new_resting_cost),
               names_to = "parameter", values_to = "value") %>% 
  group_by(Sex, Age, Lifestage, with.pup, parameter) %>% #deleted pup_cost
  summarise(value_mean = mean(value),
            value_sd = sd(value),
            n = n(),
            .groups = "drop") %>% 
  # Calculate standard error and 95% confidence intervals
  mutate(value_SE = value_sd/sqrt(n),
         lower.ci = value_mean - qt(1 - (0.05 / 2), n - 1) * value_SE,
         upper.ci = value_mean + qt(1 - (0.05 / 2), n - 1) * value_SE) %>% 
  # group types of otters
  mutate(otter_type = paste(Sex, with.pup))


# Plot total_energy
ggplot(SA_results %>% 
         filter(parameter == "total_energy"),
       aes (x = Age, y = value_mean,
            color = otter_type)) +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci)) +
  geom_point() +
  geom_line()
   # geom_smooth(aes(ymin = lower.ci, ymax = upper.ci))
