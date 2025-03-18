# Chapter 3 Prey energy calculations for diet analysis

#Date Created: March 2025

#Author: Julia Adelsheim
#Collaborators: Andreas Novotny 

# Prey Energy Calculations from Otter Model v.26 in excel
# This is to set up the diet analysis using model output values later using a few 
#  of the values calculated in this script. 

rm(list = ls())

# Set Working Directory ------------------------------------------------
setwd("~/Documents/Thesis/otteR/Data")

# Data ----------------------------------------------------------------
diet_scenarios <- read.csv(file ='Diet_Analysis.csv') 

# Subset Diet Scenarios -----------------------------------------------------
# In app- diet scenario(s) are changeable- 
#  User should be able to edit: 
# - species
# - proportion of diet
# - edible portion
# - mass of edible portion 
# - energy density

#   

#Diet 1 ----------------------------------------------------------------
diet1 <- diet_scenarios %>% 
  filter (diet == "1") %>% 
  #calculate ingested mass per 100 items consumed
  mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
  mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
  #calculate the proportion of edible biomass for each prey item
  # so of total biomass, what proportion is each species within the diet scenario
  mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
  #calculate original (og) mass of prey items by species 
  mutate(og_mass = mass_edible/portion_edible) %>% 
  #calculate the original (og) mass ingested per 100 items consumed
  mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
  #calculate sum of original mass 
  mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
  #calculate the proportion of biomass from original mass calcs
  mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
  #calculate the energy contribution of each species in the diet (kJ)
  mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
  #calculate total energy density of diet
  mutate(total_energy_density = sum(energy_contribution)) %>% 
  #calculate the total captured biomass (versus ingested) (g/kJ)
  # so for each kJ of energy, how many grams of each species is contributing
  mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
  mutate(total_captured_biomass = sum(captured_biomass)) %>% 
  #calculates total captured mass per 100kJ of energy intake
  mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
  #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
  mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
  mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)

#Diet 2 ----------------------------------------------------------------
diet2 <- diet_scenarios %>% 
  filter (diet == "2") %>% 
  #calculate ingested mass per 100 items consumed
  mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
  mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
  #calculate the proportion of edible biomass for each prey item
  # so of total biomass, what proportion is each species within the diet scenario
  mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
  #calculate original (og) mass of prey items by species 
  mutate(og_mass = mass_edible/portion_edible) %>% 
  #calculate the original (og) mass ingested per 100 items consumed
  mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
  #calculate sum of original mass 
  mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
  #calculate the proportion of biomass from original mass calcs
  mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
  #calculate the energy contribution of each species in the diet (kJ)
  mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
  #calculate total energy density of diet
  mutate(total_energy_density = sum(energy_contribution)) %>% 
  #calculate the total captured biomass (versus ingested) (g/kJ)
  # so for each kJ of energy, how many grams of each species is contributing
  mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
  mutate(total_captured_biomass = sum(captured_biomass)) %>% 
  #calculates total captured mass per 100kJ of energy intake
  mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
  #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
  mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
  mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)


#Diet 3 ----------------------------------------------------------------
diet3 <- diet_scenarios %>% 
  filter (diet == "3") %>% 
  #calculate ingested mass per 100 items consumed
  mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
  mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
  #calculate the proportion of edible biomass for each prey item
  # so of total biomass, what proportion is each species within the diet scenario
  mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
  #calculate original (og) mass of prey items by species 
  mutate(og_mass = mass_edible/portion_edible) %>% 
  #calculate the original (og) mass ingested per 100 items consumed
  mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
  #calculate sum of original mass 
  mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
  #calculate the proportion of biomass from original mass calcs
  mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
  #calculate the energy contribution of each species in the diet (kJ)
  mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
  #calculate total energy density of diet
  mutate(total_energy_density = sum(energy_contribution)) %>% 
  #calculate the total captured biomass (versus ingested) (g/kJ)
  # so for each kJ of energy, how many grams of each species is contributing
  mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
  mutate(total_captured_biomass = sum(captured_biomass)) %>% 
  #calculates total captured mass per 100kJ of energy intake
  mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
  #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
  mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
  mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)

#Diet 4 ----------------------------------------------------------------
diet4 <- diet_scenarios %>% 
  filter (diet == "4") %>% 
  #calculate ingested mass per 100 items consumed
  mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
  mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
  #calculate the proportion of edible biomass for each prey item
  # so of total biomass, what proportion is each species within the diet scenario
  mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
  #calculate original (og) mass of prey items by species 
  mutate(og_mass = mass_edible/portion_edible) %>% 
  #calculate the original (og) mass ingested per 100 items consumed
  mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
  #calculate sum of original mass 
  mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
  #calculate the proportion of biomass from original mass calcs
  mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
  #calculate the energy contribution of each species in the diet (kJ)
  mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
  #calculate total energy density of diet
  mutate(total_energy_density = sum(energy_contribution)) %>% 
  #calculate the total captured biomass (versus ingested) (g/kJ)
  # so for each kJ of energy, how many grams of each species is contributing
  mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
  mutate(total_captured_biomass = sum(captured_biomass)) %>% 
  #calculates total captured mass per 100kJ of energy intake
  mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
  #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
  mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
  mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)

#Diet 5 ----------------------------------------------------------------
diet5 <- diet_scenarios %>% 
  filter (diet == "5") %>% 
  #calculate ingested mass per 100 items consumed
  mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
  mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
  #calculate the proportion of edible biomass for each prey item
  # so of total biomass, what proportion is each species within the diet scenario
  mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
  #calculate original (og) mass of prey items by species 
  mutate(og_mass = mass_edible/portion_edible) %>% 
  #calculate the original (og) mass ingested per 100 items consumed
  mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
  #calculate sum of original mass 
  mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
  #calculate the proportion of biomass from original mass calcs
  mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
  #calculate the energy contribution of each species in the diet (kJ)
  mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
  #calculate total energy density of diet
  mutate(total_energy_density = sum(energy_contribution)) %>% 
  #calculate the total captured biomass (versus ingested) (g/kJ)
  # so for each kJ of energy, how many grams of each species is contributing
  mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
  mutate(total_captured_biomass = sum(captured_biomass)) %>% 
  #calculates total captured mass per 100kJ of energy intake
  mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
  #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
  mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
  mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)

  



