# Chapter 3 Prey energy calculations for diet analysis

#Date Created: March 2025

#Author: Julia Adelsheim
#Collaborators: Andreas Novotny 

# Prey Energy Calculations from Otter Model v.26 in excel
# This is to set up the diet analysis using model output values later using a few 
#  of the values calculated in this script. 

# Diet Scenarios are: 
# Diet 1 = Cancer spp crab and abalone
# Diet 2 = Kelp crab and bivalves
# Diet 3 = Marine snails
# Diet 4 = Urchin
# Diet 5 = Average diet
# 
# Sources: 
#   Diets 1-3: Tinker et al., 2007
#   Diet 4: Fujii et al., 2017
#   Diet 5: Tinker, 2004
#   Energy density, mass, % edible values: Oftedal et al., 2007

# In app- diet scenario(s) is(are) changeable- 
#  User should be able to edit: 
# - species (of prey items)
# - energy density of prey item
# - proportion of diet represented by each prey item
# - edible portion of prey item
# - mass of edible portion of prey item

rm(list = ls())

# Set Working Directory ------------------------------------------------
setwd("~/Documents/Thesis/otteR/Data")

# Data ----------------------------------------------------------------
diet_scenarios <- read.csv(file ='Diet_Analysis.csv') 

# Prey Energy Calculations -----------------------------------------------------
# These calcs are done to calculate average values to compare between diet scenarios

prey_energy_calcs <- diet_scenarios %>% 
  # to separate out calculations by diet scenario
  group_by(diet) %>% 
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

# Save Prey Energy Calcs ----------------------------------------------------- 
folder_path <- "~/Documents/Thesis/otteR/Results"
write.csv(prey_energy_calcs, file.path(folder_path, "prey_energy_calcs.csv"), row.names=FALSE)

# --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
# To calculate the # of prey items of each species, we have to: divide the gross energy
#   by the specific diet's total energy density, then multiply that by the 
#   proportional edible biomass of the prey item divided by the ave mass of the edible portion 
#   of that species. So I think it was easiest to make multipliers for each species 

# Species Multipliers ---------------------------------------------------------------
#Create multipliers for each prey species 

abalone <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "abalone") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 

# TODO FIgure out this chunk of code, how to isolate one cell to multiply it
   mutate(prop_edible_biomass_diet1 = ifelse(diet == "1",
                                            filter(species == "abalone" %>% 
                                            pull(prop_edible_biomass))))

cancer_crab <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "cancer_crabs") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density))

kelp_crab <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "kelp_crab") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density))

urchin <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "urchin") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density))

clam <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "clam") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density))

mussel <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "mussel") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density))

turban_snail <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "turban_snail") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) 
  
  

# --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
# Next step is to either read in TEE output csv or run the model to get TEE outputs
# Model Output ----------------------------------------------------------

#Choose model results you want to use
model_results <- read.csv("~/Documents/Thesis/otteR/Results/original_model_run_new.csv") 

# Read in mass data so we can calculate % body mass by diet 
masses <- read.csv(file ='mass_growth.csv') 

# Add mass to model outputs 
model_results <- model_results %>% 
  left_join(masses %>% select(Age, Sex, Av_mass), by = c("Age","Sex"))

# Subset Diets ------------------------------------------------------------
# We want to use the prey energy values that correspond to each specific diet 

diet1 <- prey_energy_calcs %>% 
  filter(diet == 1)

diet2 <- prey_energy_calcs %>% 
  filter(diet == 2)

diet3 <- prey_energy_calcs %>% 
  filter(diet == 3)

diet4 <- prey_energy_calcs %>% 
  filter(diet == 4)

diet5 <- prey_energy_calcs %>% 
  filter(diet == 5)

# Diet Analysis ------------------------------------------------------------------

# Diet 1 --------------------------------------------------------------------------
diet1_analysis <- model_results %>% 
  # Clean up df 
  select(Sex, Age, Lifestage, with.pup, total_energy, Av_mass) %>% 
  #rename total energy to net energy expenditure
  rename(net_energy = total_energy) %>% 
  #convert to gross energy, assuming 40% energy losses to digestion, urine, feces
  mutate(gross_energy = (net_energy/0.6)) %>% 
  #calculate ingested food mass (IFM) (g)- there is a warning, but the calculation is correct, 
  #  so you can ignore the warning.
  mutate(ingested_food_mass = (gross_energy/diet1$total_energy_density)) %>% 
  #calculate total captured biomass (g)- same issue with warning, you can ignore it
  mutate(total_captured_biomass = (ingested_food_mass * diet1$total_captured_biomass)) %>% 
  #calculate ingested food mass (IFM) as % body mass
  mutate(IFM_perc_body_mass = (ingested_food_mass / (Av_mass*1000))*100)
  
 # Prey Item #'s ------------------------------------------------------------------

diet1_analysis <- diet1_analysis %>% 
  mutate(abalone = gross_energy/ ((diet1$total_energy_density) *
                                    (diet1$prop_edible_biomass1 /abalone$mass_edible)))
 








  
  
# 
# #Diet 2 ----------------------------------------------------------------
# diet2 <- diet_scenarios %>% 
#   filter (diet == "2") %>% 
#   #calculate ingested mass per 100 items consumed
#   mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
#   mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
#   #calculate the proportion of edible biomass for each prey item
#   # so of total biomass, what proportion is each species within the diet scenario
#   mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
#   #calculate original (og) mass of prey items by species 
#   mutate(og_mass = mass_edible/portion_edible) %>% 
#   #calculate the original (og) mass ingested per 100 items consumed
#   mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
#   #calculate sum of original mass 
#   mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
#   #calculate the proportion of biomass from original mass calcs
#   mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
#   #calculate the energy contribution of each species in the diet (kJ)
#   mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
#   #calculate total energy density of diet
#   mutate(total_energy_density = sum(energy_contribution)) %>% 
#   #calculate the total captured biomass (versus ingested) (g/kJ)
#   # so for each kJ of energy, how many grams of each species is contributing
#   mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
#   mutate(total_captured_biomass = sum(captured_biomass)) %>% 
#   #calculates total captured mass per 100kJ of energy intake
#   mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
#   #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
#   mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
#   mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)
# 
# 
# #Diet 3 ----------------------------------------------------------------
# diet3 <- diet_scenarios %>% 
#   filter (diet == "3") %>% 
#   #calculate ingested mass per 100 items consumed
#   mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
#   mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
#   #calculate the proportion of edible biomass for each prey item
#   # so of total biomass, what proportion is each species within the diet scenario
#   mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
#   #calculate original (og) mass of prey items by species 
#   mutate(og_mass = mass_edible/portion_edible) %>% 
#   #calculate the original (og) mass ingested per 100 items consumed
#   mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
#   #calculate sum of original mass 
#   mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
#   #calculate the proportion of biomass from original mass calcs
#   mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
#   #calculate the energy contribution of each species in the diet (kJ)
#   mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
#   #calculate total energy density of diet
#   mutate(total_energy_density = sum(energy_contribution)) %>% 
#   #calculate the total captured biomass (versus ingested) (g/kJ)
#   # so for each kJ of energy, how many grams of each species is contributing
#   mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
#   mutate(total_captured_biomass = sum(captured_biomass)) %>% 
#   #calculates total captured mass per 100kJ of energy intake
#   mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
#   #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
#   mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
#   mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)
# 
# #Diet 4 ----------------------------------------------------------------
# diet4 <- diet_scenarios %>% 
#   filter (diet == "4") %>% 
#   #calculate ingested mass per 100 items consumed
#   mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
#   mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
#   #calculate the proportion of edible biomass for each prey item
#   # so of total biomass, what proportion is each species within the diet scenario
#   mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
#   #calculate original (og) mass of prey items by species 
#   mutate(og_mass = mass_edible/portion_edible) %>% 
#   #calculate the original (og) mass ingested per 100 items consumed
#   mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
#   #calculate sum of original mass 
#   mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
#   #calculate the proportion of biomass from original mass calcs
#   mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
#   #calculate the energy contribution of each species in the diet (kJ)
#   mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
#   #calculate total energy density of diet
#   mutate(total_energy_density = sum(energy_contribution)) %>% 
#   #calculate the total captured biomass (versus ingested) (g/kJ)
#   # so for each kJ of energy, how many grams of each species is contributing
#   mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
#   mutate(total_captured_biomass = sum(captured_biomass)) %>% 
#   #calculates total captured mass per 100kJ of energy intake
#   mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
#   #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
#   mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
#   mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)
# 
# #Diet 5 ----------------------------------------------------------------
# diet5 <- diet_scenarios %>% 
#   filter (diet == "5") %>% 
#   #calculate ingested mass per 100 items consumed
#   mutate(ing_mass_per100items = mass_edible * diet_proportion) %>% 
#   mutate(sum_ing_mass_per100items = sum(ing_mass_per100items)) %>% 
#   #calculate the proportion of edible biomass for each prey item
#   # so of total biomass, what proportion is each species within the diet scenario
#   mutate(prop_edible_biomass = ing_mass_per100items/sum_ing_mass_per100items) %>% 
#   #calculate original (og) mass of prey items by species 
#   mutate(og_mass = mass_edible/portion_edible) %>% 
#   #calculate the original (og) mass ingested per 100 items consumed
#   mutate(og_ing_mass_per100_items = og_mass * diet_proportion) %>% 
#   #calculate sum of original mass 
#   mutate(sum_og_ing_mass_per100_items = sum(og_ing_mass_per100_items)) %>% 
#   #calculate the proportion of biomass from original mass calcs
#   mutate(prop_og_biomass = og_ing_mass_per100_items/sum_og_ing_mass_per100_items) %>% 
#   #calculate the energy contribution of each species in the diet (kJ)
#   mutate(energy_contribution = energy_density * prop_edible_biomass) %>% 
#   #calculate total energy density of diet
#   mutate(total_energy_density = sum(energy_contribution)) %>% 
#   #calculate the total captured biomass (versus ingested) (g/kJ)
#   # so for each kJ of energy, how many grams of each species is contributing
#   mutate(captured_biomass= prop_edible_biomass/portion_edible) %>% 
#   mutate(total_captured_biomass = sum(captured_biomass)) %>% 
#   #calculates total captured mass per 100kJ of energy intake
#   mutate(cap_mass_per100kJ = (total_captured_biomass/total_energy_density)*100) %>% 
#   #calculate the ecological ratio, which is the proportion of captured vs ingested biomass
#   mutate(ecological_ratio = sum_og_ing_mass_per100_items/sum_ing_mass_per100items) %>% 
#   mutate(ing_mass_per100kJ = cap_mass_per100kJ/ecological_ratio)
# 
#   
# 
# 
# 
