# Chapter 3 Prey energy calculations for diet analysis

#Date Created: March 2025

#Author: Julia Adelsheim
#Collaborators: Andreas Novotny 

# Notes -------------------------------------------------------------------------------
# Prey Energy Calculations from Otter Model v.26 in excel
# This is to set up the diet analysis using model output values later using a few 
#  of the values calculated in this script. 

#  Diet Scenarios are: 
#  Diet 1 = Cancer spp crab and abalone
#  Diet 2 = Kelp crab and bivalves
#  Diet 3 = Marine snails
#  Diet 4 = Urchin
#  Diet 5 = Average diet
# 
#  Sources: 
#    Diets 1-3: Tinker et al., 2007
#    Diet 4: Fujii et al., 2017
#    Diet 5: Tinker, 2004
#    Energy density, mass, % edible values: Oftedal et al., 2007

# In app- diet scenario(s) is(are) changeable- 
#  User should be able to edit: 
# - species (of prey items)
# - energy density of prey item
# - proportion of diet represented by each prey item
# - edible portion of prey item
# - mass of edible portion of prey item

#rm(list = ls())

# Set Working Directory ------------------------------------------------
#setwd("~/Documents/Thesis/otteR/Data")
library(tidyverse)

# Data ----------------------------------------------------------------
diet_scenarios <- read.csv(file ='Data/Diet_Analysis.csv') 

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

# Save Prey Energy Calcs 
#folder_path <- "~/Documents/Thesis/otteR/Results"
write.csv(prey_energy_calcs, "Results/prey_energy_calcs.csv", row.names=FALSE)

# --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 

# To calculate the # of prey items of each species, we have to: divide the gross energy
#   by the specific diet's total energy density, then multiply that by the 
#   proportional edible biomass of the prey item divided by the ave mass of the edible portion 
#   of that species. So I think it was easiest to make "multipliers" for each species 

# Subset Prey Species-----------------------------------------------------------------------------
#Create multipliers for each prey species 

# Select out the proportion of edible biomass calculations needed to calculate 
#   number of prey items consumed 

prop_edible_biomass_df <- prey_energy_calcs %>%
  # Select relevant columns
  select(diet, species, prop_edible_biomass) %>%  
  # Sort by species
  arrange(species) %>% 
  # Separate by diet
  pivot_wider(names_from = diet, values_from = prop_edible_biomass)

# Now create a df of values for each species 

abalone <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "abalone") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 
  right_join(., prop_edible_biomass_df, by = "species") %>% 
  filter(species == "abalone") %>% 
  rename(diet_1 = "1", diet_2 = "2", diet_3 = "3", diet_4 = "4", diet_5 = "5")
         
cancer_crab <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "cancer_crabs") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 
  right_join(., prop_edible_biomass_df, by = "species") %>% 
  filter(species == "cancer_crabs") %>% 
  rename(diet_1 = "1", diet_2 = "2", diet_3 = "3", diet_4 = "4", diet_5 = "5")
 

kelp_crab <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "kelp_crab") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 
  right_join(., prop_edible_biomass_df, by = "species") %>% 
  filter(species == "kelp_crab") %>% 
  rename(diet_1 = "1", diet_2 = "2", diet_3 = "3", diet_4 = "4", diet_5 = "5")

urchin <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "urchin") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 
  right_join(., prop_edible_biomass_df, by = "species") %>% 
  filter(species == "urchin") %>% 
  rename(diet_1 = "1", diet_2 = "2", diet_3 = "3", diet_4 = "4", diet_5 = "5")

clam <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "clam") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 
  right_join(., prop_edible_biomass_df, by = "species") %>% 
  filter(species == "clam") %>% 
  rename(diet_1 = "1", diet_2 = "2", diet_3 = "3", diet_4 = "4", diet_5 = "5")

mussel <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "mussel") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 
  right_join(., prop_edible_biomass_df, by = "species") %>% 
  filter(species == "mussel") %>% 
  rename(diet_1 = "1", diet_2 = "2", diet_3 = "3", diet_4 = "4", diet_5 = "5")

turban_snail <- diet_scenarios %>% 
  group_by(species) %>% 
  filter(species == "turban_snail") %>% 
  select(!diet_proportion) %>% 
  select(!diet) %>% 
  summarise(portion_edible = mean(portion_edible), 
            mass_edible = mean(mass_edible), 
            energy_density = mean(energy_density)) %>% 
  right_join(., prop_edible_biomass_df, by = "species") %>% 
  filter(species == "turban_snail") %>% 
  rename(diet_1 = "1", diet_2 = "2", diet_3 = "3", diet_4 = "4", diet_5 = "5")

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

# This is to have an easy to call total energy density of each diet 
#  (clunky but only way I could figure out how to get code to work)
energy_density_all_diets <- prey_energy_calcs %>% 
  summarise(mean(total_energy_density)) %>% 
  pivot_wider(names_from ="diet", values_from = "mean(total_energy_density)")


# Model Output ---------------------------------------------------------------------------
# Next step is to either read in TEE output csv or run the model to get TEE outputs

#Choose model results you want to use- need total energy expenditure values
model_results <- read.csv("Results/original_model_run_new.csv") 

# Read in mass data so we can calculate % body mass by diet 
masses <- read.csv(file ='Data/mass_growth.csv') 

# Add mass to model outputs 
model_results <- model_results %>% 
  left_join(masses %>% select(Age, Sex, Av_mass), by = c("Age","Sex"))
 

# Diet Analysis ------------------------------------------------------------------

# Calculate out all of the prey items by species and within each diet

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
  mutate(IFM_perc_body_mass = (ingested_food_mass / (Av_mass*1000))*100) %>% 
 
   #Now calculate prey item numbers:
  
  #calculate number of abalone 
  mutate(abalone = ((gross_energy/(energy_density_all_diets$"1")) 
                    * (abalone$diet_1/abalone$mass_edible))) %>% 
  #calculate number of Cancer crab
  mutate(cancer_crabs = ((gross_energy/(energy_density_all_diets$"1")) 
                    * (cancer_crab$diet_1/cancer_crab$mass_edible))) %>% 
  #calculate number of kelp crab
  mutate(kelp_crab = ((gross_energy/(energy_density_all_diets$"1")) 
                         * (kelp_crab$diet_1/kelp_crab$mass_edible))) %>%
  #calculate number of urchin
  mutate(urchin = ((gross_energy/(energy_density_all_diets$"1")) 
                         * (urchin$diet_1/urchin$mass_edible))) %>%
  #calculate number of clam
  mutate(clam = ((gross_energy/(energy_density_all_diets$"1")) 
                   * (clam$diet_1/clam$mass_edible))) %>%
  #calculate number of mussels
  mutate(mussel = ((gross_energy/(energy_density_all_diets$"1")) 
                   * (mussel$diet_1/mussel$mass_edible))) %>%
  #calculate number of snails
  mutate(turban_snail = ((gross_energy/(energy_density_all_diets$"1")) 
                   * (turban_snail$diet_1/turban_snail$mass_edible))) 
 
#Save output
#folder_path <- "~/Documents/Thesis/otteR/Results"
write.csv(diet1_analysis, "Results/Diet1_Analysis.csv", row.names=FALSE)

# Diet 2 ----------------------------------------------------------------

diet2_analysis <- model_results %>% 
  # Clean up df 
  select(Sex, Age, Lifestage, with.pup, total_energy, Av_mass) %>% 
  #rename total energy to net energy expenditure
  rename(net_energy = total_energy) %>% 
  #convert to gross energy, assuming 40% energy losses to digestion, urine, feces
  mutate(gross_energy = (net_energy/0.6)) %>% 
  #calculate ingested food mass (IFM) (g)- there is a warning, but the calculation is correct, 
  #  so you can ignore the warning.
  mutate(ingested_food_mass = (gross_energy/diet2$total_energy_density)) %>% 
  #calculate total captured biomass (g)- same issue with warning, you can ignore it
  mutate(total_captured_biomass = (ingested_food_mass * diet2$total_captured_biomass)) %>% 
  #calculate ingested food mass (IFM) as % body mass
  mutate(IFM_perc_body_mass = (ingested_food_mass / (Av_mass*1000))*100) %>% 
  
  #Now calculate prey item numbers:
  
  #calculate number of abalone 
  mutate(abalone = ((gross_energy/(energy_density_all_diets$"2")) 
                    * (abalone$diet_2/abalone$mass_edible))) %>% 
  #calculate number of Cancer crab
  mutate(cancer_crabs = ((gross_energy/(energy_density_all_diets$"2")) 
                         * (cancer_crab$diet_2/cancer_crab$mass_edible))) %>% 
  #calculate number of kelp crab
  mutate(kelp_crab = ((gross_energy/(energy_density_all_diets$"2")) 
                      * (kelp_crab$diet_2/kelp_crab$mass_edible))) %>%
  #calculate number of urchin
  mutate(urchin = ((gross_energy/(energy_density_all_diets$"2")) 
                   * (urchin$diet_2/urchin$mass_edible))) %>%
  #calculate number of clam
  mutate(clam = ((gross_energy/(energy_density_all_diets$"2")) 
                 * (clam$diet_2/clam$mass_edible))) %>%
  #calculate number of mussels
  mutate(mussel = ((gross_energy/(energy_density_all_diets$"2")) 
                   * (mussel$diet_2/mussel$mass_edible))) %>%
  #calculate number of snails
  mutate(turban_snail = ((gross_energy/(energy_density_all_diets$"2")) 
                         * (turban_snail$diet_2/turban_snail$mass_edible))) 

#Save output
#folder_path <- "~/Documents/Thesis/otteR/Results"
write.csv(diet2_analysis, "Results/Diet2_Analysis.csv", row.names=FALSE)

# #Diet 3 ----------------------------------------------------------------

diet3_analysis <- model_results %>% 
  # Clean up df 
  select(Sex, Age, Lifestage, with.pup, total_energy, Av_mass) %>% 
  #rename total energy to net energy expenditure
  rename(net_energy = total_energy) %>% 
  #convert to gross energy, assuming 40% energy losses to digestion, urine, feces
  mutate(gross_energy = (net_energy/0.6)) %>% 
  #calculate ingested food mass (IFM) (g)- there is a warning, but the calculation is correct, 
  #  so you can ignore the warning.
  mutate(ingested_food_mass = (gross_energy/diet3$total_energy_density)) %>% 
  #calculate total captured biomass (g)- same issue with warning, you can ignore it
  mutate(total_captured_biomass = (ingested_food_mass * diet3$total_captured_biomass)) %>% 
  #calculate ingested food mass (IFM) as % body mass
  mutate(IFM_perc_body_mass = (ingested_food_mass / (Av_mass*1000))*100) %>% 
  
  #Now calculate prey item numbers:
  
  #calculate number of abalone 
  mutate(abalone = ((gross_energy/(energy_density_all_diets$"3")) 
                    * (abalone$diet_3/abalone$mass_edible))) %>% 
  #calculate number of Cancer crab
  mutate(cancer_crabs = ((gross_energy/(energy_density_all_diets$"3")) 
                         * (cancer_crab$diet_3/cancer_crab$mass_edible))) %>% 
  #calculate number of kelp crab
  mutate(kelp_crab = ((gross_energy/(energy_density_all_diets$"3")) 
                      * (kelp_crab$diet_3/kelp_crab$mass_edible))) %>%
  #calculate number of urchin
  mutate(urchin = ((gross_energy/(energy_density_all_diets$"3")) 
                   * (urchin$diet_3/urchin$mass_edible))) %>%
  #calculate number of clam
  mutate(clam = ((gross_energy/(energy_density_all_diets$"3")) 
                 * (clam$diet_3/clam$mass_edible))) %>%
  #calculate number of mussels
  mutate(mussel = ((gross_energy/(energy_density_all_diets$"3")) 
                   * (mussel$diet_3/mussel$mass_edible))) %>%
  #calculate number of snails
  mutate(turban_snail = ((gross_energy/(energy_density_all_diets$"3")) 
                         * (turban_snail$diet_3/turban_snail$mass_edible))) 

#Save output
#folder_path <- "~/Documents/Thesis/otteR/Results"
write.csv(diet3_analysis, "Results/Diet3_Analysis.csv", row.names=FALSE)

# #Diet 4 ----------------------------------------------------------------

diet4_analysis <- model_results %>% 
  # Clean up df 
  select(Sex, Age, Lifestage, with.pup, total_energy, Av_mass) %>% 
  #rename total energy to net energy expenditure
  rename(net_energy = total_energy) %>% 
  #convert to gross energy, assuming 40% energy losses to digestion, urine, feces
  mutate(gross_energy = (net_energy/0.6)) %>% 
  #calculate ingested food mass (IFM) (g)- there is a warning, but the calculation is correct, 
  #  so you can ignore the warning.
  mutate(ingested_food_mass = (gross_energy/diet4$total_energy_density)) %>% 
  #calculate total captured biomass (g)- same issue with warning, you can ignore it
  mutate(total_captured_biomass = (ingested_food_mass * diet4$total_captured_biomass)) %>% 
  #calculate ingested food mass (IFM) as % body mass
  mutate(IFM_perc_body_mass = (ingested_food_mass / (Av_mass*1000))*100) %>% 
 
   #Now calculate prey item numbers:
  
  #calculate number of abalone 
  mutate(abalone = ((gross_energy/(energy_density_all_diets$"4")) 
                    * (abalone$diet_4/abalone$mass_edible))) %>% 
  #calculate number of Cancer crab
  mutate(cancer_crabs = ((gross_energy/(energy_density_all_diets$"4")) 
                         * (cancer_crab$diet_4/cancer_crab$mass_edible))) %>% 
  #calculate number of kelp crab
  mutate(kelp_crab = ((gross_energy/(energy_density_all_diets$"4")) 
                      * (kelp_crab$diet_4/kelp_crab$mass_edible))) %>%
  #calculate number of urchin
  mutate(urchin = ((gross_energy/(energy_density_all_diets$"4")) 
                   * (urchin$diet_4/urchin$mass_edible))) %>%
  #calculate number of clam
  mutate(clam = ((gross_energy/(energy_density_all_diets$"4")) 
                 * (clam$diet_4/clam$mass_edible))) %>%
  #calculate number of mussels
  mutate(mussel = ((gross_energy/(energy_density_all_diets$"4")) 
                   * (mussel$diet_4/mussel$mass_edible))) %>%
  #calculate number of snails
  mutate(turban_snail = ((gross_energy/(energy_density_all_diets$"4")) 
                         * (turban_snail$diet_4/turban_snail$mass_edible))) 

#Save output
#folder_path <- "~/Documents/Thesis/otteR/Results"
write.csv(diet4_analysis, "Results/Diet4_Analysis.csv", row.names=FALSE)

# #Diet 5 ----------------------------------------------------------------

diet5_analysis <- model_results %>% 
  # Clean up df 
  select(Sex, Age, Lifestage, with.pup, total_energy, Av_mass) %>% 
  #rename total energy to net energy expenditure
  rename(net_energy = total_energy) %>% 
  #convert to gross energy, assuming 40% energy losses to digestion, urine, feces
  mutate(gross_energy = (net_energy/0.6)) %>% 
  #calculate ingested food mass (IFM) (g)- there is a warning, but the calculation is correct, 
  #  so you can ignore the warning.
  mutate(ingested_food_mass = (gross_energy/diet5$total_energy_density)) %>% 
  #calculate total captured biomass (g)- same issue with warning, you can ignore it
  mutate(total_captured_biomass = (ingested_food_mass * diet5$total_captured_biomass)) %>% 
  #calculate ingested food mass (IFM) as % body mass
  mutate(IFM_perc_body_mass = (ingested_food_mass / (Av_mass*1000))*100) %>% 
  #Now calculate prey item numbers:
  
  #calculate number of abalone 
  mutate(abalone = ((gross_energy/(energy_density_all_diets$"5")) 
                    * (abalone$diet_5/abalone$mass_edible))) %>% 
  #calculate number of Cancer crab
  mutate(cancer_crabs = ((gross_energy/(energy_density_all_diets$"5")) 
                         * (cancer_crab$diet_5/cancer_crab$mass_edible))) %>% 
  #calculate number of kelp crab
  mutate(kelp_crab = ((gross_energy/(energy_density_all_diets$"5")) 
                      * (kelp_crab$diet_5/kelp_crab$mass_edible))) %>%
  #calculate number of urchin
  mutate(urchin = ((gross_energy/(energy_density_all_diets$"5")) 
                   * (urchin$diet_5/urchin$mass_edible))) %>%
  #calculate number of clam
  mutate(clam = ((gross_energy/(energy_density_all_diets$"5")) 
                 * (clam$diet_5/clam$mass_edible))) %>%
  #calculate number of mussels
  mutate(mussel = ((gross_energy/(energy_density_all_diets$"5")) 
                   * (mussel$diet_5/mussel$mass_edible))) %>%
  #calculate number of snails
  mutate(turban_snail = ((gross_energy/(energy_density_all_diets$"5")) 
                         * (turban_snail$diet_5/turban_snail$mass_edible))) 

#Save output
#folder_path <- "~/Documents/Thesis/otteR/Results"
write.csv(diet5_analysis, "Results/Diet5_Analysis.csv", row.names=FALSE)

# I made this code my 

#     BBBB  III  TTTTT  CCCC  H   H
#     B   B  I     T   C      H   H
#     BBBB   I     T   C      HHHHH
#     B   B  I     T   C      H   H
#     BBBB  III    T    CCCC  H   H





# #Generalized function ----------------------------------------------------------------


analyze_diet <- function(diet_number, model_results, diet_list, energy_density_all_diets, prey_items) {
  
  diet_data <- diet_list[[paste0("diet", diet_number)]]
  
  analysis <- model_results %>%
    select(Sex, Age, Lifestage, with.pup, total_energy, Av_mass) %>%
    rename(net_energy = total_energy) %>%
    mutate(
      gross_energy = net_energy / 0.6,
      ingested_food_mass = gross_energy / diet_data$total_energy_density,
      total_captured_biomass = ingested_food_mass * diet_data$total_captured_biomass,
      IFM_perc_body_mass = (ingested_food_mass / (Av_mass * 1000)) * 100
    )
  
  # Loop through each prey item and calculate the number of individuals
  for (prey in names(prey_items)) {
    analysis <- analysis %>%
      mutate(!!prey := (gross_energy / energy_density_all_diets[[as.character(diet_number)]]) * 
               (prey_items[[prey]][[paste0("diet_", diet_number)]] / prey_items[[prey]]$mass_edible))
  }
  
  # Save output
  #write.csv(analysis, paste0("Results/Diet", diet_number, "_Analysis.csv"), row.names = FALSE)
  
  return(analysis)
}

diet_list <- list(diet1 = diet1, diet2 = diet2, diet3 = diet3, diet4 = diet4, diet5 = diet5)  # Store diet data in a list
prey_items <- list(abalone = abalone, cancer_crab = cancer_crab, kelp_crab = kelp_crab,
                   urchin = urchin, clam = clam, mussel = mussel, turban_snail = turban_snail)



