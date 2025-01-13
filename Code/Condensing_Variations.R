
#Condensing the variations from literature
# TODO: the activity budgets need to add up to 100 and they don't 
#       because values are being averaged, not added together


library(tidyverse)
library(readxl)

Variation_csv <- read_excel("Variation_csv2.xlsx") #trying to fix average vs add problem with cvs2 version

condensed_var <- Variation_csv %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  group_by(Sex, with.pup, Lifestage, Age, Parameter) %>% 
  summarise(Mean = mean(values_sim),
            Std.dev = sd(values_sim))

print(condensed_var)

# Julia trying to fix it with chatgpt 1/12
# library(tidyverse)
# library(readxl)
# 
Variation_csv <- read_excel("Variation_csv2.xlsx")

# Calculate summed values for the specified parameters

#----MRs ----
#MR_active
summed_var_MR_act <- Variation_csv %>%
  filter(Parameter %in% c("MR_active1", "MR_active2", "MR_active3")) %>% 
  mutate(sample.size = round(Proportion * 10000)) %>%
  rowwise() %>%
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>%
  unnest(values_sim) %>%
  group_by(Sex, with.pup, Lifestage, Age) %>%
  summarise(Mean = sum(mean(values_sim)),  # Summing means
            Std.dev = sqrt(sum(sd(values_sim)^2)),   # Summing variances
            .groups = 'drop') %>%
  mutate(Parameter = "MR_active_total") # Add new parameter
  
#MR_rest
summed_var_MR_rest <- Variation_csv %>%
  filter(Parameter %in% c("MR_rest1", "MR_rest2")) %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  rowwise() %>%
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>%
  unnest(values_sim) %>%
  group_by(Sex, with.pup, Lifestage, Age) %>%
  summarise(Mean = sum(mean(values_sim)),  # Summing means
            Std.dev = sqrt(sum(sd(values_sim)^2)),  # Summing variances
            .groups = 'drop') %>%
  mutate(Parameter = "MR_rest_total")

# ---- Activity Budgets ----
#act_budg_active
summed_var_act_bud_active <- Variation_csv %>%
  filter(Parameter %in% c("act_budg_active1", "act_budg_active2", "act_budg_active3")) %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  rowwise() %>%
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>%
  unnest(values_sim) %>%
  group_by(Sex, with.pup, Lifestage, Age) %>%
  summarise(Mean = sum(mean(values_sim)),  # Summing means
            Std.dev = sqrt(sum(sd(values_sim)^2)),  # Summing variances
            .groups = 'drop') %>%
  mutate(Parameter = "act_budg_active_total")

#act_budg_rest
summed_var_act_bud_rest <- Variation_csv %>%
  filter(Parameter %in% c("act_budg_rest1", "act_budg_rest2", "act_budg_rest3", 
                          "act_budg_rest4", "act_budg_rest5", "act_budg_rest6")) %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  rowwise() %>%
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>%
  unnest(values_sim) %>%
  group_by(Sex, with.pup, Lifestage, Age) %>%
  summarise(Mean = sum(mean(values_sim)),  # Summing means
            Std.dev = sqrt(sum(sd(values_sim)^2)),  # Summing variances
            .groups = 'drop') %>%
  mutate(Parameter = "act_budg_rest_total")

# Remove original rows for the specified parameters
filtered_var <- Variation_csv %>%
  filter(!Parameter %in% c("MR_active1", "MR_active2", "MR_active3",
                           "MR_rest1", "MR_rest2", 
                           "act_budg_active1", "act_budg_active2", "act_budg_active3",
                           "act_budg_rest1", "act_budg_rest2", "act_budg_rest3", 
                           "act_budg_rest4", "act_budg_rest5", "act_budg_rest6"))

# Combine the filtered data with the summed data
final_var <- bind_rows(filtered_var, 
                       summed_var_MR_rest, 
                       summed_var_MR_act,
                       summed_var_act_bud_active,
                       summed_var_act_bud_rest
                       ) 

#Rename parameters I chnanged
# final_var <- final_var %>%
#   mutate(Parameter = ifelse(Parameter == "MR_active_total", "MR_active", Parameter)) %>% 
#   mutate(Parameter = ifelse(Parameter == "MR_rest_total", "MR_rest", Parameter)) %>%
#   mutate(Parameter = ifelse(Parameter == "act_budg_active_total", "act_budg_active", Parameter)) %>%
#   mutate(Parameter = ifelse(Parameter == "act_budg_rest_total", "act_budg_rest", Parameter))

print(final_var)

#Variation_csv <- read_excel("Variation_csv2.xlsx") #trying to fix average vs add problem with cvs2 version

######################################################
#######################################################
TODO #Need to figure out how to condense the other parameters that dont have active1, 2 etc. 
######################################################
#######################################################

condensed_var <- final_var %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  group_by(Sex, with.pup, Lifestage, Age, Parameter) %>% 
  summarise(Mean = mean(values_sim),
            Std.dev = sd(values_sim))

print(condensed_var)


##### Dont think I need this ####
#Thometz et al 2014
#
#Juveniles
#Metabolic Rates- activity
# Proportions
#   Grooming = 0.34
#   Swimming = 0.52
#   Other = 0.14

a<- data.frame(mean = rnorm(10000*prop,1,0.68), prop = 0.34)  
b <- data.frame(mean = rnorm(10000,1.54,0.67), prop = 0.52) 
c <- data.frame(mean = rnorm(10000,0.4,0.36), prop= 0.14) 

abc <- rbind(a,b,c)

abc_weighted <- mean= (mean*)
mean(abc$mean)
sd(abc$mean)


weighted.mean(abc$mean, abc$prop)
# 1.19

