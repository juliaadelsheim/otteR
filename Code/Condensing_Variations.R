
#Condensing the variations from literature
# TODO: the activity budgets need to add up to 100 and they don't 
#       because values are being averaged, not added together
##Start-----------------------------------------------------------------------

rm(list = ls())
# setwd("~/Documents/Thesis/Otter Data/Model Data/R codes/Model to R/Sensitivity Analysis")
setwd("~/Documents/Thesis/otteR/Data")

library(tidyverse)
library(readxl)
library(dplyr)
library(truncnorm)
################# Read in correct data!!!!! ############
Variation_csv <- read_excel("Variation_csv3.xlsx") #trying to fix average vs add problem with cvs2 version

set.seed(22222)

condensed_var <- Variation_csv %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  group_by(Sex, with.pup, Lifestage, Age, Parameter) %>% 
  summarise(Mean = mean(values_sim),
            Std.dev = sd(values_sim))

print(condensed_var)

#Adding up the components of activity budgets
# Need Juveniles - activity (3) and rest (2)
#       Pups - activity (3) and rest (6)
#       Adult Males - activity (3)

#---- Data ----
# USING cvs3 !!!!!!!!!!!!!!!!
Variation_csv <- read_excel("Variation_csv3.xlsx")

#----- Set-up ----
library(truncnorm)
set.seed(22222)

#Separate out the categories that need to be added up
# rtrunc is a truncated normal distribution

# ---- Activity Budgets ----

#### Pups ####

# Activity and Resting

########Activity
active1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active1") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

active2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active2") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

active3 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active3") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim3 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter3 = Parameter, values_sim3)

#Combines the activity types
final_active_pup <- active1 %>%
  cbind(active2, active3) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum = rowSums(across(c(2,4,6)), na.rm = TRUE)) %>% 
  # calculates standard deviation of the whole row_sum row
  mutate(std_dev = sd(row_sum, na.rm = TRUE))

#Calculate std dev- double checking the calue calculated above
sd(final_active_pup$row_sum)
# 10.90
mean(final_active_pup$row_sum)
# 22.00


############ Resting
## 6 components 
rest1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest1") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

rest2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest2") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

rest3 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest3") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim3 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter3 = Parameter, values_sim3)

rest4 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest4") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim4 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter4 = Parameter, values_sim4)

rest5 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest5") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim5 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter5 = Parameter, values_sim5)

rest6 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest6") %>%
  filter(Lifestage == "pup") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim6 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter6 = Parameter, values_sim6)

#Combines the activity types
final_rest_pup <- rest1 %>%
  cbind(rest2, rest3, rest4, rest5, rest6) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum = rowSums(across(c(2,4,6,8,10,12)))) %>% 
  # calculates standard deviation of the whole row_sum row
  mutate(std_dev = sd(row_sum))

#Calculate std dev- double checking the calue calculated above
sd(final_rest_pup$row_sum)
# 28.48174
mean(final_rest_pup$row_sum)
# 80.17601

#### Juveniles ####

# Activity and Resting

########Activity
active1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active1") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

active2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active2") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

active3 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active3") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim3 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter3 = Parameter, values_sim3)

#Combines the 3 activity types
final_active_juv <- active1 %>%
  cbind(active2, active3) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum = rowSums(across(c(2,4,6)))) %>% 
  # calculates standard deviation of the whole row_sum row
  mutate(std_dev = sd(row_sum))

#Calculate std dev- double checking the calue calculated above
sd(final_active_juv$row_sum)
# 9.228797
mean(final_active_juv$row_sum)
# 30.73849

############ Resting
rest1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest1") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

rest2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest2") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

#Combines the activity types
final_rest_juv <- rest1 %>%
  cbind(rest2) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum = rowSums(across(c(2,4)))) %>% 
  # calculates standard deviation of the whole row_sum row
  mutate(std_dev = sd(row_sum))

#Calculate std dev- double checking the calue calculated above
sd(final_rest_juv$row_sum)
# 15.2303
mean(final_rest_juv$row_sum)
# 33.09653



# Julia trying to fix it with chatgpt 1/12
# library(tidyverse)
# library(readxl)
# 
Variation_csv <- read_excel("Variation_csv3.xlsx")

# Calculate summed values for the specified parameters


#### Adult Males ####

# Activity

active1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active1") %>%
  filter(Lifestage == "adult", Sex == "M") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

active2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active2") %>%
  filter(Lifestage == "adult", Sex == "M") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

active3 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active3") %>%
  filter(Lifestage == "adult", Sex == "M") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim3 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter3 = Parameter, values_sim3)

#Combines the activity types
final_active_male_adult <- active1 %>%
  cbind(active2, active3) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum = rowSums(across(c(2,4,6)))) %>% 
  # calculates standard deviation of the whole row_sum row
  mutate(std_dev = sd(row_sum))

#Calculate std dev- double checking the calue calculated above
sd(final_active_male_adult$row_sum)
# 7.618868
mean(final_active_male_adult$row_sum)
# 27.09996



####  Subadult Males ####

# Using mean and std dev from Adult males and juveniles
#   average the values together to get std dev for MR and act budget

# They are not in the original data because they are averaged between 
#   juveniles and adults

#---- Percent Time----

#---- Perc Time Activity ---
## Juvenile Activity 
active1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active1") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

active2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active2") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

active3 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active3") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim3 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter3 = Parameter, values_sim3)

#Combines the 3 activity types
final_active_juv <- active1 %>%
  cbind(active2, active3) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum_juv = rowSums(across(c(2,4,6)))) #%>% 
  # calculates standard deviation of the whole row_sum row
 # mutate(std_dev = sd(row_sum))

sd(final_active_juv$row_sum)
# 9.228797
mean(final_active_juv$row_sum)
# 30.73849

## Adult Activity

active1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active1") %>%
  filter(Lifestage == "adult", Sex == "M") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

active2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active2") %>%
  filter(Lifestage == "adult", Sex == "M") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

active3 <- Variation_csv %>%
  filter(Parameter  == "act_budg_active3") %>%
  filter(Lifestage == "adult", Sex == "M") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim3 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter3 = Parameter, values_sim3)

#Combines the activity types
final_active_male_adult <- active1 %>%
  cbind(active2, active3) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum_ad = rowSums(across(c(2,4,6)))) #%>% 
  # calculates standard deviation of the whole row_sum row
  #mutate(std_dev = sd(row_sum))

## Add the two datasets together and average them
subadult_active <- final_active_juv$row_sum_juv %>% 
  cbind(final_active_male_adult$row_sum_ad) %>% 
  as.data.frame() %>% # Convert to data frame if needed
  rename(., juveniles = .,adults = V2) %>% 
  #caluculate the mean of the 2 values in each row
  mutate(subadult_perc_time_active = rowMeans(across(c(1,2)))) %>% 
  mutate(std_dev = sd(subadult_perc_time_active))

#standard deviation
sd(subadult_active$subadult_perc_time_active)
 #5.981999

#---- Perc Time Rest ---

## Juvenile
rest1 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest1") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values, a is the limit of the distribution, so this code does not
  # allow for negative values to be generated
  mutate(values_sim1 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>%
  select(Parameter1 = Parameter, values_sim1)

rest2 <- Variation_csv %>%
  filter(Parameter  == "act_budg_rest2") %>%
  filter(Lifestage == "juvenile") %>%
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim2 = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter2 = Parameter, values_sim2)

#Combines the activity types
final_rest_juv <- rest1 %>%
  cbind(rest2) %>% 
  #add up the 3 components by row into a new column
  mutate(row_sum = rowSums(across(c(2,4)))) %>% 
  # calculates standard deviation of the whole row_sum row
  mutate(std_dev = sd(row_sum))

## Adult
final_rest_adultM <- Variation_csv %>% 
  filter(Parameter == "act_budg_rest") %>% 
  filter(Lifestage == "adult") %>% 
  filter(Sex == "M") %>% 
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter = Parameter, values_sim)

## Combine for subadult
subadult_rest <- final_rest_juv$row_sum %>% 
  cbind(final_rest_adultM$values_sim) %>% 
  as.data.frame() %>% # Convert to data frame if needed
  rename(., juveniles = .,adults = V2) %>% 
  #caluculate the mean of the 2 values in each row
  mutate(subadult_perc_time_rest = rowMeans(across(c(1,2)))) %>% 
  mutate(std_dev = sd(subadult_perc_time_rest))

sd(subadult_rest$subadult_perc_time_rest)
# 8.595079

# ---- Perc Time Forage ---

## Juvenile

final_forage_juv <- Variation_csv %>% 
  filter(Parameter == "act_budg_forage") %>% 
  filter(Lifestage == "juvenile") %>% 
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter = Parameter, values_sim)

## Adult
final_forage_adultM <- Variation_csv %>% 
  filter(Parameter == "act_budg_forage") %>% 
  filter(Lifestage == "adult") %>% 
  filter(Sex == "M") %>% 
  mutate(sample.size = round(Proportion * 10000)) %>%
  # Expand rows based on sample size
  uncount(sample.size) %>%
  # Simulate values
  mutate(values_sim = rtruncnorm(n(), a = 0, mean = Mean, sd = Std.dev)) %>% 
  select(Parameter = Parameter, values_sim)

## Combine for subadult
subadult_forage <- final_forage_juv$values_sim %>% 
  cbind(final_forage_adultM$values_sim) %>% 
  as.data.frame() %>% # Convert to data frame if needed
  rename(., juveniles = .,adults = V2) %>% 
  #caluculate the mean of the 2 values in each row
  mutate(subadult_perc_time_forage = rowMeans(across(c(1,2)))) %>% 
  mutate(std_dev = sd(subadult_perc_time_forage))

sd(subadult_forage$subadult_perc_time_forage)
# 9.228917

# ---- MR ----

# ---- Active ---

## Juvenile
final_MRactive_juv  <- Variation_csv %>% 
  filter(Parameter == "MR_active") %>% 
  filter(Lifestage == "juvenile") %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  #Need to convert MJ/day to kJ/kg/min
  mutate(values_simMJ = (values_sim/(404.64))*1000) %>%  #404.64min is min/day active from excel
  mutate(values_sim_kj_kg_min = values_simMJ/14.42) #14.42kg is the ave mass of juveniles

## Adult
final_MRactive_adultM <- Variation_csv %>% 
  filter(Parameter == "MR_active") %>% 
  filter(Lifestage == "adult") %>% 
  filter(Sex == "M") %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  #Convert to MJ per day
  mutate(values_sim_kJ_kg_min = values_sim*(20.08/1000))

## Combine for subadult
subadult_MRactive <- final_MRactive_juv$values_sim_kj_kg_min %>% 
  cbind(final_MRactive_adultM$values_sim_kJ_kg_min) %>% 
  as.data.frame() %>% # Convert to data frame if needed
  rename(., juveniles = .,adults = V2) %>% 
  #caluculate the mean of the 2 values in each row
  mutate(subadult_MRactive = rowMeans(across(c(1,2)))) %>% 
  mutate(std_dev = sd(subadult_MRactive))

sd(subadult_MRactive$subadult_MRactive)
# 0.07338876

# ---- Rest ---

## Juvenile
final_MRrest_juv  <- Variation_csv %>% 
  filter(Parameter == "MR_rest") %>% 
  filter(Lifestage == "juvenile") %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  #Need to convert MJ/day to kJ/kg/min
  mutate(values_simMJ = (values_sim/(439.20))*1000) %>%  #404.64min is min/day active from excel
  mutate(values_sim_kj_kg_min = values_simMJ/14.42)

## Adult
final_MRrest_adultM <- Variation_csv %>% 
  filter(Parameter == "MR_rest") %>% 
  filter(Lifestage == "adult") %>% 
  filter(Sex == "M") %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  #Convert to MJ per day
  mutate(values_sim_kJ_kg_min = values_sim*(20.08/1000))


## Combine for subadult
subadult_MRrest <- final_MRrest_juv$values_sim_kj_kg_min %>% 
  cbind(final_MRrest_adultM$values_sim_kJ_kg_min) %>% 
  as.data.frame() %>% # Convert to data frame if needed
  rename(., juveniles = .,adults = V2) %>% 
  #caluculate the mean of the 2 values in each row
  mutate(subadult_MRrest = rowMeans(across(c(1,2)))) %>% 
  mutate(std_dev = sd(subadult_MRrest))

sd(subadult_MRrest$subadult_MRrest)
# 0.07684956

# ---- Foraging ---

### Juvenile
final_MRforage_juv  <- Variation_csv %>% 
  filter(Parameter == "MR_forage") %>% 
  filter(Lifestage == "juvenile") %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  #Need to convert MJ/day to kJ/kg/min
  mutate(values_simMJ = (values_sim/(596.16))*1000) %>%  #404.64min is min/day active from excel
  mutate(values_sim_kj_kg_min = values_simMJ/14.42)

## Adult
final_MRforage_adultM <- Variation_csv %>% 
  filter(Parameter == "MR_forage") %>% 
  filter(Lifestage == "adult") %>% 
  filter(Sex == "M") %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  mutate(values_sim_kJ_kg_min = values_sim*(20.08/1000))


## Combine for subadult
subadult_MRforage <- final_MRforage_juv$values_sim_kj_kg_min %>% 
  cbind(final_MRforage_adultM$values_sim_kJ_kg_min) %>% 
  as.data.frame() %>% # Convert to data frame if needed
  rename(., juveniles = .,adults = V2) %>% 
  #caluculate the mean of the 2 values in each row
  mutate(subadult_MRforage = rowMeans(across(c(1,2)))) %>% 
  mutate(std_dev = sd(subadult_MRforage))

sd(subadult_MRforage$subadult_MRforage)
# 0.10632

#### Notes  ####
# 1-17-2025

# I ran the code for males and juveniles and averaged them after generating 
# random values and the calculated the sd. I used the recorded values and put 
# them directly into the csv file. 

# I realized that I needed to convert the units from MJ or ml O2 to kj/mg/min 
# because that is the unit that the values in the model data csv are in.
# for M and F and pups I did them in the excel sheet but subadult i did in 
# the condensing code R script

# 1-16-2025

#  I recorded the SD values that I calculated and put them manually into 
#  the stdev_MR_perc_time csv file that get puts into the SA function
#  I didnt want to figure out how to combine or replace them in the R code lol

#   There are also no MR values activity and foraging for females with a pup 
#   because the % change from RMR was used to calculate the other MR's per 
#   Thometz et al. 2016 paper 

# ---- Dont think I need any of this ---- ####

#--MRs ---
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



