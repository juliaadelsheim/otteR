
#Condensing the variations from literature
# TODO: the activity budgets need to add up to 100 and they don't 
#       because values are being averaged, not added together


library(tidyverse)
library(readxl)

Variation_csv <- read_excel("Variation_csv.xlsx")

condensed_var <- Variation_csv %>% 
  mutate(sample.size  = round(Proportion * 10000)) %>% 
  rowwise() %>% 
  mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
  unnest(values_sim) %>% 
  group_by(Sex, with.pup, Lifestage, Age, Parameter) %>% 
  summarise(Mean = mean(values_sim),
            Std.dev = sd(values_sim))

print(condensed_var)


# ##### From Chat GPT ####
# library(tidyverse)
# library(readxl)
# 
# # Load the data
# Variation_csv <- read_excel("Variation_csv.xlsx")
# 
# # Condense the data proportionally
# condensed_var <- Variation_csv %>% 
#   mutate(sample.size = round(Proportion * 1000)) %>%   # Convert proportion to sample size
#   rowwise() %>% 
#   mutate(values_sim = list(rnorm(sample.size, mean = Mean, sd = Std.dev))) %>% 
#   unnest(values_sim) %>% 
#   group_by(Sex, with.pup, Lifestage, Age, Parameter) %>% 
#   summarise(Total = sum(values_sim),             # Sum the values_sim proportionally
#             Count = n(),                         # Total number of samples
#             Mean = Total / Count,                # Calculate mean if needed
#             Std.dev = sd(values_sim))            # Calculate std deviation
# 
# # Display the result
# print(condensed_var)
# 
# 
# print(condensed_var)


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

