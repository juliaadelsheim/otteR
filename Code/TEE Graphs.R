
## Graphing TEE with error bars calculated from Sensitivity Analysis


##Start-----------------------------------------------------------------------

#Clean environment
rm(list = ls())

# set working directory 
setwd("~/Documents/Thesis/otteR/Results")

# Packages--------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# All parameters, lit ---> varied with literature error -----
# Data------------------------------------------------------------------
SA_tee <- read.csv(file ='SA_results_perc_sd_lit_param_all.csv') 

# Graphs ---------------------------------------------------------------

  TEE_plot <- 
  ggplot(SA_tee %>% filter(parameter == "total_energy"),
         aes(x = Age, y = value_mean, color = otter_type)) +
  scale_color_brewer(palette = "Dark2", name = NULL, labels = c("Female without pup", "Female with a pup", "Male")) +
  ylab("Energy Expenditure (kJ/day)") +
  xlab("Age (yrs)") +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci), width = .5) +
  geom_point() +
  geom_line() +
  theme_bw()+
  theme(legend.position = c(0.75, 0.1),  # Moves legend to the bottom center (0.5)
         legend.justification = c(0.5, 0),  # Aligns it properly
         legend.background = element_rect(fill = "white", color = "black")) 

print(TEE_plot)


ggsave("~/Documents/Thesis/otteR/Plots/SA_all_param_lit.png", plot = TEE_plot, width = 8, height = 6)


# All parameters, 10% -> varied with 10% std dev -----------------------------------

# Data------------------------------------------------------------------
SA_tee10 <- read.csv(file ='SA_results_perc_sd_0.1_param_all.csv') 

# Graphs ---------------------------------------------------------------

TEE_plot <- 
  ggplot(SA_tee10 %>% filter(parameter == "total_energy"),
         aes(x = Age, y = value_mean, color = otter_type)) +
  scale_color_brewer(palette = "Dark2", name = NULL, labels = c("Female without pup", "Female with a pup", "Male")) +
  ylab("Energy Expenditure (kJ/day)") +
  xlab("Age (yrs)") +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci), width = .5) +
  geom_point() +
  geom_line() +
  theme_bw()+
  theme(legend.position = c(0.75, 0.1),  # Moves legend to the bottom center (0.5)
        legend.justification = c(0.5, 0),  # Aligns it properly
        legend.background = element_rect(fill = "white", color = "black")) 

print(TEE_plot)

ggsave("~/Documents/Thesis/otteR/Plots/SA_all_param_0.1.png", plot = TEE_plot, width = 8, height = 6)


# Mass Specific TEE, all parameters, literature graph ------------------

# Data ---------------------------
masses2 <- read.csv(file ='mass_growth_with.pup.csv') 

mass_spec_TEE <- SA_tee %>% 
  filter(parameter == "total_energy") %>% 
  left_join(masses2, by = c("Sex", "with.pup", "Age")) %>%  # Avoids collapsing data
  mutate(mass_spec = value_mean / Av_mass)

# Graph --------------------------

mass_spec_TEE_plot <- 
  ggplot(mass_spec_TEE,
         aes(x = Age, y = mass_spec, color = otter_type)) +
  scale_color_brewer(palette = "Dark2", name = NULL, labels = c("Female without pup", "Female with a pup", "Male")) +
  ylab("Mass Specific Energy Expenditure \n (kJ/kg/day)") +
  xlab("Age (yrs)") +
  scale_y_continuous(limits = c(500, 900)) +
  geom_errorbar(aes(ymin = lower.ci/Av_mass, ymax = upper.ci/Av_mass), width = .5) +
  geom_point() +
  geom_line() +
  theme_bw()+
  theme(legend.position = c(0.5, 0.5),  # Moves legend to the bottom center (0.5)
        legend.justification = c(0.5, 0),  # Aligns it properly
        legend.background = element_rect(fill = "white", color = "black")) 

print(mass_spec_TEE_plot)

#Save
ggsave("~/Documents/Thesis/otteR/Plots/Mass_Spec_SA_all_param_lit.png", plot = mass_spec_TEE_plot, width = 8, height = 6)






# ---- Garbage ----



# Original Model Run graph with error from SA

#Sort SA data to match

SA_tee_only <- SA_tee %>% 
  filter(parameter == "total_energy")

#Sort data by sex/with.pup
  model.run.1 <- model.run.1 %>%
    mutate(Sex_Pup = case_when(
    Sex == "F" & with.pup == "yes" ~ "Female with pup",
    Sex == "F" & with.pup == "no" ~ "Female without pup",
    Sex == "M" ~ "Male")) 
  
# Graph
  TEE_plot_og <- model.run.1 %>% 
  ggplot(model.run.1 %>%
         aes(x = Age, y = total_energy, color = Sex_Pup)) +
  scale_color_viridis_d(name = NULL) +
  ylab("Energy Expenditure (kJ/day)") +
  xlab("Age (yrs)") +
  geom_errorbar(aes(ymin = (total_energy - SA_tee_only$value_sd), ymax = (total_energy + SA_tee_only$value_sd)), width = .5) +
   # geom_errorbar(aes(ymin = SA_tee_only$lower.ci, ymax = SA_tee_only$upper.ci), width = .5) +
    geom_point() +
  geom_line() +
  theme_bw()+
  theme(legend.position = c(0.75, 0.1),  # Moves legend to the bottom center (0.5)
        legend.justification = c(0.5, 0),  # Aligns it properly
        legend.background = element_rect(fill = "white", color = "black")) 

print(TEE_plot_og)

