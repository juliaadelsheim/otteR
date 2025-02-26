
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

# All parameters, lit ---> SAvaried with literature error -----
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
  scale_color_brewer(palette = "Dark2", name = NULL) +
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


# OG Model with SA Variation ----


# Data ----
SA_lit_all_sd <- read.csv(file = "SA_results_TEE_lit_param_all.csv")

#Need to run model on different R script (OtterModel.R) to get model.run.1

#Sort data by sex/with.pup
model.run.og <- model.run.1 %>%
  arrange(Sex, with.pup) %>% 
  #Add column for otter type
  mutate(Sex_pup = case_when(     
    Sex == "F" & with.pup == "yes" ~ "Female with pup",
    Sex == "F" & with.pup == "no" ~ "Female without pup",
    Sex == "M" ~ "Male")) 

# Sort data by sex/with.pup so it lines up with other dataset
SA_lit_all_sd <- SA_lit_all_sd %>% 
  arrange(Sex, with.pup)

# Graph - Ribbons----
TEE_plot_og <- model.run.og %>% 
  ggplot(model.run.og %>%
           aes(x = Age, y = total_energy, color = Sex_pup)) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_fill_brewer(palette = "Dark2", name = NULL) + 
  ylab("Energy Expenditure (kJ/day)") +
  xlab("Age (yrs)") +
  # Version with color shading 
  geom_ribbon(aes(ymin = total_energy - SA_lit_all_sd$value_sd, 
                  ymax = total_energy + SA_lit_all_sd$value_sd, 
                  fill = Sex_pup), 
              alpha = 0.3, color = NA) +
  coord_cartesian(ylim = c(0, 20000)) + # V2 has scale of 5000 to 20k
  geom_point() +
  geom_line(aes(color = Sex_pup)) +
  theme_bw()+
  theme(legend.position = c(0.75, 0.04),  # Moves legend to the bottom center (0.5)
        legend.justification = c(0.5, 0),  # Aligns it properly
        legend.background = element_rect(fill = "white", color = "black")) 

print(TEE_plot_og)

#Save
ggsave("~/Documents/Thesis/otteR/Plots/TEE_plot_w_SA_sd_ribbon.png", plot = TEE_plot_og, width = 8, height = 6)


# Graph - Error Bars ----
TEE_plot_og_errorbar <- model.run.og %>% 
  ggplot(model.run.og %>%
           aes(x = Age, y = total_energy, color = Sex_pup)) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  ylab("Energy Expenditure (kJ/day)") +
  xlab("Age (yrs)") +
  geom_errorbar(aes(ymin = total_energy - SA_lit_all_sd$value_sd, ymax = total_energy + SA_lit_all_sd$value_sd), width = .5) +
  coord_cartesian(ylim = c(0, 20000)) + # V2 has scale of 5000 to 20k
  geom_point() +
  geom_line(aes(color = Sex_pup)) +
  theme_bw()+
  theme(legend.position = c(0.75, 0.04),  # Moves legend to the bottom center (0.5)
        legend.justification = c(0.5, 0),  # Aligns it properly
        legend.background = element_rect(fill = "white", color = "black")) 

print(TEE_plot_og_errorbar)

#Save
ggsave("~/Documents/Thesis/otteR/Plots/TEE_plot_w_SA_sd_errorbar.png", plot = TEE_plot_og_errorbar, width = 8, height = 6)


# Mass Specific OG model with SA Variation ----

# Data ----

# Read in mass dataset 
masses2 <- read.csv(file ='mass_growth_with.pup.csv') 

#Arrange data and calculate mass specific values 
model.run.og.ms <- model.run.1 %>%
  arrange(Sex, with.pup) %>% 
  #merge masses2 with OG run
  left_join(masses2, by = c("Sex", "with.pup", "Age")) %>% 
  #Add column for otter type
  mutate(Sex_pup = case_when(     
    Sex == "F" & with.pup == "yes" ~ "Female with pup",
    Sex == "F" & with.pup == "no" ~ "Female without pup",
    Sex == "M" ~ "Male")) %>% 
  mutate(mass_spec_TEE = total_energy / Av_mass) %>% 
  mutate(mass_spec_sd = SA_lit_all_sd$value_sd / Av_mass)

# Sort data by sex/with.pup so it lines up with other dataset
SA_lit_all_sd <- SA_lit_all_sd %>% 
  arrange(Sex, with.pup)

# Graph - Ribbons ----
mass_spec_TEE_plot_og <- model.run.og.ms %>% 
  ggplot(model.run.og.ms %>% 
         aes(x = Age, y = mass_spec_TEE, color = Sex_pup)) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_fill_brewer(palette = "Dark2", name = NULL) + 
  ylab("Mass Specific Energy Expenditure \n (kJ/kg/day)") +
  xlab("Age (yrs)") +
  coord_cartesian(ylim = c(400, 900)) +
  geom_ribbon(aes(ymin = mass_spec_TEE - mass_spec_sd, 
                  ymax = mass_spec_TEE + mass_spec_sd, 
                  fill = Sex_pup), 
              alpha = 0.3, color = NA) +
  geom_point() +
  geom_line() +
  theme_bw()+
  theme(legend.position = c(0.5, 0.55),  # Moves legend to the bottom center (0.5)
        legend.justification = c(0.5, 0.13),  # Aligns it properly
        legend.background = element_rect(fill = "white", color = "black"))
        # legend.key.size = unit(0.4, "cm"),  # Reduces legend key size (default ~1cm)
        # legend.text = element_text(size = 7.5),  # Makes legend text smaller (default ~11)
        # legend.title = element_text(size = 8.5))

print(mass_spec_TEE_plot_og)

#Save
ggsave("~/Documents/Thesis/otteR/Plots/Mass_Spec_TEE_plot_w_SA_sd.png", plot = mass_spec_TEE_plot_og, width = 8, height = 6)



