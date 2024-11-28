rm(list=ls())
#---- Mass Q plots from sensitivity analysis----
mods <- read.csv("model_all_reps_Av_mass_0.2.csv")
# unique(model_all_reps$Sex)
# fem <- model_all_reps[model_all_reps$Sex=='F' & model_all_reps$with.pup=='yes',]
ggplot(mods, aes(Age, total_energy, shape=Sex, colour=rep_num)) + geom_point()

range <- mods %>%
  group_by(Sex, Lifestage, Age, with.pup) %>%
  mutate(mean = mean(total_energy), 
         Q95 = quantile(total_energy, 0.95),
         Q5 = quantile(total_energy, 0.05))

range <- distinct(range[,c('Sex', 'Lifestage', 'Age', 'with.pup', 'mean', 'Q95', 'Q5')])

ggplot(data=range, aes(colour=as.factor(Sex), linetype=as.factor(with.pup))) + 
  geom_line(aes(Age, mean)) +
  geom_line(aes(Age, Q95)) +
  geom_line(aes(Age, Q5)) 

#---- Qplot 20% by AMBRE----
(Mass_Qplot_0.2<-
  ggplot(data=range, aes(colour=as.factor(Sex), fill=as.factor(Sex), linetype=as.factor(with.pup))) + 
  geom_line(aes(Age, mean)) +
  geom_ribbon(aes(x=Age, ymin=Q5, ymax=Q95), colour=NA, alpha=.3) +
  theme_bw()+
    ggtitle("Ave Mass, stdev 20%")+
    ylab("Energy Requirements (kJ/day)")
)
ggsave ("Mass_Qplot_20.jpeg", width = 6, height = 3)


#---- Qplot stdev from Lit----
mods <- read.csv("model_all_reps_Av_mass_lit.csv")
# unique(model_all_reps$Sex)
# fem <- model_all_reps[model_all_reps$Sex=='F' & model_all_reps$with.pup=='yes',]
ggplot(mods, aes(Age, total_energy, shape=Sex, colour=rep_num)) + geom_point()

range <- mods %>%
  group_by(Sex, Lifestage, Age, with.pup) %>%
  mutate(mean = mean(total_energy), 
         Q95 = quantile(total_energy, 0.95),
         Q5 = quantile(total_energy, 0.05))

range <- distinct(range[,c('Sex', 'Lifestage', 'Age', 'with.pup', 'mean', 'Q95', 'Q5')])

(
  Mass_Qplot_lit <-
ggplot(data=range, aes(colour=as.factor(Sex), fill=as.factor(Sex), linetype=as.factor(with.pup))) + 
  geom_line(aes(Age, mean)) +
  geom_ribbon(aes(x=Age, ymin=Q5, ymax=Q95), colour=NA, alpha=.3) +
  theme_bw()+
  ggtitle("Ave Mass, stdev from literature")+
    ylab("Energy Requirements (kJ/day)")
)
ggsave ("Mass_Qplot_lit.jpeg", width = 6, height = 3)
