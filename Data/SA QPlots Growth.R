## Q plots from Sensitivity analysys

# Growth

rm(list=ls())
setwd("~/Documents/Thesis/otteR/Data")

#---- Mass Q plots from sensitivity analysis----
mods <- read.csv("model_all_reps_Growth_0.1.csv")
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

#---- Qplot 10% by AMBRE----
(Growth_Qplot_0.1<-
   ggplot(data=range, aes(colour=as.factor(Sex), fill=as.factor(Sex), linetype=as.factor(with.pup))) + 
   geom_line(aes(Age, mean)) +
   geom_ribbon(aes(x=Age, ymin=Q5, ymax=Q95), colour=NA, alpha=.3) +
   theme_bw()+
   ggtitle("Growth, stdev 10%")+
   ylab("Energy Requirements (kJ/day)")
)
ggsave ("Growth_Qplot_10.jpeg", width = 6, height = 3)

#---- Qplot 5% ----
mods <- read.csv("model_all_reps_Growth_0.05.csv")
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

(Growth_Qplot_0.05<-
   ggplot(data=range, aes(colour=as.factor(Sex), fill=as.factor(Sex), linetype=as.factor(with.pup))) + 
   geom_line(aes(Age, mean)) +
   geom_ribbon(aes(x=Age, ymin=Q5, ymax=Q95), colour=NA, alpha=.3) +
   theme_bw()+
   ggtitle("Growth, stdev 5%")+
   ylab("Energy Requirements (kJ/day)")
)
ggsave ("Growth_Qplot_05.jpeg", width = 6, height = 3)

#---- Qplot 20% ----
mods <- read.csv("model_all_reps_Growth_0.2.csv")
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

(Growth_Qplot_0.20<-
    ggplot(data=range, aes(colour=as.factor(Sex), fill=as.factor(Sex), linetype=as.factor(with.pup))) + 
    geom_line(aes(Age, mean)) +
    geom_ribbon(aes(x=Age, ymin=Q5, ymax=Q95), colour=NA, alpha=.3) +
    theme_bw()+
    ggtitle("Growth, stdev 20%")+
    ylab("Energy Requirements (kJ/day)")
)
ggsave ("Growth_Qplot_20.jpeg", width = 6, height = 3)


#---- Qplot stdev from Lit----
mods <- read.csv("model_all_reps_Growth_lit.csv")
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
  Growth_Qplot_lit <-
    ggplot(data=range, aes(colour=as.factor(Sex), fill=as.factor(Sex), linetype=as.factor(with.pup))) + 
    geom_line(aes(Age, mean)) +
    geom_ribbon(aes(x=Age, ymin=Q5, ymax=Q95), colour=NA, alpha=.3) +
    theme_bw()+
    ggtitle("Growth, stdev from literature")+
    ylab("Energy Requirements (kJ/day)")
)
ggsave ("Growth_Qplot_lit.jpeg", width = 6, height = 3)
