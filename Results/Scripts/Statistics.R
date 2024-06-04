# In this script, we will statistically test our results
library(tidyverse)
library(ggplot2)
library(reshape2)
library(skimr)
library(lmerTest)

# We load in the files
experiment_1 <- read.csv("Experiment_1/Curated/results.csv", encoding = "UTF-8", sep = ";")
experiment_2 <- read.csv("Experiment_1/Automatic/results.csv", encoding = "UTF-8", sep = ";")
experiment_3 <- read.csv("Experiment_2/Curated/BIC/results.csv", encoding = "UTF-8", sep = ";")
experiment_4 <- read.csv("Experiment_2/Automatic/BIC/results.csv", encoding = "UTF-8", sep = ";")
experiment_5 <- read.csv("Experiment_3/Curated/BIC/results.csv", encoding = "UTF-8", sep = ";")
experiment_6 <- read.csv("Experiment_3/Automatic/BIC/results.csv", encoding = "UTF-8", sep = ";")

# We add a column "Experiment" and a column "Mode" to each data frame
experiment_1$Experiment <- "Experiment 1"
experiment_1$Mode <- "Curated"
experiment_2$Experiment <- "Experiment 1"
experiment_2$Mode <- "Automatic"
experiment_3$Experiment <- "Experiment 2"
experiment_3$Mode <- "Curated"
experiment_4$Experiment <- "Experiment 2"
experiment_4$Mode <- "Automatic"
experiment_5$Experiment <- "Experiment 3"
experiment_5$Mode <- "Curated"
experiment_6$Experiment <- "Experiment 3"
experiment_6$Mode <- "Automatic"

# We combine the dataframes
df_all <- rbind(experiment_1, experiment_2, experiment_3, 
                experiment_4, experiment_5, experiment_6) %>%
  select(-c(Weighted, Overall, Amount_senses, Cluster_Estimation))
df_words <- df_all %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average")
df_averages <- df_all %>%
  filter(X == "Average_nouns" | X == "Average_verbs" | X == "Global_average")

# We make two sub-dataframes, one for each "Model"
df_flau <- df_averages %>%
  filter(Model == "Flaubert")
df_cam <- df_averages %>%
  filter(Model == "Camembert")
# We delete experiment_1 to 6
rm(experiment_1, experiment_2, experiment_3, experiment_4, experiment_5, experiment_6)

#-----------------------Investigating the data------------------------
flau_curated <- df_words %>%
  filter(Model == "Flaubert" & Mode == "Curated") %>%
  select(-c(Model, Mode))
flau_cur_exp_1 <- flau_curated %>%
  filter(Experiment == "Experiment 1") %>%
  select(-c(Experiment))

flau_cur_exp_2 <- flau_curated %>%
  filter(Experiment == "Experiment 2") %>%
  select(-c(Experiment))

flau_cur_exp_3 <- flau_curated %>%
  filter(Experiment == "Experiment 3") %>%
  select(-c(Experiment))

cam_curated <- df_words %>%
  filter(Model == "Camembert" & Mode == "Curated") %>%
  select(-c(Model, Mode))

cam_cur_exp_1 <- cam_curated %>%
  filter(Experiment == "Experiment 1") %>%
  select(-c(Experiment))

cam_cur_exp_2 <- cam_curated %>%
  filter(Experiment == "Experiment 2") %>%
  select(-c(Experiment))

cam_cur_exp_3 <- cam_curated %>%
  filter(Experiment == "Experiment 3") %>%
  select(-c(Experiment))

# Automatic dataset
flau_auto <- df_words %>%
  filter(Model == "Flaubert" & Mode == "Automatic") %>%
  select(-c(Model, Mode))
flau_auto_exp_1 <- flau_auto %>%
  filter(Experiment == "Experiment 1") %>%
  select(-c(Experiment))
flau_auto_exp_2 <- flau_auto %>%
  filter(Experiment == "Experiment 2") %>%
  select(-c(Experiment))
flau_auto_exp_3 <- flau_auto %>%
  filter(Experiment == "Experiment 3") %>%
  select(-c(Experiment))

cam_auto <- df_words %>%
  filter(Model == "Camembert" & Mode == "Automatic") %>%
  select(-c(Model, Mode))
cam_auto_exp_1 <- cam_auto %>%
  filter(Experiment == "Experiment 1") %>%
  select(-c(Experiment))
cam_auto_exp_2 <- cam_auto %>%
  filter(Experiment == "Experiment 2") %>%
  select(-c(Experiment))
cam_auto_exp_3 <- cam_auto %>%
  filter(Experiment == "Experiment 3") %>%
  select(-c(Experiment))

# --- Skimming the dataframes ---
# Curated:
# together
skim(flau_curated)
skim(cam_curated)
# first experiment
skim(flau_cur_exp_1)
skim(cam_cur_exp_1)
# second experiment
skim(flau_cur_exp_2)
skim(cam_cur_exp_2)
# third experiment
skim(flau_cur_exp_3)
skim(cam_cur_exp_3)

# Automatic:
# together
skim(flau_auto)
skim(cam_auto)

# first experiment
skim(flau_auto_exp_1)
skim(cam_auto_exp_1)



# ----------------------Statistical tests-------------------------

# --- Curated datasets ---


# We want to use a paired t-test
# We first check if the data is normally distributed

# To compare clusterings, it is better to use v-measure
# Cam
shapiro.test(cam_cur_exp_1$V_measure)
shapiro.test(cam_cur_exp_2$V_measure)
shapiro.test(cam_cur_exp_3$V_measure)
# Flau
shapiro.test(flau_cur_exp_1$V_measure)
shapiro.test(flau_cur_exp_2$V_measure)
shapiro.test(flau_cur_exp_3$V_measure)
# All the data is normally distributed, 
# so we can compare the scores using paired t-tests

# 1: we check if method 2 is significantly lower than method 1
# We make a vector of the differences between V-measure scores
cam_v_measure <- cam_cur_exp_2$V_measure - cam_cur_exp_1$V_measure
flau_v_measure <- flau_cur_exp_2$V_measure - flau_cur_exp_1$V_measure

# We perform the t-test
t.test(cam_v_measure, alternative = "less")
t.test(flau_v_measure, alternative = "less")


# 2: we check if method 3 Cam is significantly better than method 3 flau
v_measure <- cam_cur_exp_3$V_measure - flau_cur_exp_3$V_measure
t.test(v_measure, alternative = "greater")

# 3: we check if method 3 Camembert is significantly better than method 1 Flaubert
v_measure <- cam_cur_exp_3$V_measure - flau_cur_exp_1$V_measure
t.test(v_measure, alternative = "greater")
# No statistical significance!

# --- Automatic datasets ---
# We want to use a paired t-test
# We first check if the data is normally distributed

# Cam
shapiro.test(cam_auto_exp_1$V_measure)
shapiro.test(cam_auto_exp_2$V_measure) # NOT NORMAL
shapiro.test(cam_auto_exp_3$V_measure) # NOT NORMAL

# Flau
shapiro.test(flau_auto_exp_1$V_measure)
shapiro.test(flau_auto_exp_2$V_measure) # NOT NORMAL
shapiro.test(flau_auto_exp_3$V_measure) # NOT NORMAL

# Not all the data is normally distributed,
# Luckily, we wanted to conduct most tests on the first experiment,
# which is normally distributed

# 1: We check if method 1 auto is worse than method 1 curated
v_measure <- cam_auto_exp_1$V_measure - cam_cur_exp_1$V_measure
t.test(v_measure, alternative = "less")

v_measure <- flau_auto_exp_1$V_measure - flau_cur_exp_1$V_measure
t.test(v_measure, alternative = "less")

# 2: We check if camembert statistically outperforms flaubert on method 1
v_measure <- cam_auto_exp_1$V_measure - flau_auto_exp_1$V_measure
t.test(v_measure, alternative = "greater")
