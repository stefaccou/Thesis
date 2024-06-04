# In this script, we will compare the results of the different methods
library(tidyverse)
library(ggplot2)

# We load in the necessary files
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
                experiment_4, experiment_5, experiment_6)
df_words <- df_all %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average")
df_averages <- df_all %>%
  filter(X == "Average_nouns" | X == "Average_verbs" | X == "Global_average")

# We make two sub-dataframes, one for each "Model"
df_flau <- df_averages %>%
  filter(Model == "Flaubert")
df_cam <- df_averages %>%
  filter(Model == "Camembert")

# Together:
df_scores <- df_all %>% 
  select(X, V_measure, F1, Rand, Experiment, Mode, Model) %>% 
  gather(key = "Score", value = "Value", -X, -Experiment, -Mode, -Model)

# Camembert:
df_scores_cam <- df_cam %>% 
  select(X, Weighted, Overall, V_measure, F1, Rand, Experiment, Mode) %>% 
  gather(key = "Score", value = "Value", -X, -Experiment, -Mode)
# Flaubert:
df_scores_flau <- df_flau %>%
  select(X, Weighted, Overall, V_measure, F1, Rand, Experiment, Mode) %>% 
  gather(key = "Score", value = "Value", -X, -Experiment, -Mode)

# We compare methods, using the same dataset
df_scores_cam_curated <- df_scores_cam %>% 
  filter(Score %in% c("F1", "V_measure", "Rand")) %>%
  filter(Mode == "Curated")
df_scores_flau_curated <- df_scores_flau %>%
  filter(Score %in% c("F1", "V_measure", "Rand")) %>%
  filter(Mode == "Curated")
df_scores_cam_auto <- df_scores_cam %>% 
  filter(Score %in% c("F1", "V_measure", "Rand")) %>%
  filter(Mode == "Automatic") 
df_scores_flau_auto <- df_scores_flau %>% 
  filter(Score %in% c("F1", "V_measure", "Rand")) %>%
  filter(Mode == "Curated")

ggplot(df_scores_cam_curated, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() +
  facet_wrap(~Experiment, ncol = 3, nrow = 1) +
  labs(title = "Scores for Experiments", x = "Words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#-------------Visualisation: score per experiment-------------------
# Curated data:


# CamemBERT
cam_cur <- df_scores_cam_curated %>%
  # We change column names: "_" becomes " "
  mutate(Score = gsub("Rand", "ARI", Score),
         Score = gsub("_", " ", Score),
         X = gsub("_", " ", X), 
         Experiment = gsub("_", " ", Experiment)) %>%
  # we change the order: F1 should come first
  mutate(Score = factor(Score, levels = c("F1", "V measure", "ARI")))

ggplot(cam_cur, aes(x = X, y = Value, fill = Experiment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#a8199c","#f7dd5c", "#f75c7b")) +
  theme_minimal() +
  facet_wrap(~Score, ncol = 3, nrow = 1) +
  labs(title = "CamemBERT: Scores on curated dataset", x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  # We fix the y-axis to go from 0-1
  scale_y_continuous(limits = c(0, 0.90))

# FlauBERT
flau_cur <- df_scores_flau_curated %>%
  # We change column names: "_" becomes " "
  mutate(Score = gsub("Rand", "ARI", Score),
         Score = gsub("_", " ", Score),
         X = gsub("_", " ", X), 
         Experiment = gsub("_", " ", Experiment)) %>%
  # we change the order: F1 should come first
  mutate(Score = factor(Score, levels = c("F1", "V measure", "ARI")))


ggplot(flau_cur, aes(x = X, y = Value, fill = Experiment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#a8199c","#f7dd5c", "#f75c7b")) +
  theme_minimal() +
  facet_wrap(~Score, ncol = 3, nrow = 1) +
  labs(title = "FlauBERT Scores on curated dataset", x = "Words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  # We fix the y-axis to go from 0-1
  scale_y_continuous(limits = c(0, 0.90))



# Automatic data:

# CamemBERT
cam_aut <- df_scores_cam_auto %>%
  # We change column names: "_" becomes " "
  mutate(Score = gsub("Rand", "ARI", Score),
         Score = gsub("_", " ", Score),
         X = gsub("_", " ", X), 
         Experiment = gsub("_", " ", Experiment)) %>%
  # we change the order: F1 should come first
  mutate(Score = factor(Score, levels = c("F1", "V measure", "ARI")))

ggplot(cam_aut, aes(x = X, y = Value, fill = Experiment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#a8199c","#f7dd5c", "#f75c7b")) +
  theme_minimal() +
  facet_wrap(~Score, ncol = 3, nrow = 1) +
  labs(title = "CamemBERT Scores on automatic dataset", x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  # We fix the y-axis to go from 0-1
  scale_y_continuous(limits = c(0, 0.90))

# FlauBERT
ggplot(df_scores_flau_auto, aes(x = X, y = Value, fill = Experiment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#a8199c","#f7dd5c", "#f75c7b")) +
  theme_minimal() +
  facet_wrap(~Score, ncol = 3, nrow = 1) +
  labs(title = "FlauBERT Scores on automatic dataset", x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))




ggplot(df_scores_cam_auto, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() +
  facet_wrap(~Experiment, ncol = 3, nrow = 2) +
  labs(title = "Scores for Experiments", x = "Words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(df_scores_cam_auto, aes(x = X, y = Value, fill = Experiment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442", "#34deeb")) +
  theme_minimal() +
  facet_wrap(~Score, ncol = 3, nrow = 1) +
  labs(title = "Scores for Experiments", x = "Words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggplot(df_scores_cam_auto, aes(x = X, y = Value, fill = Experiment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() +
  facet_wrap(~Score, ncol = 3, nrow = 1) +
  labs(title = "Scores for Experiments", x = "Words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#------------------Comparison experiment 1 and 2-------------------
# We now compare experiment one and two,
#  Focussing on all words
first_exp_scores <- df_scores %>% 
  filter(Experiment %in% c("Experiment 1", "Experiment 2")) %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average")

# We make two sub-dataframes, one for each "Model"
first_exp_scores_cam <- first_exp_scores %>% 
  filter(Model == "Camembert")
first_exp_scores_flau <- first_exp_scores %>%
  filter(Model == "Flaubert")

# WE make the same dataset, but Zooming in on the averages of nouns, verbs and the total
first_exp_avg <- df_scores %>% 
  filter(Experiment %in% c("Experiment 1", "Experiment 2")) %>%
  filter(X %in% c("Average_nouns", "Average_verbs", "Global_average"))
# We make two sub-dataframes, one for each "Model"
first_exp_avg_cam <- first_exp_avg %>% 
  filter(Model == "Camembert")
first_exp_avg_flau <- first_exp_avg %>%
  filter(Model == "Flaubert")


ggplot(first_exp_scores_cam, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() + 
  # We facet wrap by both experiment and mode
  facet_wrap(~ Mode + Experiment) +
  labs(title = "Camembert scores for first and second experiment", x = ""
       ) +
  # We rotate the x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(first_exp_scores_flau, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() + 
  # We facet wrap by both experiment and mode
  facet_wrap(~ Mode + Experiment) +
  labs(title = "Flaubert scores for first and second experiment", x = "") +
  # We rotate the x-axis labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


#------------- AAN TE PASSEN------------------
# We make the same plots with the averages
vis_cam_avg <- first_exp_avg_cam %>%
  mutate(X = gsub("_", " ", X),
         Score = gsub("Rand", "ARI", Score),
         Score = gsub("V_measure", "V-measure", Score)
         ) %>%
  # F1 should come first, then V-measure, then ARI
  mutate(Score = factor(Score, levels = c("F1", "ARI", "V-measure")))
ggplot(vis_cam_avg, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() + 
  # We facet wrap by both experiment and mode
  facet_wrap(~ Mode + Experiment) +
  labs(title = "Camembert: Average scores", x = "",
       subtitle = "Clustering of contextualized embeddings (Experiment 1)\nDynamic-pattern-based substitute clustering (Experiment 2)"
       )
ggplot(vis_cam_avg, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() + 
  # We facet wrap by both experiment and mode
  facet_wrap(~ Mode + Experiment) +
  labs(title = "Clustering of contextualized embeddings (1) or symmetric-pattern-based substitutes (2)", x = "",
       subtitle = "Average scores for CamemBERT") +
  # We fix the y-axis to go from 0-0.8
  scale_y_continuous(limits = c(0, 0.80))


vis_flau_avg <- first_exp_avg_flau %>%
  mutate(X = gsub("_", " ", X),
         Score = gsub("Rand", "ARI", Score),
         Score = gsub("V_measure", "V-measure", Score)
         ) %>%
  # F1 should come first, then V-measure, then ARI
  mutate(Score = factor(Score, levels = c("F1", "ARI", "V-measure")))
ggplot(vis_flau_avg, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f", "#f5a442")) +
  theme_minimal() +
  # We facet wrap by both experiment and mode
  facet_wrap(~ Mode + Experiment) +
  labs(title = "Clustering of contextualized embeddings (1) or symmetric-pattern-based substitutes (2)", 
       x = "", subtitle = "Average scores for FlauBERT") +
  # We fix the y-axis to go from 0-0.8
  scale_y_continuous(limits = c(0, 0.80))





