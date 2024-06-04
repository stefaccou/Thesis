# In this script, we will see how much the scores go down between
# the curated and the automatic dataset
library(tidyverse)
library(ggplot2)

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

# We bind all the data frames
df = rbind(experiment_1, experiment_2, experiment_3, 
           experiment_4, experiment_5, experiment_6)

# For each experiment, we calculate the difference between the scores
# of the curated and automatic datasets

f1_diffs <- df %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average") %>%
  select(X, Experiment, Model, F1, Mode) %>%
  group_by(X, Experiment, Model) %>%
  mutate(Difference = mean(F1[Mode == "Curated"]) - mean(F1[Mode == "Automatic"]),
         Metric = "F1") %>%
  select(X, Experiment, Model, Difference, Metric)

v_measure_diffs <- df %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average") %>%
  select(X, Experiment, Model, V_measure, Mode) %>%
  group_by(X, Experiment, Model) %>%
  mutate(Difference = mean(V_measure[Mode == "Curated"]) - mean(V_measure[Mode == "Automatic"]),
         Metric = "V_measure") %>%
  select(X, Experiment, Model, Difference, Metric)
  
rand_diffs <- df %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average") %>%
  select(X, Experiment, Model, Rand, Mode) %>%
  group_by(X, Experiment, Model) %>%
  mutate(Difference = mean(Rand[Mode == "Curated"]) - mean(Rand[Mode == "Automatic"]),
         Metric = "Rand") %>%
  select(X, Experiment, Model, Difference, Metric)

# We bind the three data frames
df_diffs <- rbind(f1_diffs, v_measure_diffs, rand_diffs)

# We plot the difference between the scores of the curated and automatic datasets
ggplot(df_diffs, aes(x = Model, y = Difference, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Experiment) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Difference between the scores of the curated and automatic datasets",
       x = "Model",
       y = "Difference") +
  theme(legend.position = "bottom")

# We make a table with the differences.
df_diffs %>%
  group_by(Experiment, Model, Metric) %>%
  summarise(Difference = mean(Difference)) %>%
  spread(Metric, Difference) %>%
  arrange(Experiment, Model) %>%
  knitr::kable()
# We present this table in a nice format
df_diffs %>%
  group_by(Experiment, Model, Metric) %>%
  summarise(Difference = mean(Difference)) %>%
  spread(Metric, Difference) %>%
  arrange(Model, Experiment) %>%
  knitr::kable()

# Result: on average, scores for automatic dataset are worse than for curated dataset
# The difference is a lot more pronounced for the third experiment.
    # ==> Our idea why: sparse vectors become very long as their dimension grows with
    # each new word in the vocabulary. This makes it harder for the clustering algorithm
    # to find the right clusters

# The difference is the smallest for the first experiment.

