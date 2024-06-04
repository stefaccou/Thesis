# In this script, we will check if the scores of BIC outperform the baselines
# of "three clusters" and "seven clusters"

library(tidyverse)
library(ggplot2)

# Load the data
v_measure = read.csv("Experiment_1/Curated/cluster_comparison/v_measure_results.csv")
adj_rand = read.csv("Experiment_1/Curated/cluster_comparison/adjusted_rand_results.csv")

# We want to see if the "score" column is significantly higher
# than the "three_cluster_baseline" and "seven_cluster_baseline" columns

# We will use a paired t-test to compare the scores
# checking if the scores are significantly higher than the baselines

# V-Measure
v_measure_scores = v_measure %>%
  #filter(optimal_clusters != 3) %>%
  select(score, three_cluster_baseline, seven_cluster_baseline)

v_measure_three_t = t.test(v_measure_scores$score, v_measure_scores$three_cluster_baseline, 
                                 alternative = "greater", paired = TRUE)
v_measure_three_t
# p = 0.04. Significant hogere waarden als we BIC gebruiken dan de baseline 3


v_measure_seven_t = t.test(v_measure_scores$score, v_measure_scores$seven_cluster_baseline, 
                                 alternative = "greater", paired = TRUE)
v_measure_seven_t
# p = 0.06. NET significant hogere waarden als we BIC gebruiken dan de baseline 7

# Adjusted Rand Index
adj_rand_scores = adj_rand %>%
  #filter(optimal_clusters != 3) %>%
  select(score, three_cluster_baseline, seven_cluster_baseline)

adj_rand_three_t = t.test(adj_rand_scores$score, adj_rand_scores$three_cluster_baseline, 
                                 alternative = "greater", paired = TRUE)
adj_rand_three_t
# p = 0.24. NIET significant hogere waarden als we BIC gebruiken dan de baseline

adj_rand_seven_t = t.test(adj_rand_scores$score, adj_rand_scores$seven_cluster_baseline, 
                                 alternative = "greater", paired = TRUE)
adj_rand_seven_t
# p = 0.02. Significant hogere waarden als we BIC gebruiken dan de baseline


# We kijken ook of de scores significant lager zijn dan die wanneer we expliciet
# het correcte aantal clusters meegeven aan het model (niet meer unsupervised!)

# V-Measure
v_measure_correct_t = t.test(v_measure$score, v_measure$correct_cluster_score, 
                                 alternative = "less", paired = TRUE)
v_measure_correct_t
# p = 0.61 Helemaal niet significant lager

# Adjusted Rand Index
adj_rand_correct_t = t.test(adj_rand$score, adj_rand$correct_cluster_score, 
                                 alternative = "less", paired = TRUE)
adj_rand_correct_t
# p = 0.44 Helemaal niet significant lager


# We bekijken het ook eens samen
adj_rand$type = "Adjusted Rand Index"
v_measure$type = "V-Measure"

all_scores = rbind(adj_rand, v_measure)


all_scores_three_t = t.test(all_scores$score, all_scores$three_cluster_baseline, 
                                 alternative = "greater", paired = TRUE)
all_scores_three_t
# p = 0.060

all_scores_seven_t = t.test(all_scores$score, all_scores$seven_cluster_baseline, 
                                 alternative = "greater", paired = TRUE)
all_scores_seven_t
# p = 0.005

all_scores_correct_t = t.test(all_scores$score, all_scores$correct_cluster_score, 
                              alternative = "less",paired = TRUE)
all_scores_correct_t
# p = 0.50


# We maken een tabel van de scores

all_scores %>%
  select(type, score, three_cluster_baseline, seven_cluster_baseline, correct_cluster_score) %>%
  pivot_longer(cols = c(score, three_cluster_baseline, seven_cluster_baseline, correct_cluster_score),
               names_to = "method", values_to = "value") %>%
  ggplot(aes(x = type, y = value, fill = method)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparison of clustering scores using FlauBERT",
       x = "Score type",
       y = "Score") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("blue", "red", "green", "purple")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We maken nu een tabel die de significantiescores voor de baselines
# in vergelijking met de BIC scores weergeeft

significant_scores = data.frame(type = c("Adjusted Rand Index", "V-Measure"),
                                three_cluster = c(all_scores_three_t$p.value, v_measure_three_t$p.value),
                                seven_cluster = c(all_scores_seven_t$p.value, v_measure_seven_t$p.value),
                                correct_cluster = c(all_scores_correct_t$p.value, adj_rand_correct_t$p.value))
