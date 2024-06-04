# In this script, we will visualise our data
# We load in the files
library(tidyverse)
library(ggplot2)
library(reshape2)

# We compare BIC results
# -------------- PART 1: CURATED DATA ----------------
df_cur <- read.csv("Experiment_2/Curated/BIC/results.csv", encoding = "UTF-8", sep = ";")
df_cur$Model <- as.factor(df_cur$Model)

df_cur_cam <- read.csv("Experiment_2/Curated/BIC/Camembert_scores.csv", encoding = "UTF-8", sep = ";")
df_cur_flau <- read.csv("Experiment_2/Curated/BIC/Flaubert_scores.csv", encoding = "UTF-8", sep = ";")
df_cur_all_in_one <- read.csv("Experiment_2/Curated/BIC/all_in_one_scores.csv", encoding = "UTF-8", sep = ";")
# We add the "All_in_one" column to the df_cur dataframe, grouping by X
# We only want to add the "All_in_one" column
# We thus first remove "Type" column from df_cur_all_in_one
df_cur_all_in_one <- df_cur_all_in_one %>%
  select(-Type)
df_cur_all <- merge(df_cur, df_cur_all_in_one, by = "X")
df_cur_words <- df_cur_all %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average")

# We create a "no cluster" line, that gives the F1 scores if no clustering would take place
no_cluster_f1_line_cur <- df_cur_all_in_one$F1_no_cluster[df_cur_all_in_one$X == "Global_average"]


# We make the same boxplot for comparison of v measure across models

ggplot(df_cur_words, aes(x = Model, y = V_measure, fill = Model)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal()

# and the plot for F1 scores
ggplot(df_cur_words, aes(x = Model, y = F1, fill = Model)) +
  geom_boxplot() +
  geom_hline(data = df_cur_all_in_one, aes(yintercept = no_cluster_f1_line_cur),
             linetype = "dashed", color = "Black") +
  annotate("text", x = 0, y = no_cluster_f1_line_cur, 
           label = "No clustering", hjust = -0.1, vjust = -0.75)  +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal()

# And for Adjusted Rand Index score
ggplot(df_cur_words, aes(x = Model, y = Rand, fill = Model)) +
  geom_boxplot() +
  geom_hline(data = df_cur_all_in_one, aes(yintercept = no_cluster_f1_line_cur),
             linetype = "dashed", color = "Black") +
  annotate("text", x = 0, y = no_cluster_f1_line_cur, 
           label = "No clustering", hjust = -0.1, vjust = -0.75)  +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  labs(title="Adjusted Rand Index scores for both models on task 1",
       y="Adjusted Rand Index") +
  theme_minimal()
  


# Reshape the data
df_cur_long <- melt(df_cur_words, id.vars = "Model", measure.vars = c("V_measure", "F1", "Rand"))


# Create the boxplot
ggplot(df_cur_long, aes(x = Model, y = value, fill = Model)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  geom_boxplot() +
  facet_wrap(~ variable) +
  labs(title = "Weighted and Overall Scores", x = " ", y = "Score") +
  # We remove the legend
  theme_minimal() +
  theme(legend.position = "none")
# Scores should not be compared, rather this plot serves as a comparison between models
# for each of the scores, which all highlight a different aspect
# F1 is a pretty good measure of overall performance
# Adjusted Rand Index takes chance into account, and shows the model performs
#   remarkably better than just chance, with a very similar gemiddelde between models
# v-measure takes all clusters into account, past the greatest cluster
#   and can thus be seen as an overall cluster evalutation, rather than "performance"
#   evaluation (where F1 is a better metric)


# We make a barplot for the score of each model for each word
# We remove the rows "Average_nouns", "Average_verbs" and "Global_average"
# And visualize them in another plot

df_cur_averages <- df_cur_all %>%
  filter(X == "Average_nouns" | X == "Average_verbs" | X == "Global_average")

# For the v-measure metric
ggplot(df_cur_words, aes(x = X, y = V_measure, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal() +
  labs(title = "V measure clustering Scores for each word", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Remarkable: hit or miss for both models on some words
#   CamemBERT fails with 'Faculté' whereas this word poses no problem for FlauBERT
#   FlauBERT fails with 'Tirer', but CamemBERT has no problem here
# We will look into the reason for these inconsistencies
# We also see that 'Bureau', which was included to show the performance on more
#   polysemous words, is the lowest scoring noun overall.

# And for F1 scores
ggplot(df_cur_words, aes(x = X, y = F1, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  geom_segment(data = df_cur_words, aes(x = X, xend = X, y = 0, yend = F1_no_cluster), 
               alpha = 0.8, color = "grey", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Experiment 2, curated dataset: F1 scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# We oberve similar tendencies on this metric.
# 'Bureau' and 'Supporter' get low scores on both models.
# For 'Supporter', both models clustered the data into many more clusters
#   than our manual annotation did. LOOK AT VISUALISATION TO LOOK IF IT DID SO WRONGLY
#   OR IF THIS CAN BE EXPLAINED BY A DIFFERENCE IN GRANULARITY!


# And for Adjusted Rand Index scores
ggplot(df_cur_words, aes(x = X, y = Rand, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal() +
  labs(title = "Adjusted Rand Index scores for each word", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Adjusted Rand Index scores show that on some words
#   Bureau (C+F), Supporter (C+F), Faculté (C), Tirer (F)
#   the models just barely get higher scores than a randomly assigned cluster model would.
# This is bad. On other words, this problem does not occur.
#   We do a qualitative check on these words. The problem seems to reside in clustering.



# We do the same for the overall scores
ggplot(df_cur_averages, aes(x = X, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  geom_segment(data = df_cur_averages, aes(x = X, xend = X, y = 0, yend = Overall_no_cluster), 
               alpha = 0.8, color = "grey", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Overall Average Scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# And for the v-measure scores
ggplot(df_cur_averages, aes(x = X, y = V_measure, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal() +
  labs(title = "V_measure average Scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# And for the F1 scores
ggplot(df_cur_averages, aes(x = X, y = F1, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  geom_segment(data = df_cur_averages, aes(x = X, xend = X, y = 0, yend = F1_no_cluster), 
               alpha = 0.8, color = "grey", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "F1 average Scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# we will compare the different types of scores:
df_cur_scores <- df_cur_words %>% 
  select(X, Weighted, Overall, V_measure, F1, Model) %>% 
  gather(key = "Score", value = "Value", -X, -Model)

# We will make a boxplot for the scores of Flaubert
ggplot(df_cur_scores, aes(x = Score, y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~Model) +
  labs(title = "Scores for FlauBERT and for CamemBERT", x = "Score", y = "Value")

# We also make a barplot, comparing the scores of "F1" and "V_measure" for each word
df_cur_scores_select <- df_cur_scores %>% filter(Score %in% c("F1", "V_measure"))
ggplot(df_cur_scores_select, aes(x = X, y = Value, fill = Score)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#d435e6","#3d912f")) +
  theme_minimal() +
  facet_wrap(~Model, ncol = 1, nrow = 2) +
  labs(title = "Scores for FlauBERT and for CamemBERT", x = "Words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#------------------------PART 2: AUTOMATIC DATA--------------------------------

df_auto <- read.csv("Experiment_2/Automatic/BIC/results.csv", encoding = "UTF-8", sep = ";")
df_auto$Model <- as.factor(df_auto$Model)

df_auto_cam <- read.csv("Experiment_2/Automatic/BIC/Camembert_scores.csv", encoding = "UTF-8", sep = ";")
df_auto_flau <- read.csv("Experiment_2/Automatic/BIC/Flaubert_scores.csv", encoding = "UTF-8", sep = ";")
df_auto_all_in_one <- read.csv("Experiment_2/Automatic/BIC/all_in_one_scores.csv", encoding = "UTF-8", sep = ";")
# We add the "All_in_one" column to the df_auto dataframe, grouping by X
# We only want to add the "All_in_one" column
# We thus first remove "Type" column from df_auto_all_in_one
df_auto_all_in_one <- df_auto_all_in_one %>%
  select(-Type)
df_auto_all <- merge(df_auto, df_auto_all_in_one, by = "X")
df_auto_words <- df_auto_all %>%
  filter(X != "Average_nouns" & X != "Average_verbs" & X != "Global_average")


no_cluster_f1_line_auto <- df_auto_all_in_one$F1_no_cluster[df_auto_all_in_one$X == "Global_average"]


# We make the same boxplot for comparison of v measure across models

ggplot(df_auto_words, aes(x = Model, y = V_measure, fill = Model)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal()

# and the plot for F1 scores
ggplot(df_auto_words, aes(x = Model, y = F1, fill = Model)) +
  geom_boxplot() +
  geom_hline(data = df_auto_all_in_one, aes(yintercept = no_cluster_f1_line_auto),
             linetype = "dashed", color = "Black") +
  annotate("text", x = 0, y = no_cluster_f1_line_auto, 
           label = "No clustering", hjust = -0.1, vjust = -0.75)  +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal()





# We make a barplot for the score of each model for each word
# We remove the rows "Average_nouns", "Average_verbs" and "Global_average"
# And visualize them in another plot

df_auto_averages <- df_auto_all %>%
  filter(X == "Average_nouns" | X == "Average_verbs" | X == "Global_average")

# And for the v_measure
ggplot(df_auto_words, aes(x = X, y = V_measure, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal() +
  labs(title = "V measure clustering Scores for each word", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# And for F1 scores
ggplot(df_auto_words, aes(x = X, y = F1, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  geom_segment(data = df_auto_words, aes(x = X, xend = X, y = 0, yend = F1_no_cluster), 
               alpha = 0.8, color = "grey", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "F1 scores for each word", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))





# We make a barplot for the average weighted scores
ggplot(df_auto_averages, aes(x = X, y = Weighted, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal() +
  labs(title = "Weighted Average Scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# We do the same for the overall scores
ggplot(df_auto_averages, aes(x = X, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  geom_segment(data = df_auto_averages, aes(x = X, xend = X, y = 0, yend = Overall_no_cluster), 
               alpha = 0.8, color = "grey", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Overall Average Scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# And for the v-measure scores
ggplot(df_auto_averages, aes(x = X, y = V_measure, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  theme_minimal() +
  labs(title = "V_measure average Scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# And for the F1 scores
ggplot(df_auto_averages, aes(x = X, y = F1, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6)) +
  scale_fill_manual(values = c("#3452eb","#eb3452")) +
  geom_segment(data = df_auto_averages, aes(x = X, xend = X, y = 0, yend = F1_no_cluster), 
               alpha = 0.8, color = "grey", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "F1 average Scores", x = "Words", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))




