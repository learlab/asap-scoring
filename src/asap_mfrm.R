install.packages("ordinal")

library(dplyr)
library(tibble)
library(ordinal)
library(tidyr)
library(psych)

setwd("~/active-projects/asap_scoring/data")
df <- read.csv("asap_final_scores_pruned_IN_only.csv")

colnames(df)
unique(df$Reader1)
unique(df$Reader2)

# Make three dataframes for each of the readers and their scores
df1 <- df[c("essay_id", "Reader1", "Score1")]
df1_wide <- df1 %>%
  pivot_wider(names_from = Reader1, values_from = Score1)

df2 <- df[c("essay_id", "Reader2", "Score2")]
df2_wide <- df2 %>%
  pivot_wider(names_from = Reader2, values_from = Score2)

df3 <- df[c("essay_id", "Reader3_expert", "Score3_adjudicate")]
df3_wide <- df3 %>%
  pivot_wider(names_from = Reader3_expert, values_from = Score3_adjudicate)

# Merge the first two
merged_df <- df1_wide %>%
  mutate(across(everything(), ~ coalesce(., df2_wide[[cur_column()]])))

# Merge the first two
merged_df <- merged_df %>%
  mutate(across(everything(), ~ coalesce(., df3_wide[[cur_column()]])))

# Look at the dataframe
head(merged_df)
# Sanity check. Each row should have either 2 or three raters
as.data.frame(rowSums(!is.na(merged_df[-1])))$rowSums

# Check mean and count for each rater

describe(merged_df)


###################
#######MFRM########
###################

# Reshape to long format

colnames(merged_df)[-1]

long_df <- merged_df %>%
  pivot_longer(
    cols = colnames(merged_df)[-1],
    names_to = "rater_id",
    values_to = "score"
  ) %>%
  filter(!is.na(score))  # Remove rows with NA scores
describe(long_df)
hist(long_df$score)


# Fit a cumulative link mixed model
ordinal_model <- clmm(
  factor(score) ~ 1 + (1 | essay_id) + (1 | rater_id),  # Random effects for essays and raters
  data = long_df,
  link = "logit"  # Use logit link for ordinal data
)

# Summarize the model
summary(ordinal_model)

#########################
###Plot The Difficulty###
#########################

coefficients = as.vector(ordinal_model$coefficients)
seq_along(coefficients)
subtract_values <- seq(-10, 15, by = 0.5)
# Initialize an empty list to store columns
prob_columns <- list()
# Loop through each log likelihood
for (i in seq_along(coefficients)) {
  # Compute probabilities for the current log likelihood
  probabilities <- exp(coefficients[i] - subtract_values) / (1 + exp(coefficients[i] - subtract_values))
  # Store the probabilities as a column in the list
  prob_columns[[paste0("LogLik_", i)]] <- probabilities
}
prob_df
# Convert the list of columns into a dataframe
prob_df <- as.data.frame(prob_columns)
colnames(prob_df) <- c("<2", "<3", "<4", "<5", "<6")
# Add the subtract_values as the first column (optional)
prob_df <- cbind(Subtract_Value = subtract_values, prob_df)
prob_df$facet_score <- seq(-10, 15, by = 0.5)

# Make a cumulative distribution function
prob_df_long <- prob_df %>%
  pivot_longer(cols = starts_with("<"), names_to = "Score_Range", values_to = "Probability")

library(ggplot2)
ggplot(prob_df_long, aes(x = facet_score, y = Probability, color = Score_Range)) +
  geom_line(size = 1) +  # Draw lines for each Score_Range
  labs(
    title = "CDF by Score Range",
    x = "Facet Score",
    y = "Probability",
    color = "Score Range"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom


# Make a probability distribution function
pdf <- prob_df['facet_score']
pdf$'1' <- prob_df$"<2"
pdf$'2' <- prob_df$"<3" - prob_df$"<2" 
pdf$'3' <- prob_df$"<4" - prob_df$"<3" - prob_df$"<2" 
pdf$'4' <- prob_df$"<5" - prob_df$"<4" - prob_df$"<3" - prob_df$"<2" 
pdf$'5' <- prob_df$"<6" - prob_df$"<5" - prob_df$"<4" - prob_df$"<3" - prob_df$"<2" 
pdf$'6' <- 1 - prob_df$"<6"


pdf_long <- pdf %>%
  pivot_longer(cols = c('1', '2', '3', '4', '5', '6'), names_to = "Score_Range", values_to = "Probability")

pdf_long
library(ggplot2)
ggplot(pdf_long, aes(x = facet_score, y = Probability, color = Score_Range)) +
  geom_line(size = 1) +  # Draw lines for each Score_Range
  labs(
    title = "PDF by Score Range",
    x = "Facet Score",
    y = "Probability",
    color = "Score Range"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 1))  +  
  geom_jitter(data=merged_long_df, aes(x = total_score, y = 0, fill = score), 
              width = 0.1, height = 0.02, alpha = 0.5)  # Add jittered points

pdf_long


ggplot(pdf_long, aes(x = facet_score, y = Probability, color = Score_Range)) +

  geom_jitter(data = merged_long_df, aes(x = total_score, y = 0, fill = score), 
              width = 0.1, height = 0.02, alpha = 0.5, shape = 21) +  # Use `fill` instead of `color`
  labs(
    title = "PDF by Score Range",
    x = "Facet Score",
    y = "Probability",
    color = "Score Range",
    fill = "Actual Score"  # Add a label for the fill aesthetic
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 1))

##################################################
###Look at Random Effects for Individual Raters###
##################################################

# Look at rater and essay effects
essay_effects <- ranef(ordinal_model)$essay_id
rater_effects <- ranef(ordinal_model)$rater_id


ranef(ordinal_model)
# Look at distributions of rater and essay effects
hist(essay_effects[[1]])  # Distribution of essay effects
hist(rater_effects[[1]])  # Distribution of rater effects
barplot(rater_effects[[1]], names.arg = rownames(rater_effects), las = 2, main = "Rater Effects")

essay_effects$essay_id <- rownames(essay_effects)
colnames(essay_effects) <- c('essay_score', 'essay_id')
rater_effects$rater_id <- rownames(rater_effects)
colnames(rater_effects) <- c('rater_score', 'rater_id')

# Merge essay scores
merged_long_df <- merge(long_df, essay_effects, by = "essay_id", all.x = TRUE)
# Merge rater scores
merged_long_df <- merge(merged_long_df, rater_effects, by = "rater_id", all.x = TRUE)
# Add a new column for the sum of scores
merged_long_df$total_score <- merged_long_df$essay_score + merged_long_df$rater_score

merged_long_df
#########################
###Check against score###
#########################

scores_df
essay_effects$essay_id <- row.names(essay_effects)
scores2 <- df[c("essay_id", "Score2")]
scores_df <- merge(scores2, essay_effects, on="essay_id")
scores_df
plot(scores_df$Score2, scores_df$essay_score)
cor.test(scores_df$Score2, scores_df$essay_score)
