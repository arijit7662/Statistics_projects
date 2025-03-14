# Load required libraries
library(tidyverse)
library(car)  # For ANOVA tests
library(bestNormalize)  # For handling non-normal data transformations

# Load the data (assuming the file is saved locally as "Two_Way_Data.csv")
data=Two_Way_m_obs

# Display the structure of the dataset
str(data)

# Check if data is balanced (6 observations per cell)
table(data$State, data$Age)

# Convert factors to categorical variables if needed
data$State <- as.factor(data$State)
data$Age <- as.factor(data$Age)

# Check for normality using Shapiro-Wilk test
data_normality <- data %>%
  group_by(State, Age) %>%
  summarise(p_value = shapiro.test(Case)$p.value)

print("Shapiro-Wilk Test Results for Each Group:")
print(data_normality)

# If data is not normal, normalize it using bestNormalize
if (any(data_normality$p_value < 0.05)) {
  print("Data is not normally distributed. Applying normalization...")
  normalization <- bestNormalize(data$Case)
  data$Case <- normalization$x.t
  print("Normalization applied using:")
  print(normalization$chosen_transform)
} else {
  print("Data is normally distributed. Proceeding without transformation...")
}
# Perform Two-Way ANOVA
two_way_anova <- aov(Case ~ State + Age + State:Age, data = data)

# Summary of the ANOVA results
summary(two_way_anova)

# Check for interaction effects using an interaction plot
interaction.plot(data$Age, data$State, data$Case,
                 col = rainbow(length(unique(data$State))),
                 legend = TRUE, 
                 xlab = "Age Group",
                 ylab = "Mean Cases",
                 main = "Interaction Plot")

# Perform diagnostics for ANOVA
par(mfrow = c(2, 2))
plot(two_way_anova)

# Post-hoc test if needed (e.g., Tukey HSD)
tukey_results <- TukeyHSD(two_way_anova)
print(tukey_results)

