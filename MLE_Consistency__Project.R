# Project Work: Maximum Likelihood Estimation (MLE) and Consistency
# Topic: Application of MLE and Consistency Using a Real-life Dataset

# Load required libraries
library(tidyverse)

# Simulate a real-life dataset: Assume we are modeling the height of individuals in a population
# Generate data for the population assuming a normal distribution
set.seed(123)  # for reproducibility
n <- 1000  # sample size
mean_height <- 170  # population mean in cm
sd_height <- 10  # population standard deviation in cm
heights <- rnorm(n, mean = mean_height, sd = sd_height)

# Visualize the dataset
heights_df <- data.frame(Height = heights)
ggplot(heights_df, aes(x = Height)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Simulated Heights",
       x = "Height (cm)",
       y = "Frequency")

# Define the log-likelihood function for a normal distribution
log_likelihood <- function(params, data) {
  mu <- params[1]  # mean
  sigma <- params[2]  # standard deviation
  -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))  # negative log-likelihood
}

# Perform MLE to estimate parameters
initial_guess <- c(mean(heights), sd(heights))
mle_results <- optim(par = initial_guess, 
                     fn = log_likelihood, 
                     data = heights, 
                     method = "L-BFGS-B",
                     lower = c(-Inf, 1e-6))  # Ensure standard deviation > 0

# Extract MLE estimates
mle_mu <- mle_results$par[1]
mle_sigma <- mle_results$par[2]

# Display the results
cat("Maximum Likelihood Estimates:\n")
cat("Estimated Mean:", mle_mu, "\n")
cat("Estimated Standard Deviation:", mle_sigma, "\n\n")

# Check the consistency of the estimator
# Consistency means that as the sample size increases, the estimates approach the true values
sample_sizes <- seq(100, 1000, by = 100)
estimates <- data.frame(SampleSize = sample_sizes, EstimatedMean = NA, EstimatedSD = NA)

for (i in seq_along(sample_sizes)) {
  sample_data <- sample(heights, sample_sizes[i])
  mle_temp <- optim(par = initial_guess, 
                    fn = log_likelihood, 
                    data = sample_data, 
                    method = "L-BFGS-B",
                    lower = c(-Inf, 1e-6))
  estimates$EstimatedMean[i] <- mle_temp$par[1]
  estimates$EstimatedSD[i] <- mle_temp$par[2]
}

# Plot consistency of the estimates
estimates_long <- estimates %>% pivot_longer(cols = c(EstimatedMean, EstimatedSD), 
                                             names_to = "Parameter", 
                                             values_to = "Estimate")
ggplot(estimates_long, aes(x = SampleSize, y = Estimate, color = Parameter)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(title = "Consistency of MLE Estimates",
       x = "Sample Size",
       y = "Estimate") +
  geom_hline(yintercept = mean_height, linetype = "dashed", color = "blue", size = 1) +
  geom_hline(yintercept = sd_height, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 600, y = mean_height + 1, label = "True Mean", color = "blue") +
  annotate("text", x = 600, y = sd_height + 1, label = "True SD", color = "red")

