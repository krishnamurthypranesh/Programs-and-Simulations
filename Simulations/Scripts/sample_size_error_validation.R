
# Introduction ------------------------------------------------------------

# In this script, I'm just looking to verify [empirically] the relationship
# between sample size and the value of the sample metric we're considering.
# If you're wondering how this goes, the error in the estimation of the sample 
# metric is inversely proportional to the square-root of the sample size.

# Load Libraries ----------------------------------------------------------

library(ggplot2)  # load the ggplot2 for visualizations

# load the dplyr and tidyr packages for data wrangling
library(dplyr)  

library(tidyr)


# Run the simulation ------------------------------------------------------

# data frame to store simulation values
sim_df <- data.frame(
  sample_size = seq(1000, 100000, by = 1000),  # sample sizes from 1000 to 100000
  # in steps of 1000. So, there are  100 sample sizes
  sample_means = vector("numeric", 100),
  sample_sd = vector("numeric", 100)
)

# getting the sample means for various sample sizes
sim_df[, 2] <- sapply(sim_df[, 1], function(x) mean(rnorm(x, mean = 0, sd = 2)))
# getting the sample standard deviations for various sample sizes
sim_df[, 3] <- sapply(sim_df[, 1], function(x) sd(rnorm(x, mean = 0, sd = 2)))

# plotting the sample metrics and the sample sizes
sim_df %>% 
  gather(sample_means:sample_sd, key = "metric", value = "value") %>% 
  ggplot() + aes(sample_size, value) + geom_point(aes(col = as.factor(metric))) +
  geom_smooth(aes(col = metric), se = F) + 
  facet_wrap(~metric)


sim_df %>% 
  ggplot() + aes(sample_size, sample_means) + geom_point() + 
  geom_smooth(span = 0.4)

# building a linear model to check the relationship
sim_mod1 <- lm(sample_means ~ sample_size, data = sim_df)


summary(sim_mod1)  # not significant...

# we can see the significance of sample size if we plot the difference in
# sample mean with the true mean instead of the simulated sample mean.

# plotting the errors in estimated and true values as a function of sample size
sim_df %>% 
  mutate(actual_diff_mean = abs(0 - sample_means),
         actual_diff_sd = abs(2 - sample_sd)) %>% 
  gather(c(actual_diff_mean, actual_diff_sd), key = "metric", value = "value") %>% 
  ggplot() + aes(sample_size, value, col = as.factor(metric)) + geom_point() + 
  geom_smooth(span = 1) + facet_wrap(~metric, scales = "free_y")

# Clearly a non-linear relationship

sim_df$sample_size > 1000 & sim_df$sample_size < 5000
