
# Load packages
pacman::p_load(tidyverse, pwr)

# Read data - long format
data <- read_csv(
  here::here("cleaned-data","manually-cleaned-data-long.csv")
)

# Set number of predictors (assuming factor variables counted once for each level, excluding the base/comparison level)
k <- 5

# Find the effect size from the R^2
f2 <- (0.1)/(1-0.1)

# Use power package to find sample size for these values
# Specifying desired power 90% and significance level 0.05
obj <- pwr.f2.test(
  u = k,
  v = NULL,
  f2 = f2,
  power = 0.90,
  sig.level = 0.05)

# If we have v = n - k - 1
# Then n = v + k + 1
# Get n from calculated v:
obj$v + k + 1

# Sample size must be at least 153.86
# So minimum sample size is 154

# Just for check: calculate power at the sample size we have (88; with 1 NA)
# k = 5
pwr.f2.test(
  u = 5,
  v = 154 - 5 - 1,
  f2 = f2,
  sig.level = 0.05
)
