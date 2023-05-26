
# Load packages
pacman::p_load(tidyverse, pwr)

# Read data
data <- read_csv(
  here::here("cleaned-data","manually-cleaned-data-long.csv")
)

# Set number of predictors (assuming factor variables count for every level)
k <- 5

# Find the effect size from the R^2
f2 <- (0.1)/(1-0.1)

obj <- pwr.f2.test(
  u = k,
  v = NULL,
  f2 = f2,
  power = 0.90,
  sig.level = 0.05)

# If we have v = n - k - 1
# Then n = v + k + 1
# Get n from calculated v
obj$v + k + 1


# plot.power.htest(obj)


# Just for check: calculate power at the sample size we have (88; with 1 NA)
# k = 6
pwr.f2.test(
  u = 6,
  v = 88 - 6 - 1,
  f2 = (summary(M1)$r.squared)/(1 - summary(M1)$r.squared),
  sig.level = 0.05
)
