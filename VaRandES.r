library(quantmod);library(ggplot2);library(moments);library(MASS);library(metRology)
set.seed(123)

# Get Wilshire 5000 data
wilshire <- getSymbols(Symbols = "WILL5000IND", src = "FRED", auto.assign = FALSE)
wilshire <- na.omit(wilshire)
wilshire <- wilshire["1980-01-01/2020-06-01"]

# Create time series plot of data
ggplot(data = wilshire, aes(x = Index, y = WILL5000IND)) +
  geom_line(size = 1.0) +
  xlab("") + 
  ylab("") +
  ggtitle("Wilshire 5000 Index")

# Calculate daily log returns
log_returns <- diff(log(wilshire))[-1]

# Calculate mean and std. deviation
mu <- round(mean(log_returns), 6) # Mean: 0.000428
sigma <- round(sd(log_returns), 6) # Standard Deviation: 0.011063

# Produce histogram of returns overlaid with a kernel density plot
ggplot(data = log_returns, aes(x = WILL5000IND)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.01,
                 color = "black",
                 fill = "lightblue") +
  geom_density(size = 1.0) + 
  xlab("Daily Log Returns") +
  ggtitle("Wilshire 5000 Index",
         subtitle = "Mean = 0.000428, Std. Deviation = 0.011063")
         
# Print skewness and kurtosis
cat("Skewness:", skewness(log_returns),
    "Kurtosis:", kurtosis(log_returns),
    sep = "\n")

# Perform Jarque-Bera test of normality
jarque.test(as.vector(log_returns))

# 95% confidence level
alpha <- 0.05

#### Method 1: Estimate using empirical distribution ####

# Take a random sample of 100,000 observations from the empirical distribution with replacement
sample.empirical <- sample(as.vector(log_returns), 1000000, replace = TRUE)

# Calculate VaR 
VaR.empirical <- round(quantile(sample_returns, alpha), 6)

# Calculate ES
ES.empirical <- round(mean(sample.empirical[sample.empirical < VaR.empirical]), 6)

# Print results
cat("Value at Risk:", VaR.empirical, "Expected Shortfall:", ES.empirical, sep = "\n")

#### Method 2: Estimate using rescaled Student-t ####

# Fit Student-t distribution to log returns data
t.fit <- fitdistr(as.vector(log_returns), "t")

# Scale distribution based on 100,000 observation sample
sample.t <- rt.scaled(100000, mean = t.fit$estimate[1], sd = t.fit$estimate[2], df = t.fit$estimate[3])

# Calculate VaR and ES based on this distribution
VaR.t <- quantile(sample.t, alpha)
ES.t <- round(mean(sample.t[sample.t < VaR.t]), 6)

# Print results
cat("Value at Risk:", VaR.t, "Expected Shortfall:", ES.t, sep = "\n")
