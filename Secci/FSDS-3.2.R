# H0: pi = 0.5
# Data:
n <- 1648 # Poll size
p_obs <- 0.515 # Observed proportion
p0 <- 0.5 # Presumed proportion

# (a) Find the STANDARD ERROR under p0:
se <- sqrt(p0*(1-p0)/n)
se

# (b.i) Run a simulation:
set.seed(0)

# Generating 1M polls
B <- 1e6

# Running the simulation:
results <- rbinom(B, n, p0) / n
mean(results); sd(results)

# Plotting results:
hist(results, breaks = 50, main = "Sample proportions under H0: pi = 0.5", xlab = "Sample proportion")
abline(v = p_obs, col = "red", lwd=2)

# Calculating the probability of a 50% population of giving 51.5% or more:
mean(results>= p_obs)

# (b.ii) Using the previously found SE:
z <- (p_obs - p0) / se
z

# Double checking:
1 - pnorm(z)
