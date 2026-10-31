# --- given summaries ---
mu    <- 2.6      # population mean (Census)
sigma <- 1.5      # population SD (Census)
n     <- 225      # sample size
ybar  <- 2.4      # observed sample mean
s     <- 1.4      # observed sample SD

# --- (a) population distribution ---
# center = mu = 2.6 , spread = sigma = 1.5
# shape: skewed right (household size is a positive count with a long right tail)

# --- (b) sample data distribution ---
# center = ybar = 2.4 , spread = s = 1.4
# These are close to, but not equal to, mu and sigma because the sample is one
# random draw of 225 homes: its mean and SD vary from sample to sample.
# Its shape should look like the population (skewed right).

# --- (c) sampling distribution of the sample mean ---
center_c  <- mu                     # E(Ybar) = mu
se_theory <- sigma / sqrt(n)        # sigma / sqrt(n) = 1.5 / 15 = 0.1
se_hat    <- s / sqrt(n)            # estimated from the sample: 1.4 / 15 ~ 0.093
center_c; se_theory; se_hat
# shape: approximately normal, by the Central Limit Theorem (n = 225 is large),
# even though the population distribution is skewed.

# --- how unusual is the observed sample mean? ---
z <- (ybar - mu) / se_theory        # how many SEs is ybar from mu?
z
2 * pnorm(-abs(z))                  # two-sided probability, ~0.046

# --- plot of (c) ---
curve(dnorm(x, center_c, se_theory), from = 2.2, to = 3.0,
      main = "(c) Sampling distribution of Ybar, n = 225",
      xlab = "sample mean household size", ylab = "density")
abline(v = ybar, col = "red", lty = 2)   # mark the observed ybar

# --- comments ---
# 1. sigma = 1.5 is the spread of single households. The SD of Ybar is
#    sigma / sqrt(n) = 0.1, because averaging 225 homes cancels most of the variation.
# 2. The population is skewed, but the sampling distribution of Ybar is
#    approximately normal: that is the CLT for large n.
# 3. ybar = 2.4 is 2 SEs below mu = 2.6 (two-sided p ~ 0.046), so it is somewhat
#    unusual if the true mean is 2.6.
# 4. With only the sample we would use se_hat = s / sqrt(n) ~ 0.093, which is close
#    to the true SE of 0.1.
