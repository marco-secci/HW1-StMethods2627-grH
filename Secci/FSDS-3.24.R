set.seed(0)

# --- build a plausible population: Y = drinks in the past day ---
y <- 0:10                         # possible values
p <- c(60, 35, 20, 15, 10, 8, 6, 4, 2, 1, 0.5) # weights: big spike at 0, long right tail
p <- p / sum(p)                   # normalize
sum(p)                            # must be 1

mu    <- sum(y*p)                 # population mean (formula from §2.3)
sigma <- sqrt(sum((y-mu)^2*p))    # population SD   (formula from §2.3)
mu; sigma

barplot(p, names.arg = y, main = "Population distribution",
        xlab = "drinks in the past day", ylab = "probability")

# --- (a) one survey of n = 1000 people ---
n <- 1000
samp <- sample(y, n, replace = TRUE, prob = p)
mean(samp); sd(samp)              # compare with mu and sigma

phat <- table(factor(samp, levels = y)) / n   # sample proportions
barplot(rbind(p, phat), beside = TRUE, names.arg = y,
        legend.text = c("population", "sample"),
        main = "(a) Population vs. sample, n = 1000",
        xlab = "drinks in the past day", ylab = "proportion")

# --- (b) sampling distribution of Ybar: 10,000 surveys of n = 1000 ---
B <- 10000
ybars <- replicate(B, mean(sample(y, n, replace = TRUE, prob = p)))

mean(ybars); sd(ybars)            # simulated center and spread
center_theory <- mu               # E(Ybar) = mu
se_theory     <- sigma / sqrt(n)  # sigma_Ybar = sigma / sqrt(n)
center_theory; se_theory

hist(ybars, freq = FALSE, breaks = 50,
     main = "(b) Sampling distribution of Ybar, n = 1000",
     xlab = "sample mean drinks per day")
curve(dnorm(x, center_theory, se_theory), add = TRUE, col = "blue", lwd = 2)  # CLT prediction
abline(v = mu, col = "red", lty = 2)                           # mark mu

# --- comments ---
# 1. Y is a count (0, 1, 2, ...), so the population is discrete and can't be
#    negative. The most common answer is 0 drinks (P(0) = 0.37), many people
#    had 1-2, and a few drank a lot, so p decreases into a long right tail
#    (skewed right). This gives mu = 1.82 and sigma = 2.15: the SD is larger
#    than the mean, which is typical of a skewed count.
# 2. The single survey gives ybar = 1.85 and s = 2.20, very close to
#    mu = 1.82 and sigma = 2.15, and the sample proportions match p closely
#    in the barplot. They are not exactly equal because the sample is one
#    random draw: a different sample would give slightly different values.
#    With a larger n the sample data distribution resembles the population
#    more closely.
# 3. The 10,000 sample means have mean 1.819 and SD 0.0676. Theory gives
#    E(Ybar) = mu = 1.820 and sigma/sqrt(n) = 2.15/sqrt(1000) = 0.0680,
#    so simulation and theory agree. The SD of Ybar is about 32 times smaller
#    than sigma (sqrt(1000) ~ 31.6): averaging 1000 people cancels out most
#    of the person-to-person variation.
# 4. It is the distribution of the sample mean over all possible surveys of
#    1000 people: as if 10,000 agencies each ran the same survey and reported
#    their own ybar. It shows how much ybar varies from survey to survey,
#    so it tells us how close one survey's ybar is likely to be to mu
#    (almost always within about 3 SE = 0.20 drinks).
# 5. By the Central Limit Theorem, for large n the sampling distribution of
#    Ybar is approximately normal whatever the shape of the population.
#    n = 1000 is large enough to remove the skewness of the population, so
#    the histogram matches the normal curve N(mu, sigma/sqrt(n)).
