#' @title Template for Various Power Analyses
#' @description For ease of use in quick sample size estimations for RCT design

#' @title Load pwr package
#' @keywords internal
#' @noRd
#
library(pwr)

#' @title One-sample t-test power analysis 
#' @description Use case: baseline equivalence or non-inferiority
#' @keywords internal
#' @noRd

# Assumptions
mu0 <- 5       # Reference mean (e.g., control or population)
mu1 <- 4.5     # Expected mean under treatment
sd  <- 1.5     # Estimated SD
alpha <- 0.05
power <- 0.80

# Compute standardized effect size
d <- (mu1 - mu0) / sd
print(paste("Effect size (Cohen's d):", round(d, 3)))

# One-sample t-test power analysis
pwr.t.test(d = d, power = power, sig.level = alpha, 
           type = "one.sample", alternative = "less")


#' @title One-way ANOVA power analysis
#' @description Use case: For 2+ arm RCT endpoint analysis (must follow assumption of randomized design)
#' @keywords internal
#' @noRd
# 

# Assumptions
groups <- 3
f <- 0.25  # Medium effect size: small = 0.10, medium = 0.25, large = 0.40
alpha <- 0.05
power <- 0.80

# ANOVA power analysis
pwr.anova.test(k = groups, f = f, sig.level = alpha, power = power)


#' @title Multilevel model power analysis
#' @description Use case: 2+ timepoints where study question involves change over time
#' @keywords internal
#' @noRd
# 
# Power analysis for MLM using 'simr' package
# Install if needed: install.packages("simr")
library(lme4)
library(simr)

# Simulate a 3-time-point RCT (simplified model)
# Fixed effect: Group × Time interaction

# Simulate data structure
set.seed(123)
n_subjects <- 50  # starting point for sample size
time <- factor(rep(c("pre", "post", "followup"), each = n_subjects))
group <- factor(rep(rep(c("control", "treatment"), each = n_subjects/2), times = 3))
subject <- factor(rep(1:n_subjects, times = 3))

# Generate toy dataset
y <- rnorm(n_subjects * 3)
df <- data.frame(subject, time, group, y)

# Fit a mixed-effects model
model <- lmer(y ~ time * group + (1 | subject), data = df)

# Set desired effect
fixef(model)["timepost:groupcontrol"] <- 0.3  # adjust for hypothesized effect

# Extend for power analysis
model_ext <- extend(model, along = "subject", n = 100)  # try with 100 subjects

# Power analysis on the interaction effect
powerSim(model_ext, fixed("timepost:groupcontrol", "t"), nsim = 100)



#' @title Power analysis on known sample size
#' @description Post-hoc power estimation 
#' @keywords internal
#' @noRd
#' 
n <- 50  # per group
d <- 0.4 # expected effect size (Cohen’s d)
alpha <- 0.05

# Two-sample t-test post-hoc power estimation
pwr.t.test(n = n, d = d, sig.level = alpha, type = "two.sample", alternative = "two.sided")


