#!/usr/bin/Rscript

# ----------------------------------------------------------------------------
# Implied risk-neutral density
# your name
# 2025-09-30
# license: MIT
# ----------------------------------------------------------------------------

# --- PREPARATIONS -----------------------------------------------------------

# install and load some packages if they are missing
packages <- c('readxl','here')
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# select location of this file as working directory
setwd(here())

# --- READ THE DATA ----------------------------------------------------------

# read data from Excel file
prices <- read_excel("option-prices.xlsx")

# provide additional information
t0 <- "2025-09-29"  # date of observation of option prices
t1 <- "2025-12-19"  # date of expiration of options
dT <- as.numeric(difftime(t1, t0, units="days") / 365.25)

r <- 4.33 / 100     # 3-months interest rate at t0

# *** TASK 2 : Please plot the data. The horizontal axis should represent the
# strike prices K and the vertivcal axis shows the option prices.

# ...

# --- DEFINE MIXED NORMAL DISTRIBUTION ---------------------------------------
# The normal distribution is defined by the mean and variance, The mixed
# normal is a weighted average of two or more normal distributions.
# For instance, one can have the first normal distribution with mean 190
# and standard deviation 40. In R, the density of this distribution is
# computed with
#     dnorm(x, 190, 40)
# This returns the density at the point x.
# Then, have a second normal, e.g.
#     dnorm(x, 120, 50)
# The mixed normal is then just a weighted average of these two densities,
# for instance
#     0.8 * dnorm(x,190,40) + 0.2 * dnorm(x,120,50)
# This would give 80% weight to the first and 20% weight to the second.
# You can combine more than two normals, provided all weights are positive
# and sum to one.

# define mixed normal density
w = c(0.8, 0.2)     # weights (must sum to 1.0)
m = c(190, 120)     # means
s = c(40, 50)       # standard deviations

# *** TASK 3
# Define the function f that computes the mixed normal density. Make it so
# that it can accommodate not just two, but an arbitrary number of normals
# (depending on the length of the vectors to pass in as arguments).
f <- function(x, w, m, s) {
#   ...
}

# plot mixed normal
x <- seq(0,400)
plot(x, f(x, w, m, s), type='l', lwd=2, lty='solid', col='black')
lines(x, f(x, w[1], m[1], s[1]), type='l', lwd=2, lty='solid', col='red')
lines(x, f(x, w[2], m[2], s[2]), type='l', lwd=2, lty='solid', col='blue')
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)

# --- OPTION PRICING USING RISK-NEUTRAL DENSITIES ----------------------------
# The payoff function at time of expiry of a call option with strike K is
# max(0, x-K), if x is the value of the underlying. The payoff of a put option
# is max(0, K-x).
# The current value of these options is the expected value of their payoff
# function using the risk-neutral density to weigh the different possible
# payoffs. In addition, the expected payoff so computed has tpo be discounted
# by multiplying with exp(-r*dT).

# PUT OPTIONS
# payoff function
integrand_put <- function(x, K, w, m, s) {
    max(K-x,0) * f(x, w, m, s)
}

# There is a procedure in R for numerical integration, called 'integrate'.
# Here is a solution using this procedure. However, I have found that 
# integrate is very unreliable and returns very inaccurate (I mean, wrong)
# results.
#
# put_price <- function(K, r, w, m, s) {
#     out <- integrate(integrand_put, lower=0, upper=K, K=K, w=w, m=m, s=s)
#     out <- exp(-r * dT) * out$value
#     return(out)
# }

# You should therefore find another way of performing this integration (maybe
# by manually performing the Riemann sum).

# *** TASK 4a
put_price <- function(K, w, m, s) {
#   ...
}

# Most likely, your function can only compute one option price a a time.
# It is, however, very useful to be able to pass in the whole vector of
# strike prices at once. The 'Vectorize' procedure allows us to so so.
put_prices <- Vectorize(put_price, "K")
# You can now computer put_prices(prices$K, w, m, s) and it should return
# all put prices given your risk-neutral mixed normal.

# *** TASK 4b: do the same for call options

# ...

# PLOT OPTION PRICES, GIVEN THE CHOSEN RISK-NEUTRAL DISTRIBUTION
matplot(prices$K, prices[,2:5], pch=16)

C <- call_prices(prices$K,w,m,s)
lines(prices$K, C)

P <- put_prices(prices$K,w,m,s)
lines(prices$K, P)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)

# --- DEFINE OBJECTIVE FUNCTION ----------------------------------------------
# We want to find a mixed normal distribution that describes the risk-neutral
# distribution so that the observed option prices are in alignment with the
# pricing model as closely as possible. We will minimize the squared
# residuals between observed option prices and theoretical prices.

# *** TASK 5
# compute residuals (observed prices minus theoretical prices based on RND)
resid <- function(r,w,m,s) {
#    ...
}

# Compute the sum of squares residuals (we want to minimize this value).
# the ssr function takes only one vector of arguments (param), not individual
# arguments (w, m, s). This is necessary in order to be able to apply the
# numerical minimizer later.
# Important: ssr will be called by the minimizer, and the minimizer will
# sometimes try weights that do not sum to one. Therefore, in the ssr
# procedure, do normalize w so that it sums to one.

# *** TASK 6
ssr <- function(param) {
#    ...
}

# --- PERFORM THE ESTIMATION -------------------------------------------------
# non-linear numerical minimization
param <- c(w,m,s)

# TASK 7: use nlminb or another of R's optimizers to find the best parameters
# that describe the risk-neutral density so that ssr is minimized.

# ...

# report results
cat("  means =", m, "\n")
cat("  volas =", s, "\n")
cat("weights =", w, "\n")
cat('neg likelihood (minimized) =', opt_result$objective, '\n')
cat(opt_result$message, '\n')

# plot fitted and empirical option prices
matplot(prices$K, prices[,2:5], pch=16)
C <- call_prices(prices$K,w,m,s)
lines(prices$K, C)
P <- put_prices(prices$K,w,m,s)
lines(prices$K, P)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)

# plot fitted mixed normal risk-neutral density
x <- seq(0,400)
plot(x, f(x, w, m, s), type='l', lwd=2, lty='solid', col='black')
lines(x, f(x, w[1], m[1], s[1]), type='l', lwd=2, lty='solid', col='red')
lines(x, f(x, w[2], m[2], s[2]), type='l', lwd=2, lty='solid', col='blue')
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
