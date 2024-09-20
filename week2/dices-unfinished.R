#!/usr/bin/Rscript

# -----------------------------------------------------------------------------
# Simulation throwing a dice 
#
# This script simulates a dice. Please extend this to multiple dices.
#
# Date: 2024-03-20
# Author: Yvan Lengwiler
# License: MIT
# -----------------------------------------------------------------------------

# **** parameters *************************************************************

s <- 6      # maximum number of eyes on the dices
n <- 10000  # number of simulations

# **** compute Monte-Carlo simulation assuming uniform distrib over integers **

throws <- floor(runif(n, min=1, max=s+1))

# **** compute probabilities **************************************************

prob <- c()
for ( t in seq(1,s) ) {
    count <- length(throws[throws==t])
    prob <- c(prob, count/n)
}
barplot(prob)

# **** extend this to multiple dices ******************************************
#
# 1) Simulate d dices.
# 2) For each throw, compute the sum of the d dices (store these sums in a
#    vector called 's')
# 3) Plot the density, as we did last week (with 'density' and 'plot')
# 4) Rund the following for different d: 2,4,10,100

d <- 4       # number of dices

# WORK FOR YOU HERE:
# Cdd a few lines here to simulate n throws of d dices
# Compute the sume of the thrown eyes of the dices and put this into
# a vector calles 's'.

# ...

# plot density of the sum of d dices
density_estimate <- density(s)
plot(density_estimate, type='l')
# plot density of normal distrbution with same moments
mu <- mean(s)
sigma <- sd(s)
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = 2)

cat('\n*** script has finished ***\n')
