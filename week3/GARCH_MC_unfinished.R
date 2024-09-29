#!/usr/bin/Rscript

# -----------------------------------------------------------------------------
# Simulating a GARCH process
#
# This script produces an MC simulation of an estimated GARCH process. Based
# on this simulation, it computes the Value at Risk.
#
# Date: 2024-03-15
# Author: Yvan Lengwiler
# License: MIT
# -----------------------------------------------------------------------------

# select location of this file as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# **** do the GARCH estimation ************************************************

# Use our GARCH script to estimate the GARCH process
source('GARCH.r')

# **** Monte Carlo and Value at Risk ******************************************

start_dates <- c('2008-09-19', '2008-12-19', '2024-04-02')   # sim start dates 

sim_length <- 22    # how many steps do we have to simulate?
n <- 1000           # number of simulation runs

alpha_1 = vector()  # to collect the 1% Values at Risk
alpha_5 = vector()  # to collect the 5% Values at Risk

# WORK FOR YOU BELOW
for (the_date in start_dates) {
    
    # identify position of date
    # ...
    
    # generate standard normal shocks
    # ...

    # initial sigma2 and eps
    # ...
    
    # first step of simulation
    # ...
    
    # t = 2 ...
    for (t in seq(2, sim_length)) {
        # ...
        # ...
    }
    
    # accumulate random returns to get log levels
    # ...
    # transform log level to plain level
    # ...
    
    # make some plots
    # ...
    
    # read out the values at risk
    # ...
   
}

cat('\n')
VaR <- data.frame(start_dates, alpha_1, alpha_5, row.names=1)
print(VaR)

cat('\n *** script has finished ***\n')
