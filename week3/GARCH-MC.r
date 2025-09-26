#!/usr/bin/Rscript

# -----------------------------------------------------------------------------
# Simulating a GARCH process
#
# This script produces an MC simulation of an estimated GARCH process. Based
# on this simulation, it computes the Value at Risk.
#
# Date: 2025-09-25
# Author: your name
# License: MIT
# -----------------------------------------------------------------------------

# select location of this file as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# **** do the GARCH estimation ************************************************

# Use our GARCH script to estimate the GARCH process
source('GARCH.r')

# **** Monte Carlo and Value at Risk ******************************************

start_dates <- c('2008-09-19', '2008-12-19', '2009-09-18')   # sim start dates 

sim_length <- 5     # how many steps do we have to simulate?
# Make n larger for the final run to increase precision!
n <- 1000           # number of simulation runs

alpha_1 = vector()  # to collect the 1% Values at Risk
alpha_5 = vector()  # to collect the 5% Values at Risk

# WORK FOR YOU BELOW
for (the_date in start_dates) {
    
    # identify position of date in dates vector
    # ...
    
    # generate standard normal shocks
    # ...

    # initial sigma2 and eps
    # ...
    
    # first step of simulation (one day after 'the_date')
    # create matrices s2, e, r
    # ...
    
    # t = 2 ... (keep adding rows to s2, e, and r matrices)
    for (t in seq(2, sim_length)) {
        # ...
        # ...
    }
    
    # accumulate random returns to get log levels
    # note that e and r are annualized yeild; you must de-annualize before
    #   cumulating returns
    # ...
    # transform log level to plain level
    # ...
    
    # make plots as requested in Task 4 of the homework
    # ...
    
    # read out the values at risk as requested in Tasks 5 and 6 of the homework
    # ...
   
}

cat('\n')
VaR <- data.frame(start_dates, alpha_1, alpha_5, row.names=1)
print(VaR)

cat('\n *** script has finished ***\n')
