#!/usr/bin/Rscript

# -----------------------------------------------------------------------------
# Simulation of an equity
#
# This script downloads some stock market data from finance.yahoo.com and then
# performs a Monte Carlo simulation.
#
# The Monte-Carlo simulation is not implemented yet; you are supposed to do
# this yourself.
#
# Date: [enter date here]
# Author: [your name and Matr#]
# License: MIT
# -----------------------------------------------------------------------------

# **** preparations ***********************************************************

# install and load some packages if they are missing
packages <- c('yfR','scales','here','curl') 
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# select location of this file as working directory
setwd(here())

# **** parameters *************************************************************

#symbol    <- 'AMD'              # Advanced Micro Devices
#symbol    <- 'NVDA'             # NVIDIA Corp
#symbol    <- 'UBS'              # UBS Group, NYSE
symbol    <- 'UBSG.SW'          # UBS Group, SIX
#symbol    <- 'GOOG'             # Alphabet
#symbol    <- 'NESN.SW'          # Nestlé
#symbol    <- 'GM'               # General Motors
#symbol    <- 'MSFT'             # Microsoft
#symbol    <- 'PM'               # Philip Morris
#symbol    <- 'XOM'              # Exxon Mobil
#symbol    <- '^GSPC'            # S&P500 index
#symbol    <- 'BTC-USD'          # BTC in USD
#symbol    <- 'CHFUSD=X'         # CHF-USD FX

from_date  <- '2010-12-01'
to_date    <- '2024-12-31'
interval   <- 'monthly'

sim_years  <- 2                 # length of simulation in years
n          <- 1000              # number of simulations

draw_paths <- TRUE

# **** number of observations per year ***************************************

# we will use this later
factor <- switch(
    interval,
        'monthly' = 12,          # number of trading months
        'weekly'  = 365.25/7,    # number of trading weeks
        'daily'   = 365.25*5/7   # number of trading days
    )

# **** acquire data from finance.yahoo.com ***********************************

data <- yf_get(
    tickers = symbol,
    first_date = from_date,
    last_date = to_date,
    freq_data = interval,
    do_cache = TRUE,
    be_quiet = TRUE
)
price <- as.numeric(data$price_adjusted)
dates <- as.Date(data$ref_date)
# compute the yields
yield <- diff(log(price)) * factor

# **** compute Monte-Carlo simulation assuming normal distribution ************

# Write code that generates random future paths of the equity price. Your
# simulation should cover 'sim_years' years and you should generate 'n' such
# paths.
# 
# The following functions are helpful in this endeavor:
# 'rnorm' : This function generates normally distributed random numbers.
# 'apply' : This procedure applies a function to each element of a matrix.
# 'cumsum': Compute the cumulative sum of a vector, i.e. x1, x1+x2, 
#           x1+x2+x3, ..., x1+x2+x3+...+xn.
# You can use ?command (e.g. ?rnorm) to get some help for any command.

# ===> WORK FOR YOU HERE <===

# first and second moment of the data
mu <- mean(yield)
sigma <- sd(yield)

# how many steps do we have to simulate?
# ...

# compute random returns
# ...

# accumulate random returns to get log levels
# ...

# transform log level to plain level
# ...

# **** make some charts *******************************************************

# ===> WORK FOR YOU HERE <===

# ...

cat('\n*** script has finished ***\n')
