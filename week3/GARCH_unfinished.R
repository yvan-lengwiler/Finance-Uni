#!/usr/bin/Rscript

# -----------------------------------------------------------------------------
# Estimating a GARCH process
#
# This script downloads some stock market data from finance.yahoo.com and
# them fits a GARCH(1,1)to the data. It also produces some informative charts.
#
# This script is almost finished. You just need to complete the function GARCH
# below.
#
# Date: 2024-03-15
# Author: Yvan Lengwiler
# License: MIT
#
# Add your names here.
# -----------------------------------------------------------------------------

# **** preparations ***********************************************************

# install and load some packages if they are missing
packages <- c('rstudioapi') 
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# THIS ONLY WORKS FROM WITHIN RSTUDIO
# select location of this file as working directory
if (.Platform$GUI == "RStudio") {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# **** parameters *************************************************************

#symbol    <- 'AMD'              # Advanced Micro Devices
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

from_date <- '2010-12-01'
to_date   <- '2023-12-31'
from_date <- '2005-01-01'
to_date   <- '2024-04-30'
interval  <- '1d'

# **** collect the data we need ***********************************************

# compute date codes
basedate <- as.Date("1970-01-01")
fromcode <- difftime(as.Date(from_date), basedate, units="secs")
tocode <- difftime(as.Date(to_date), basedate, units="secs")
# construct url
url <- paste0('https://query1.finance.yahoo.com/v7/finance/download/',
    symbol, "?period1=", fromcode, "&period2=", tocode, 
    "&interval=", interval, "&events=history&includeAdjustedClose=true")
# download data
data <- read.csv(url, header = TRUE)
# remove rows with missing values
data[data == 'null'] <- NA
data <- na.omit(data)
# number of observations per year
factor <- switch(
    interval,
    '1mo' = 12,                         # number of trading months
    '1wk' = 365.25/7,                   # number of trading weeks
    '1d'  = 365.25*5/7                  # number of trading days
)
interval_name <- switch(
    interval,
    '1mo' = 'monthly',
    '1wk' = 'weekly',
    '1d'  = 'daily'
)
dates  <- as.Date(data$Date)            # extract dates
price  <- as.numeric(data$Adj.Close)    # compute yields
yields <- diff(log(price)) * factor

# **** estimate GARCH *********************************************************

# param is a vector of five parameters: mu, omega, alpha, beta, and sigma2
# at the first date (initial_sigma2) of the sample.
#
# An initial param has to be supplied for the estimation. The estimation
# algorithm returns an optimized parameter vector.
# 
# The 'GARCH' function computes the log likelihood of the distribution implied
# by the parameters for each point in time. It returns this vector of log
# likelihoods ($logL), the residuals ($eps), and the variances ($sigma2) for
# each point in time.
# 
# 'negLL' uses GARCH and then sums $logL and multiplies it with -1, because
# the optimizer searches for a minimum, not a maximum.
# 
# Finally, nlminb is the algorithm we use to find the best fit. This is a
# non-linear optimizer based on Newton-Raphson that allows us to specify
# bounds for the parameters. We want 0<omega, 0<alpha<1, 0<beta<1, and
# initial_sigma2>0.

# log likelihood function
GARCH <- function(param, eps) {
  
    # give names to parameters
    omega       <- param[1]
    alpha       <- param[2]
    beta        <- param[3]
    init_sigma2 <- param[4]
    
    # WORK FOR YOU HERE
    # Program the GARCH process for sigma2 here and compute the log likelihood
    # for each observed period.
    
    # ...
    
    # return results
    # These should be two vectors, with one entry for each observed date.
    return(list("logL"=logL, "sigma2"=sigma2))
}

# objective function: negative sum of period log densities
negLL <- function(param, eps) {
    result <- GARCH(param, eps) 
    return(-sum(result$logL))
}

# starting point for non-linear optimization
#
# Note: experience dictates that alpha is normally small and beta is often
# rather large, so this is where we start. We calibrate omega and initial
# sigma2 such that, given the choice of alpha and beta, these values are
# compatible with the variance of the sample data, var(yields).
param <- c(
    var(yields) * (1 - 0.9),  # omega
    0.1,                      # alpha
    0.8,                      # beta
    var(yields)               # initial sigma2
)

# running the NL opt
mu  <- mean(yields)
eps <- yields - mu
out <- nlminb(param, negLL, eps=eps,
    lower=c(0,0,0), upper=c(Inf,1,1),
    control=list("iter.max"=1000, "eval.max"=1200))

# **** report results *********************************************************

cat('\n Results of GARCH estimation')
cat('\n ---------------------------\n')

cat('\n Asset:', symbol, '\n')

if (out$convergence != 0) {
    cat('\n Estimation did not converge!')
    cat('\n', out$message, '\n')
    stop()
}

# extract optimized parameters
omega       <- out$par[1]
alpha       <- out$par[2]
beta        <- out$par[3]
init_sigma2 <- out$par[4]

# extract eps and sigma of optimized process
result      <- GARCH(out$par, yields)
sigma2      <- result$sigma2
sigma       <- sqrt(sigma2)

# report
cat('\n Estimation converged in', out$iterations, 'iterations.')
cat('\n', out$message, '\n\n')
cat(' neg likelihood (minimized) =', out$objective, '\n\n')
if (alpha+beta >= 1.0) {
    cat('Estimated variance process is not stable!\n\n')
}
cat(' mu    =', mu, '\n')    
cat(' omega =', omega, '\n')    
cat(' alpha =', alpha, '\n')    
cat(' beta  =', beta, '\n')    
cat(' 1-alpha-beta =', 1-alpha-beta, '\n\n')
cat(' initial volatility (t=1) =', sqrt(init_sigma2), '\n')
cat(' unconditional volatility =', sqrt(omega/(1-alpha-beta)), '\n\n')

# **** make some charts *******************************************************

# ...

cat(' *** script has finished ***\n')
