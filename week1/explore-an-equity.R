#!/usr/bin/Rscript

# ----------------------------------------------------------------------------
# Inspecting stochastic properties of an equity
#
# This script downloads some stock market data from finance.yahoo.com and
# performs a few simple analyses.
#
# Date: 2024-09-09
# Author: Yvan Lengwiler
# License: MIT
# ----------------------------------------------------------------------------

# **** preparations **********************************************************

# install and load some packages if they are missing
packages <- c('yfR','scales','here','curl') 
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# select location of this file as working directory
setwd(here())

# **** parameters ************************************************************

#symbol    <- 'AMD'              # Advanced Micro Devices
#symbol    <- 'GOOG'             # Alphabet
#symbol    <- 'NESN.SW'          # Nestlé
symbol    <- 'GM'               # General Motors
#symbol    <- 'MSFT'             # Microsoft
#symbol    <- 'PM'               # Philip Morris
#symbol    <- 'XOM'              # Exxon Mobil
#symbol    <- '^GSPC'            # S&P500 index
#symbol    <- 'BTC-USD'          # BTC in USD
#symbol    <- 'CHFUSD=X'         # CHF-USD FX

interval  <- 'monthly'           # daily, weekly, monthly

from_date <- '2010-12-01'
to_date   <- '2023-12-31'
#to_date   <- '2024-08-26'

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

# **** plot it ***************************************************************

plot(dates, log(price), main = symbol, type='l', axes = FALSE)
axis.Date(1, dates, at = seq(dates[1], dates[length(dates)], "years"))
axis(2)

# **** compute returns *******************************************************

# annualized return rate from one observation to the next
yield <- diff(log(price)) * factor

# **** plot it ***************************************************************

bullet_size <- sqrt(150/length(yield))
plot(dates[-1], yield, 
     main = paste("annualized", interval, "return of", symbol),
     pch=20, cex=bullet_size, axes = FALSE)
axis.Date(1, dates[-1], at = seq(dates[2], dates[length(dates)], "years"))
axis(2)

# **** compute density estimate and Q-Q plot *********************************

# compute kernel and plot it
density_estimate <- density(yield, kernel = "epanechnikov")
plot(density_estimate,
     main = paste("density estimate of", interval, "returns on", symbol))

# plot normal density with same moments
mu <- mean(yield)       # mean return rate
sigma <- sd(yield)      # volatility
cat("mu =", round(100*mu,1), "%, sigma =", round(100*sigma,1), "%")
curve(dnorm(x, mean = mu, sd = sigma),
      from = min(yield), to = max(yield), add = TRUE, col = 2)

# make a Q-Q plot
qqnorm(yield, cex=bullet_size, 
       main = paste("Q-Q plot for", interval, "returns of", symbol))
qqline(yield)

cat('\n*** script has finished ***\n')
