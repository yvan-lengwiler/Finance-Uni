#!/usr/bin/Rscript

# ----------------------------------------------------------------------------
# Inspecting stochastic properties of an equity
#
# This script downloads some stock market data from finance.yahoo.com and
# performs a few simple analyses.
#
# Date: 2024-03-15
# Author: Yvan Lengwiler
# License: GPL v2
# ----------------------------------------------------------------------------

# **** preparations **********************************************************

# install and load some packages if they are missing
packages <- c('rstudioapi','curl') 
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# select location of this file as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

#interval  <- '1d'                # 1d, 1wk, or 1mo
#interval  <- '1wk'               # 1d, 1wk, or 1mo
interval  <- '1mo'               # 1d, 1wk, or 1mo

from_date <- '2010-12-01'
to_date   <- '2023-12-31'

# **** number of observations per year ***************************************

# we will use this later
factor <- switch(
    interval,
        '1mo' = 12,           # number of trading months
        '1wk' = 365.25/7,     # number of trading weeks
        '1d'  = 365.25*5/7    # number of trading days
    )
interval_name <- switch(
    interval,
        '1mo' = 'monthly',
        '1wk' = 'weekly',
        '1d'  = 'daily'
    )

# **** constructing the url **************************************************

basedate <- as.Date("1970-01-01")
fromcode <- difftime(as.Date(from_date), basedate, units="secs")
tocode   <- difftime(as.Date(to_date), basedate, units="secs")

url <- paste0('https://query1.finance.yahoo.com/v7/finance/download/',
    symbol, "?period1=", fromcode, "&period2=", tocode, 
    "&interval=", interval, "&events=history&includeAdjustedClose=true")

# **** acquire data **********************************************************

data  <- read.csv(url, header=TRUE)
price <- as.numeric(data$Adj.Close)
dates <- data$Date

# **** plot it ***************************************************************

plot(1:length(price), log(price), main = symbol, 
     type='l', axes = FALSE, xlab = "")
labels <- dates[seq(1, length(dates), factor)]
axis(1, at=seq(1,length(dates),factor), labels)
axis(2)

# **** compute returns *******************************************************

# annualized return rate from one observation to the next
yield <- diff(log(price)) * factor

# **** plot it ***************************************************************

bullet_size <- sqrt(150/length(yield))
plot(yield, main = paste("annualized", interval_name, "return of", symbol),
     pch=20, cex=bullet_size, axes = FALSE, xlab = "")
labels <- dates[seq(1, length(dates)-1, factor)]
axis(1, at=seq(1,length(dates)-1,factor), labels)
axis(2)

# **** compute density estimate and Q-Q plot *********************************

# compute kernel and plot it
density_estimate <- density(yield, kernel = "epanechnikov")
plot(density_estimate,
     main = paste("density estimate of", interval_name, "returns on", symbol))

# plot normal density with same moments
mu <- mean(yield)       # mean return rate
sigma <- sd(yield)      # volatility
cat("mu =", round(100*mu,1), "%, sigma =", round(100*sigma,1), "%")
curve(dnorm(x, mean = mu, sd = sigma),
      from = min(yield), to = max(yield), add = TRUE, col = 2)

# make a Q-Q plot
qqnorm(yield, cex=bullet_size, 
       main = paste("Q-Q plot for", interval_name, "returns of", symbol))
qqline(yield)

cat('\n*** script has finished ***\n')
