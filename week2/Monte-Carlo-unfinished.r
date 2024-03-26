#!/usr/bin/Rscript

# ----------------------------------------------------------------------------
# Simulation of an equity
#
# This script downloads data of one asset from finance.yahoo.com and performs
# a simple analysis with it (computes a kernel estimation and plots the
# estimated density of returns).
#
# This script might be useful on its own right. The main purpose for the
# class, however, is to provide code for downloading equity data. From that
# you should develop the code necessary to create a Monte-Carlo simulation.
#
# Date: 2023-02-21
# Author: Yvan Lengwiler
# 
# Please provide a date and the names and Matr # of all members of your
# study group here. If you work alone, just provide the corresponding
# information for yourself.
# 
# ----------------------------------------------------------------------------

# **** preparations **********************************************************

# install and load some packages if they are missing
packages <- c('rstudioapi','curl')
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# select location of this file as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# *** parameters *************************************************************

#symbol    <- '^GSPC'           # S&P 500
#symbol    <- 'AMD'             # Advanced Micro Devices
#symbol    <- 'GOOG'            # Alphabet
#symbol    <- 'NESN.SW'         # Nestlé
#symbol    <- 'GM'              # General Motors
symbol    <- 'MSFT'            # Microsoft
#symbol    <- 'CHFUSD=X'        # CHF-USD FX
from_date <- '2000-01-01'
to_date   <- '2024-02-29'
interval  <- '1mo'             # 1d, 1wk, or 1mo

# *** constructing the url ***************************************************

basedate  <- as.Date("1970-01-01")
fromcode  <- difftime(as.Date(from_date), basedate, units="secs")
tocode    <- difftime(as.Date(to_date)  , basedate, units="secs")

url <- paste0('https://query1.finance.yahoo.com/v7/finance/download/',
    symbol, "?period1=", fromcode, "&period2=", tocode, 
    "&interval=", interval, "&events=history&includeAdjustedClose=true")

# *** acquire data ***********************************************************

data <- read.csv(url, header=TRUE, row.names='Date')
price <- data$Adj.Close

# *** compute returns ********************************************************

factor <- switch(
    interval,
        '1mo' = 12,           # number of trading months
        '1wk' = 365.25/5,     # number of trading weeks
        '1d'  = 365.25*5/7    # number of trading days
)
yield <- diff(log(price)) * factor

# *** compute density estimate and make a chart ******************************

# plot estimated density and normal density with same moments
density_estimate <- density(yield, kernel = "epanechnikov")
plot(density_estimate, main = paste("density estimate of returns on", symbol))
mu <- mean(yield)
sigma <- sd(yield)
curve(dnorm(x, mean = mu, sd = sigma),
      from = min(yield), to = max(yield), add = TRUE, col = 2)

# *** compute and graph a Monte-Carlo simulation *****************************

# ...
