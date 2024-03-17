#!/usr/bin/R

# ----------------------------------------------------------------------------
# Download data from finance.yahoo.com
#
# This script downloads some stock market data from finance.yahoo.com, 
# deposits them in a dataframe, and then computes an estimate of the
# distribution of he return rates.
#
# Date: 2023-02-21
# Author: Yvan Lengwiler
# ----------------------------------------------------------------------------

# **** preparations **********************************************************

# install and load some packages if they are missing
packages <- c('curl')
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

curl_download(url, 'temp.csv')
data  <- read.csv('temp.csv')
price <- data$Adj.Close

# *** compute returns and density estimate ***********************************

factor <- switch(interval,
            '1mo' = 12,
            '1wk' = 365.25 / 5,
            '1d'  = 365.25 * 5/7
)
yield <- diff(log(price)) * factor
density_estimate <- density(yield, kernel = "epanechnikov")

# *** make a chart ***********************************************************

# plot estimated density and normal density with same moments
plot(density_estimate, main = paste("density estimate of returns on", symbol))
mu <- mean(yield)
sigma <- sd(yield)
curve(dnorm(x, mean = mu, sd = sigma),
      from = min(yield), to = max(yield), add = TRUE, col = 2)
