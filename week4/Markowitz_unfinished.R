#!/usr/bin/Rscript

# ----------------------------------------------------------------------------
# Efficient frontier
#
# This script downloads some stock market data from finance.yahoo.com and
# deposits them in a data frame. The task demands that you compute the mean
# yields and the covariance matrix, and then compute the unconstrained and the
# constrained efficient frontier. Also make plots that show the result.
# 
# There are packages for R that make this very easy (such as "quantmod" and
# "PortfolioAnalytics"). I would advise you not to use those for now. If you do
# it more "manually", you will better understand what these packages do under
# the hood.
# 
# Computing the efficient frontier requires that you minimize the variance
# of the returns of a portfolio subject to expected mean return being not
# smaller than some value. This is a quadratic programming problem because the
# objective function has a quadratic form. Various optimizers are available for
# R that can do that. A simple one is in the quadprog library (the function is
# solve.QP). You are free to use another optimizer, of course.
#
# Date: 2024-05-03
# Author: Yvan Lengwiler
# License: MIT
#
# Please provide the current date and the names and Matr # of all members of
# your study group here. If you work alone, just provide the corresponding
# information for yourself.
# Push your solution to your github repository.
# 
# ----------------------------------------------------------------------------

# **** preparations **********************************************************

# install and load some packages if they are missing
packages <- c('rstudioapi','curl','collapse','quadprog')
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# THIS ONLY WORKS FROM WITHIN RSTUDIO
# select location of this file as working directory
if (.Platform$GUI == "RStudio") {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# **** parameters ************************************************************

# Make sure to select only assets denominated in the same currency. If you
# mix currencies, the script will run but the results are garbage.
assets    <- c('^GSPC', 'AAPL','MSFT','AMZN','GOOGL','TSLA','BRK-B','JPM',
               'AVGO','UNH','V','XOM','MA','HD','PG','JNJ','COST')
nassets   <- length(assets)

from_date <- '2010-12-01'
to_date   <- '2024-04-30'
interval  <- '1mo'               # 1d, 1wk, or 1mo

# **** number of observations per year ***************************************
# we will use this later
factor <- switch(
    interval,
        '1mo' = 12,           # number of trading months
        '1wk' = 365.25/7,     # number of trading weeks
        '1d'  = 365.25*5/7    # number of trading days
)

# **** acquire data **********************************************************

basedate  <- as.Date("1970-01-01")
fromcode  <- difftime(as.Date(from_date), basedate, units="secs")
tocode    <- difftime(as.Date(to_date), basedate, units="secs")

price <- NULL
for (symbol in assets) {
    url <- paste0('https://query1.finance.yahoo.com/v7/finance/download/',
        symbol, "?period1=", fromcode, "&period2=", tocode, 
        "&interval=", interval, "&events=history&includeAdjustedClose=true")
    data <- read.csv(url, header=TRUE, row.names='Date')
    cat(sprintf('%s: %i observations\n', symbol, nrow(data)))
    price <- cbind(price, data$Adj.Close)
}
cat("Make sure that all assets have the same number of observations!\n")
colnames(price) <- assets
rownames(price) <- rownames(data)

# **** compute returns *******************************************************

yield <- diff(log(price)) * factor  # log diff (= growth rate)

# **** compute moments *******************************************************
# compute vector of mean returns and covariance matrix

#...

# **** mean-volatility plot of assets ****************************************
# make a scatter plot with mu on the vertical and sigma on the horizontal axis
# and place the individual assets into this plot

#...

# **** unconstrained optimization ********************************************
# minimize portfolio variance subject to a minimum expected return, for a
# sequence of minimum expected returns.
# Hint: solve.QP is your friend for this task.

#...

# **** constrained optimization **********************************************
# same as before, but do not allow any weights on individual assets to be
# negative
# Hint: This is a variation of the unconstrained optimization. You should be
# able to keep much of your code. You have to do work on the specification of
# the constraints, though.

#...

# **** plot of the efficient frontier ****************************************

#...

# **** plot optimal portfolios ***********************************************

#...

cat('*** script has finished ***\n')
