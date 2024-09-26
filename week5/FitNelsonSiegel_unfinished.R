#!/usr/bin/Rscript

# ----------------------------------------------------------------------------
# Nelson-Siegel model
#
# This script fits market prices of zero and coupon bonds to the Nelson-Siegel
# model.
#
# The skript is unfinished. Please follow the handout to understand what you
# are supposed to do.
#
# Date: 2024-06-30
# Author: Yvan Lengwiler
# License: MIT
#
# Please provide a date and the names and Matr # of all members of your
# study group here. If you work alone, just provide the corresponding
# information for yourself.
#
# ----------------------------------------------------------------------------

# **** preparations **********************************************************

# install and load some packages if they are missing
packages <- c('lubridate','rstudioapi')
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# THIS ONLY WORKS FROM WITHIN RSTUDIO
# select location of this file as working directory
if (.Platform$GUI == "RStudio") {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# **** load data *************************************************************

import_data <- function(filename, price_type) {
    df <- read.csv(filename)
    # drop missing values
    df <- subset(df, select=c('coupon', 'maturity', price_type))
    df[df=='#N/A'] <- NA
    df <- na.omit(df)
    # rename price column
    colnames(df)[3] <- 'price'
    df$price <- as.numeric(df$price)
    # return to caller
    return(df)
}

# **** Nelson-Siegel model ***************************************************

NSfactors <- function(m, tau) {
    # compute and return level, slope, and curvature
    # ...
    return(l)
}

NSyield <- function(m, coef) {
    # compute Nelson-Siegel yield
    # ...
    return(y)
}

PVzero <- function(m, coef) {
    # compute present value of a zero bond, using Nelson-Siegel yield
    # ...
    return(p)
}

# **** dealing with coupon bonds *********************************************

unbundle <- function(bond, obs_date) {
    # unbundle the zero bonds contained in a coupon bond
    # return a dataframe with two columns: t (maturity) and cf (cashflow at t)
    # ...
    return(data.frame(t,cf))
}

duration <- function(bond, obs_date) {
    # compute the duration (avg time of cash flows) of a coupon bond
    return(dur)
}

PVbond <- function(coef, bond, obs_date){
    # compute the present value of a coupon bond, given Nelson-Siegel yield
    # curve
    # ...
    return(v[1])
}

# **** estimation ************************************************************

res <- function(coef, df, obs_date) {
    # compute deviations (residuals) of market prices from prices implied by
    # Nelson-Siegel yield curve
    return(eps)
}

SSR <- function(coef, df, weight, obs_date) {
    # compute weighted sum of squared residuals
    # ...
    # return(...)
}

estimate <- function(df, obs_date, price_type) {
    # This is a more challenging program. Use a non-linear optimizer
    # (I use nlminb, but there are other ones as well) to find the
    # coefficients (beta1 to beta3 and tau) that minimizes the
    # weighted SSR.
    # ...
    return(out)
}

# **** main ******************************************************************

# ==== set parameters

datafile <- '2024-06-25-CH.csv'
obs_date <- as.Date("2024-06-25")
price_type <- 'mid'

# ==== TASK 3
cat('\nTASK 3\n')
df <- import_data(datafile, price_type)
bond <- df[4,]
print(unbundle(bond, obs_date))

# ==== TASK 4
cat('\nTASK 4\n')
print(duration(bond, obs_date))

# ==== TASK 6
cat('\nTASK 6\n')
cat('make four plots (different taus)\n')

# levels of m for whch the factors will be computed
m <- c(0.001,0.02,0.05,0.1,0.15,0.2,0.25,0.3,0.38,
       0.48,0.6,0.8,1,1.2,1.5,2,3,4,5,7,10)
# make charts for three different taus
for (tau in c(0.1, 0.2, 0.6, 1.2)) {
    l <- NSfactors(m, tau)
    matplot(m,l, type='l', lwd=2, lty=1, col=c('blue','aquamarine3','red'), 
            xlab="m", ylab="", main=paste("tau =",tau))
    grid()
}

# ==== TASK 9
cat('\nTASK 9\n')
cat('make two plots (estimated yield curve and residuals)\n')

# ---- estimate
df <- import_data(datafile, price_type)
out <- estimate(df, obs_date, price_type)
coef <- out$par

# ---- plot it
m <- c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,4,5,6,7,8,9,10,
       12,14,16,18,20,22,25,30)
yc <-NSyield(m, coef)
plot(m, yc, type='l', lwd=2, col='springgreen2',
    main=paste0(obs_date, ", ", price_type, " prices"),
    xlab="maturity", ylab="yield"
)
grid()

# --- check residuals
dur <- c()
for (b in seq(1, nrow(df), 1)) {
    bond <- df[b,]
    d <- duration(bond, obs_date)
    dur <- append(dur, d)
}
eps  <- res(coef, df, obs_date)
weps <- eps / dur
plot(dur,weps)
grid()
