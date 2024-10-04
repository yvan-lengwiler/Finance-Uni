# Check out the slides for more explanations.
# The following is the key code needed for the Monte-Carlo of the GARCH process.

# identify position of date
idx <- which(dates == the_date)

# generate standard normal shocks
z <- matrix(rnorm(sim_length*n, 0, 1), nrow=sim_length)

# initial sigma2 and eps
s2_init <- matrix(sigma2[idx], nrow=1, ncol=n)
e_init  <- matrix(eps[idx], nrow=1, ncol=n)

# first step of simulation (one day after 'the_date')
s2 <- matrix(omega + alpha*e_init^2 + beta*s2_init, nrow=1, ncol=n)
e  <- matrix(z[1,] * sqrt(s2), nrow=1, ncol=n)
r  <- matrix(mu + e, nrow=1, ncol=n)

# t = 2 ... (keep adding rows to s2, e, and r matrices)
for (t in seq(2, sim_length)) {
    s2 <- rbind(s2, omega + alpha * e[t-1,]^2 + beta * s2[t-1,])
    e  <- rbind(e, z[t,] * sqrt(s2[t,]))
    r  <- rbind(r, mu + e[t,])
}