# Key steps to solve homework for week 2.
 

# This is where the Monte-Carlos simulation occurs: we generate a bunch of
# random draws.

# compute random returns
yield_mc <- matrix(rnorm(sim_length*n, mu, sigma)/factor, nrow=sim_length)

# NOTE: We could just as well have computed the non-annualized yields
#   yield <- diff(log(price))
# without multiplying with 'factor', and then draw random returns from that
# distribution
#   yield_mc <- matrix(rnorm(sim_length*n, mu, sigma), nrow=sim_length)
# without dividing by 'factor' again.

# Next we aggregate the generated random returns to compute the simulated
# equity log price levels (with cumsum = cumulative sum).

# accumulate random returns to get log levels, preappend a zero
log_level_mc <- apply(yield_mc, MARGIN=2, FUN=cumsum)

# For aestetic reasons, we add a vector of zeros, so that the simulation starts
# from zero. When we take the exponential of the log price levels, all paths
# will start from one.
log_level_mc <- rbind(matrix(0, nrow=1, ncol=n), log_level_mc)