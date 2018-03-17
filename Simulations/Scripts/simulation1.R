
# Library -----------------------------------------------------------------

packages <- c("dplyr", "ggplot2", "tidyr")

lapply(packages, library, character.only = TRUE)


# plotd -------------------------------------------------------------------

plotd <- function(x) {
  stopifnot(is.numeric(x), length(x) >1)
  
  plot(density(x))
}

# get_sim -----------------------------------------------------------------


get_sim <- function(n, x, y, dist, plot = F, seed = 1234, ...) {
  dist <- match.fun(dist)
  set.seed(seed)
  vals <- dist(n, x, y, ...)
  
  if (plot == TRUE) plotd(vals)
  vals
  }

# get_boot ----------------------------------------------------------------


get_boot <- function(n, x, y, dist, plot = F, seed = 1234, ...) {
  dist <- match.fun(dist)
  
  if (n > 100) n <- round (n / 100)
  set.seed(seed)
  vals <- dist(n, x, y, ...)
  set.seed(seed)
  boot <- vals[sample(1:length(vals), size = n, replace = TRUE)]
  
  
  if (plot == TRUE) plotd(boot)
  boot
}

# get_simdf ---------------------------------------------------------------


get_simdf <- function(n, x, y, dist, plot = F, ...) {
  
  simulated <- data.frame(
    simulated = get_sim(n, x, y, dist, plot),
    booted = get_boot(n, x, y, dist, plot)
  )
  
  if (plot == T) {
    plot(density(simulated[["booted"]]))
    plot(density(simulated[["simulated"]]))
  }
  
  simulated
}

# get_sim_all -------------------------------------------------------------

get_sim_all <- function(num_obs, x, y, dist = "rnorm", ...) {
  stopifnot(is.numeric(num_obs), length(num_obs) > 0) 
  if (!require(tibble)) library(tibble)
  
  sim_all <- expand.grid(n = num_obs, x = x, y = y)
  sim_all$data <- vector("list", nrow(sim_all)) 
  sim_all$data <- lapply(sim_all[[1]], get_simdf, x = sim_all$x,
                         y = sim_all$y, dist = dist)
  as_tibble(sim_all)
  }
  
  
# store simulation results -----------------------------------------------
# store n values to iterate over
num_obs <- c(10, 100, 1000, 10000, 100000, 1000000)

# store distribution values
distrib <- "rnorm"

# create a data.frame to store the parameters
sim_all <- expand.grid(n = num_obs, x = x, y = y)

# create a nested data.frame
sim_all$data <- vector("list", nrow(sim_all))

# getting all the data into sim_all
sim_all$data <- lapply(sim_all[[1]], get_simdf, 
                       x = sim_all$x, y = sim_all$y, dist = "rnorm")

# sim_all to tibble
sim_all <- as_tibble(sim_all)

# testing for statistical significance across all simulations ------------

# creating a test function
# check if the distributions were drawn from the same sample
test_fun <- function(df) {
  ks.test(df$simulated, df$booted)
}

# checking for statistically significant differences
suppressWarnings(lapply(sim_all$data, test_fun))
