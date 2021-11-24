
library(tidyverse)

# Observations distribution:
O_dist <- function(n) {
    sample(x = c(1,2,3,4), n, replace = T, prob=c(1/4, 0.01/4, 1.99/4, 1/4))
}

## Observed data:
n_obs <- 100
Obs <- O_dist(n_obs)


# Models:

unif_prior <- 1/2
base_rates_prior <- 1/2

Obs_pos <- tibble(Obs) %>%
    distinct(Obs) %>%
    pull(Obs) %>%
    sort()

Obs_freq <- tibble(Obs) %>%
    group_by(Obs) %>%
    summarise(n = n()) %>%
    mutate(freq = n / n_obs) %>% 
    pull(freq)

Obs_hat <- rep(0, 4)
Obs_hat[Obs_pos] <- Obs_freq

unif_lik <- 1 - sum(abs(Obs_hat - rep(1/4, 4)))

base_rates <- c(1/4, 0, 2/4, 1/4)
base_rates_lik <- 1 - sum(abs(Obs_hat - base_rates))

post_unif <- unif_lik * unif_prior
post_base_rates <- base_rates_lik * base_rates_prior

unif_prior * log(post_unif/unif_prior, base=2)
base_rates_prior * log(post_base_rates/base_rates_prior, base=2)



