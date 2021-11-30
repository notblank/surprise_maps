
library(tidyverse)

# Observations distribution:
O_dist <- function(n) {
    sample(x = c(1,2,3,4), n, replace = T, prob=c(1/4, 0.01/4, 1.99/4, 1/4))
}

## Observed data:
n_obs <- 100
Obs <- O_dist(n_obs)


# Models:

u_prior <- 1/3
br_prior <- 1/3
br_acc_prior <- 1/3

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


# Global Surprise/Update:

u_lik <- 1 - 0.5*sum(abs(Obs_hat - rep(1/4, 4)))
br <- c(1/4, 1.7/4, 0.3/4, 1/4)
br_lik <- 1 - 0.5*sum(abs(Obs_hat - br))
# accurate base rates
br_acc <- c(1/4, 0.1/4, 1.9/4, 1/4)
br_acc_lik <- 1 - 0.5*sum(abs(Obs_hat - br_acc))

post_u <- u_lik * u_prior
post_br <- br_lik * br_prior
post_br_acc <- br_acc_lik * br_acc_prior

u_prior * log(post_u/u_prior, base=2)
br_prior * log(post_br/br_prior, base=2)
br_acc_prior * log(post_br_acc/br_acc_prior, base=2)

#Local Surprise:
llik_u <- 1 - 0.5*abs(obs_hat - rep(1/4, 4))
sl_u <- log(llik_u, base = 2)

llik_br <- 1 - 0.5*abs(Obs_hat - br)
sl_br <- log(llik_br, base = 2)

llik_br_acc <- 1 - 0.5*abs(Obs_hat - br_acc)
sl_br_acc <- log(llik_br_acc, base = 2)

# surpise on states 2 and 3. Larger for base rate model.
tibble(state = 1:4, sl_u, sl_br, sl_br_acc) %>%
    pivot_longer(cols = contains('sl'), names_to = 'model', values_to = 'surprise') %>%
    ggplot(aes(state, surprise)) +
    geom_col() +
    facet_wrap(~model)
