library(tidyverse)
library(mapproj)

data_us <- read_csv('data_us.csv') %>%
    pivot_longer(cols = -State, names_to = 'year', values_to='unemployment') %>%
    janitor::clean_names() %>%
    mutate(region = tolower(state), unemployment = unemployment / 100) %>%
    select(-state)

states <- map_data("state")


names(data_us)
names(states)

unemployment_map <- data_us %>% 
    group_by(region) %>%
    summarise(unemployment = mean(unemployment)) %>%
    inner_join(states)

# unemployment map:

# national avegrage:

data_us %>%
    summarise(mean(unemployment))

unemployment_map %>%
    ggplot(aes(long, lat, group = group, fill = unemployment)) +
    geom_polygon(color = 'grey70') + 
    scale_fill_gradient2(low = 'blue', high = 'red', midpoint = 0.0593) +
    theme_void()

