#Map tests

devtools::install_github('PMassicotte/gtrendsR')

install.packages("rnaturalearthhires")

library(rnaturalearthhires)
library(gtrendsR)
library(maps)
library(ggplot2)
library(lettercase)
library(viridis)
library(pals)
library(scico)
library(ggrepel)
library(tidyverse)
library(rnaturalearth)
library(ggplot2)

my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}


#Mabye a problem with news is that is fluctuates too much over time, better to have something smoother
plot(google.trends_news) + 
  my_theme() +
  geom_line(size = 0.5) 


InterestByRegion <- as_tibble(google.trends_news$interest_by_region)
InterestByRegion2 <- InterestByRegion %>% 
  dplyr::mutate(region = stringr::str_to_lower(location))

statesMap = ggplot2::map_data("state")

ukraine <- map_data("ukraine")