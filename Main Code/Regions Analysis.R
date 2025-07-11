library(stringr)
library(dplyr)
library(purrr)

#Complete Regions
regions <-  readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/Region/trends_all_regions_2010_01_01_2025_05_01.RDS")

#Get unique region names
countries_dedup <- countries %>%
  filter(country_code=="UA") %>%
  group_by(sub_code) %>%
  arrange(desc(str_detect(name, "(?i)oblast"))) %>%  # Case-insensitive oblast detection
  slice_head(n = 1) %>%
  ungroup()

regions_fin <- map(regions, function(x) {
  # Add region name and clean data
  result <- x[[1]] %>%
    left_join(select(countries_dedup, sub_code, name), by = c("geo" = "sub_code"))
  
  result$hits <- ifelse(result$hits == "<1", 0.5, result$hits)
  result$hits <- as.numeric(ifelse(result$hits == "", 0, result$hits))
  
  return(result)
})

plots <- map(regions_processed, ~time_plot(wide_dat(.x), head(.x$name, 1)))
do.call(gridExtra::grid.arrange, c(plots[1:4], ncol = 2))
do.call(gridExtra::grid.arrange, c(plots[1:6], ncol = 3))
do.call(gridExtra::grid.arrange, c(plots[1:9], ncol = 3))


