library(stringr)
library(dplyr)
library(purrr)
library(ggalt)

#Complete Regions
regions <-  readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/Region/trends_all_regions_2010_01_01_2025_05_01.RDS")

#Get unique region names
countries_dedup <- countries %>%
  filter(country_code=="UA") %>%
  group_by(sub_code) %>%
  arrange(desc(str_detect(name, "(?i)oblast"))) %>%  # 
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


plots <- map(regions_fin, ~time_plot(wide_dat(.x), head(.x$name, 1)))
saveRDS(plots,"Figures/plots.rds")

p1 <- do.call(gridExtra::grid.arrange, c(plots[1:6], ncol = 3))
ggsave("figures/p1.png", p1, width = 15, height = 10, dpi = 300)
p2 <- do.call(gridExtra::grid.arrange, c(plots[7:12], ncol = 3))
ggsave("figures/p2.png", p2, width = 15, height = 10, dpi = 300)
p3 <- do.call(gridExtra::grid.arrange, c(plots[13:18], ncol = 3))
ggsave("figures/p3.png", p3, width = 15, height = 10, dpi = 300)

#Save Plots

#For interactive plots. have to improve
library(plotly)
# Convert to interactive plots
interactive_plots <- map(plots, ggplotly)
# Display in a more compact way
subplot(interactive_plots[1], nrows = 1, shareX = TRUE, shareY = TRUE)

#Check which regions changed most over time. Could try simple ratio or something else?
# Assuming `data` has columns: 'region', 'date', 'ratio'

over_time_ratio <- numeric(length(regions_fin))  # pre-allocate

for (i in seq_along(regions_fin)) {
  ratios <- wide_dat(regions_fin[[i]])$ratio
  over_time_ratio[i] <- mean(head(ratios, 12), na.rm = TRUE) / 
    mean(tail(ratios, 12), na.rm = TRUE)
}

mean(head(wide_dat(regions_fin[[1]])$ratio,10))

# Apply wide_dat first and then compute early vs. recent ratio changes
region_trends <- map_dfr(regions_fin, function(df_raw) {
  df <- wide_dat(df_raw)  
  name <- unique(df$name)
  
  ratios <- df$ratio
  if (length(ratios) < 12) return(NULL)  # skip if too few observations
  
  early_avg <- mean(head(ratios, 12), na.rm = TRUE)
  recent_avg <- mean(tail(ratios, 12), na.rm = TRUE)
  
  tibble(
    region = name,
    early_avg = early_avg,
    recent_avg = recent_avg,
    ratio_change = recent_avg / early_avg,
    log_change = log(recent_avg) - log(early_avg),
    abs_change = recent_avg - early_avg,
    pct_change = 100 * (recent_avg - early_avg) / early_avg
  )
})

bar_reg <- ggplot(region_trends, aes(x = reorder(region, log_change), y = log_change, fill = log_change)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_fill_gradient2(low = "firebrick", mid = "gray90", high = "darkgreen", midpoint = 0) +
  coord_flip() +
  labs(
    title = "Change in Ukrainian-to-Russian Search Ratio by Region",
    subtitle = "Log-scaled change from first 12 to last 12 months",
    x = NULL,
    y = "Log Ratio Change (Recent / Early)"
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10)
  )
ggsave("figures/regions_bar.png", bar_reg,width = 12, height = 7, dpi = 300)

region_ranks <- region_trends %>%
  mutate(start_rank = rank(-early_avg),  # rank descending (higher = better)
         end_rank = rank(-recent_avg)) %>%
  select(region, start_rank, end_rank) %>%
  pivot_longer(cols = c(start_rank, end_rank), names_to = "time", values_to = "rank") %>%
  mutate(time = ifelse(time == "start_rank", "2010", "2025"))

rank <- ggplot(region_ranks, aes(x = time, y = rank, group = region)) +
  geom_line(color = "gray70") +
  geom_point(size = 3, aes(color = time)) +
  geom_text(data = region_ranks %>% filter(time == "2010"),
            aes(label = region), hjust = 1.1, size = 3) +
  geom_text(data = region_ranks %>% filter(time == "2025"),
            aes(label = region), hjust = -0.1, size = 3) +
  scale_y_reverse(breaks = 1:max(region_ranks$rank)) +  # Rank 1 at top
  scale_x_discrete(position = "top") +
  labs(title = "Change in Regional Rankings of Ukrainian/Russian Language Ratio",
       subtitle = "Ranked by Ukrainian-to-Russian search ratio (2010 vs 2025)",
       x = NULL, y = "Rank (Lower = More Ukrainian)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("figures/regions_rank.png", rank,width = 12, height = 7, dpi = 300)