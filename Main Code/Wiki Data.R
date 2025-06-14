library(dplyr)
library(ggplot2)

folder_path <- "C:/UKR-RU-Language-Analysis/Wikimedia"
all_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Filter files by prefix
ru_files <- all_files[grepl("RU.*\\.csv$", basename(all_files))]
ukr_files <- all_files[grepl("UKR.*\\.csv$", basename(all_files))]

ru <- do.call(rbind, lapply(ru_files, read.csv, stringsAsFactors = FALSE))
ukr <- do.call(rbind, lapply(ukr_files, read.csv, stringsAsFactors = FALSE))

#Could also be interesting to look at Russo sphere over time?
ru2 <- ru %>%  filter(country == "UA") %>% select( month,views_ru=total.total) 
ukr2 <- ukr %>%  filter(country == "UA") %>% select( month,views_ua=total.total) 

full_dat <- inner_join(ru2,ukr2,by = "month")
full_dat$ukr_ru_ratio <- full_dat$views_ua/full_dat$views_ru
full_dat$date <-  as.Date(full_dat$month)

ggplot(full_dat, aes(x = date, y = ukr_ru_ratio)) + 
  geom_line(color = "#1E88E5", size = 1) +
  geom_point(color = "#1E88E5", size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "#FFC107", alpha = 0.2, linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title = "Ukrainian vs. Russian Wikipedia Usage in Ukraine",
    subtitle = "Ratio of Ukrainian to Russian Wikipedia page views over time",
    x = "Date",
    y = "Ratio (Ukrainian/Russian views)",
    caption = "Data source: Wikimedia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA),
    legend.position = "bottom"
  )


#Directly looking at page views over time could do more
library(pageviews)
library(tidyverse)

# Could try looking at ratios of different articles over time.
uk_wp <- article_pageviews(project = "uk.wikipedia", article = "Київ", 
                           start = as.Date("2017-01-01"), end = as.Date("2024-12-31"),granularity = "daily")

ru_wp <- article_pageviews(project = "ru.wikipedia", article = "Киев", 
                           start = as.Date("2017-01-01"), end = as.Date("2024-12-31"),granularity = "daily")

ru_wp <- arrange(ru_wp, date)
uk_wp <- arrange(uk_wp, date)


