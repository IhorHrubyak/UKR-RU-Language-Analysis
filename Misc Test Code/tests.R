library(gtrendsR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Define search terms in both languages
# Example: Common everyday terms or news-related terms
ukr_terms <- c("новини", "війна", "фільм") 
rus_terms <- c("новости", "война", "фильм") 

# Function to collect Google Trends data for Ukraine
get_language_trends <- function(term_ukr, term_rus, timeframe = "2010-01-01 2025-01-01") {
  # Compare the same term in both languages
  comparison <- gtrends(
    keyword = c(term_ukr, term_rus),
    geo = "UA", # Ukraine geo code
    time = timeframe,
    onlyInterest = TRUE
  )
  
  # Return the interest over time data
  return(comparison$interest_over_time)
}

# Add event markers for significant historical events
events <- data.frame(
  date = as.Date(c("2014-02-20", "2014-03-18", "2022-02-24")),
  event = c("Maidan Revolution", "Crimea Annexation", "Russian Invasion")
)

# Visualize trends over time
ggplot(all_results, aes(x = date, y = hits, color = keyword)) +
  geom_line(size = 1) +
  facet_wrap(~keyword, scales = "free_y") +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", alpha = 0.7) +
  geom_text(data = events, aes(x = date, y = 5, label = event), 
            angle = 90, vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(
    title = "Ukrainian vs. Russian Language Search Terms in Ukraine",
    subtitle = "Google Trends 2014-2025",
    x = "Date",
    y = "Search Interest",
    color = "Language"
  )

myplot <-  ggplot(all_results,aes(x=date, y=hits,color = keyword)) + 
  geom_line(size = 1) +
  facet_wrap(~keyword, scales = "free_y") +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", alpha = 0.7) +
  theme(legend.position="bottom") +
  labs(title ="UA v RU", x = "Date", y="Hits") +
  theme_minimal() +
  labs(
    title = "Ukrainian vs. Russian Language Search Terms in Ukraine",
    subtitle = "Google Trends 2014-2025",
    x = "Date",
    y = "Search Interest",
    color = "Language"
  )
print(myplot)
dev.off()

regional_comparison <- gtrends(
  keyword = c("новини", "новости"), # News in Ukrainian vs Russian
  geo = "UA",
  resolution = "REGION"
)