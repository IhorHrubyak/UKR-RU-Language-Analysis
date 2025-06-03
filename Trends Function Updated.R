library(gtrendsR)
library(reshape2)
library(dplyr)
library(stargazer)
library(tidyr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(scales)

#Set Language to Ukrainian. !
Sys.setlocale("LC_CTYPE", "ukrainian")


trends_dat <- function(data,name, s1, s2, loc = "UA",  date,
                       time, region, city, retry_count = 100, retry_delay = 500) {
  
  # Function to fetch Google Trends data with retries
  fetch_with_retry <- function(keywords, geo, time, retries, delay) {
    for (i in 1:retries) {
      tryCatch({
        return(gtrends(keywords, geo = geo, time = time))
      }, error = function(e) {
        message(sprintf("Attempt %d failed: %s", i, e$message))
        if (i < retries) {
          message(sprintf("Retrying in %d seconds...", delay))
          Sys.sleep(delay)
        } else {
          stop("All retry attempts failed.")
        }
      })
    }
  }
  
  # Run gtrends with retry logic
  dat <<- fetch_with_retry(c(s1, s2), geo = loc, time = time, retries = retry_count, delay = retry_delay)
  
  time_label <- gsub("[^0-9a-zA-Z]+", "_",time)
  file_name <- paste0(name, time_label, ".RDS")
  saveRDS(dat, file = file.path("C:/Language Analysis/Trend Queries",file_name))
  }

# saveRDS(dat, "C:/Language Analysis/why2010-01-01 2025-05-01")

#Function to process data
process_g_trends <-  function(dat){    
  # Process time-level data
  t <- dat[[1]]
  t$hits <- ifelse(t$hits == "<1", 0.5, t$hits)
  t$hits <- as.numeric(ifelse(t$hits == "", 0, t$hits))
  
  # Region-level
  r <- dat[[3]]
  r$hits <- ifelse(r$hits == "<1", 0.5, r$hits)
  r$hits <- as.numeric(ifelse(r$hits == "", 0, r$hits))
  
  # City-level
  c <- dat[[5]]
  c$hits <- ifelse(c$hits == "<1", 0.5, c$hits)
  c$hits <- as.numeric(ifelse(c$hits == "", 0, c$hits))
  
  # Assign output to global environment with original variable names
  # assign(deparse(substitute(data)), dat, envir = .GlobalEnv)
  # assign(deparse(substitute(time)), t, envir = .GlobalEnv)
  # assign(deparse(substitute(region)), r, envir = .GlobalEnv)
  # assign(deparse(substitute(city)), c, envir = .GlobalEnv)
  
  #Assign to list alternative
  return(list(time = t, region = r, city = c))
  
}

#If needed function to make data wide and reassigning variable names, creating ratio . Make sure Ukrainian first search term
wide_dat <- function(datz){
 
#To preserve column order
  cols <- c(colnames(datz)[!(colnames(datz) %in% c("keyword","hits"))], unique(datz$keyword))
  ra <- spread(datz,keyword, hits)
  
# Select the columns in the order they are listed in the cols vector
ra <- ra[,cols]
 n <- ncol(ra)
 names(ra)[(n-1):n] <- c("hits_ua","hits_ru")
 ra$ratio <- ra$hits_ua / ra$hits_ru
 
 return(ra)
}

time_plot <- function(dat,title) {
  
  events <- data.frame(
    date = as.POSIXct(c("2010-02-17","2014-02-01", "2019-04-21", "2022-02-24")),
    label = c("Yanukovych Elected","Maidan", "Zelenskyy Elected", "Full-Scale Invasion")
  )
  
  # Create plot
  ggplot(dat, aes(x = date, y = ratio)) +
    geom_line(color = "#1f78b4", size = 1) +
    geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "gray50") +
    geom_text(
      data = events,
      aes(x = date, y = max(dat$ratio, na.rm = TRUE) * 0.75, label = label),
      angle = 90, vjust = -0.5, hjust = 0, size = 3.2, color = "gray30"
    ) +
    labs(
      title = title,
      x = "Date",
      y = "Ukrainian : Russian Search Ratio"
    ) +
    scale_x_datetime(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
  geom_smooth(method = "loess", se = TRUE,color="red4")
}

#To get data for each of regions
#unique geo codes
geo.codes = sort(unique(countries[substr(countries$sub_code, 1, 2) == "UA", ]$sub_code))
ob <- unique(countries[substr(countries$sub_code, 1, 2) == "UA",])

#Check composite for each region
z <- list(1)

for(i in 1:27) {
  
  z[[i]] <-   gtrends(c("що таке+новини+ігри+рецепти+Як+ціна","что такое+Как+новости+игры+рецепты+цена"), gprop = "web", time = date,geo = geo.codes[i])[[1]]
}  
  z[[i]]$hits <- ifelse(z[[i]]$hits == "<1",.5,z[[i]]$hits)
  z[[i]]$hits <- as.numeric(ifelse(z[[i]]$hits == "",0,z[[i]]$hits))
  
  cols <- c(colnames(z[[i]])[!(colnames(z[[i]]) %in% c("keyword","hits"))], unique(z[[i]]$keyword))
  y[[i]] <- spread(z[[i]],keyword, hits)
  # Select the columns in the order they are listed in the cols vector
  y[[i]] <- y[[i]][,cols]
  n <- ncol(y[[i]])
  names(y[[i]])[(n-1):n] <- c("hits_ua","hits_ru")
  y[[i]]$ratio <- y[[i]]$hits_ua / y[[i]]$hits_ru
  y[[i]]$Oblast <- filter(ob,sub_code == y[[i]]$geo[1])[1,3]
  
} 

trends_by_regions <- function(keywords_ukrainian, keywords_russian, 
                              date , gprop = "web", 
                              retry_count = 100, retry_delay = 300) {
  
  # Get unique Ukrainian region codes
  geo_codes <- sort(unique(countries[substr(countries$sub_code, 1, 2) == "UA", ]$sub_code))
  
  # Function to fetch Google Trends data with retries
  fetch_with_retry <- function(keywords, geo, time, retries, delay) {
    for (i in 1:retries) {
      tryCatch({
        return(gtrends(keywords, geo = geo, time = time, gprop = gprop))
      }, error = function(e) {
        message(sprintf("Attempt %d failed for region %s: %s", i, geo, e$message))
        if (i < retries) {
          message(sprintf("Retrying in %d seconds...", delay))
          Sys.sleep(delay)
        } else {
          message(sprintf("All retry attempts failed for region %s.", geo))
          return(NULL)  # Return NULL if all attempts fail
        }
      })
    }
  }
  
  # List to store results
  results <- list()
  
  # Iterate through each region
  for (i in seq_along(geo_codes)) {
    message(sprintf("Processing region %d of %d: %s", i, length(geo_codes), geo_codes[i]))
    
    # Fetch data with retry logic
    region_data <- fetch_with_retry(
      c(keywords_ukrainian, keywords_russian), 
      geo = geo_codes[i], 
      time = date, 
      retries = retry_count, 
      delay = retry_delay
    )
    
    # Store result if not NULL
    if (!is.null(region_data)) {
      results[[geo_codes[i]]] <- region_data
      
      # Save individual region result
      time_label <- gsub("[^0-9a-zA-Z]+", "_", date)
      file_name <- paste0("trends_", geo_codes[i], "_", time_label, ".RDS")
      saveRDS(region_data, file = file.path("C:/Language Analysis/Trend Queries", file_name))
      
      # Add a small delay between regions to avoid hitting rate limits
      if (i < length(geo_codes)) Sys.sleep(5)
    }
  }
  
  # Save complete results
  saveRDS(results, file = file.path("C:/Language Analysis/Trend Queries", 
                                    paste0("trends_all_regions_", gsub("[^0-9a-zA-Z]+", "_", date), ".RDS")))
  
  return(results)
}
date <- "2010-01-01 2025-05-01"
reg_result <- trends_by_regions(
  keywords_ukrainian = "що таке+новини+ігри+рецепти+Як+ціна+чому+гроші",
  keywords_russian = "что такое+Как+новости+игры+рецепты+цена+почему+деньги",date
)

trends_dat(composite,"composite","що таке+новини+ігри+рецепти+Як+ціна+чому+гроші","что такое+Как+новости+игры+рецепты+цена+почему+деньги","UA",date,time,region,city)

# Usage example:
# results <- trends_by_regions(
#   keywords_ukrainian = "що таке+новини+ігри+рецепти+Як+ціна",
#   keywords_russian = "что такое+Как+новости+игры+рецепты+цена"
# )