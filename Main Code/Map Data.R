library(sf)
library(dplyr)
library(tmap)

how_region <- readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/how2010_01_01_2025_05_01.RDS")[[3]]

#Table Ranking By Region
reg_ru <- how_region  %>%
  filter(keyword=="Как") %>%
  arrange(location) %>% select(hits) 

reg_ua <- how_region  %>%
  filter(keyword=="Як") %>%
  arrange(location) %>% select(location,hits) 

#Creating separte columns for two diff search terms
reg_ua <- rename(reg_ua,hits_ua=hits)
reg_ru <- rename(reg_ru,hits_ru=hits)
regions_tab <- cbind(reg_ua,reg_ru)

ukraine_sf <- st_read("C:/UKR-RU-Language-Analysis/Shapefiles/gadm41_UKR_1.shp")

#standardize names
nam <- read.csv("C:/UKR-RU-Language-Analysis/Shapefiles/name_lookup.csv")

#Make sure names match
regions_tab <-cbind(regions_tab,shapefile_name= nam$shapefile_name)

map_data <- ukraine_sf %>%
  left_join(regions_tab, by = c("NAME_1" = "shapefile_name"))

#Kiev city fails join for some reason. Manually correcting and renmaing
map_data$rat[13] <- regions_tab$rat[12]
map_data$NAME_1[13] <- "Kyiv City"

breaks <- c(0, .1,.25, 0.5, 1, 2,4,10, Inf)
labels <- c("<0.25", "0.25–0.5", "0.5–1", "1–2", ">2")

tmap_mode("plot")
tm_shape(map_data) +
  tm_polygons("rat", palette = "RdYlGn", style = "fixed",
              breaks = breaks,
              palette = "RdYlGn",
              title = "UA/RU Language Ratio",orientation = "landscape") +
  tm_layout(title = "Ukrainian-to-Russian Search Ratio by Oblast",
            legend.outside = TRUE)

tm_shape(map_data) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_polygons("rat", palette = "RdYlGn", style = "fixed",
              breaks = breaks,
              palette = "RdYlGn",
              title = "UA/RU Language Ratio") +
  tm_layout(title = "Ukrainian-to-Russian Search Ratio by Oblast",
            title.position = c("center", "top"),
            legend.outside = TRUE,frame=FALSE )
tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"))

#interactive version
tmap_mode("view")  # Enables interactive mode
tm <-  tm_shape(map_data) +
  tm_polygons("rat", palette = "-RdYlBu", style = "cont",
              title = "UA/RU Language Ratio", 
              popup.vars = c("Oblast" = "NAME_1", "Ratio" = "rat")) +
  tm_layout(title = "Interactive Ukrainian-to-Russian Search Ratio Map")
tmap_save(tm, filename = "C:/UKR-RU-Language-Analysis/Maps/ua_ratio_map.html")