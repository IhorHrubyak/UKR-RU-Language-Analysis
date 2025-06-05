#Run queeires to get data, could maybe have loop of terms 
#date for queries if not run 
date <- "2010-01-01 2025-05-01"
# trends_dat(what,"what","що таке","что такое","UA",date,what_t,what_r,what_c)
# trends_dat(how,"how","Як","Как","UA",date,what_t,what_r,what_c)
# trends_dat(news,"news","новини","новости","UA",date,date,region,city)
# trends_dat(games,"games","ігри","игры","UA",date,date,region,city)
# trends_dat(recipes,"recipes","рецепти","рецепты","UA",date,date,region,city)
# trends_dat(price,"price","ціна","цена","UA",date,date,region,city)
# trends_dat(why,"price","чому","почему","UA",date,date,region,city)
# trends_dat(where,"price","чому","почему","UA",date,date,region,city)
# trends_dat(where,"where","де","где","UA",date,date,region,city)
# trends_dat(translate,"translate","переклад","перевод","UA",date,date,region,city)
# trends_dat(money,"money","гроші","деньги","UA",date,date,region,city)
# trends_dat(war,"war","війна","война","UA",date,date,region,city)

#Perhaphs ideal to have
# trends_dat(composite,"composite","що таке+новини+ігри+рецепти+Як+ціна+чому+гроші","что такое+Как+новости+игры+рецепты+цена+почему+деньги","UA",date,date,region,city)

#If data saved and want to load
how <-  time_plot(wide_dat(process_g_trends(readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/how2010_01_01_2025_05_01.RDS"))[[1]]), "How: Як/как")
News <-  time_plot(wide_dat(process_g_trends(readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/news2010_01_01_2025_05_01.rds"))[[1]]), "News: новини/новости")
what <-  time_plot(wide_dat(process_g_trends(readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/what2010_01_01_2025_05_01.rds"))[[1]]), "What is: Що таке/Что такое")
price <-  time_plot(wide_dat(process_g_trends(readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/recipes2010_01_01_2025_05_01.RDS"))[[1]]), "Price: Ціна/Цена")
games <-  time_plot(wide_dat(process_g_trends(readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/games2010_01_01_2025_05_01.RDS"))[[1]]), "Games: Ігри/Игри")
recipes <-  time_plot(wide_dat(process_g_trends(readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/recipes2010_01_01_2025_05_01.RDS"))[[1]]), "Recipes: Рецепти/Рецепты")


#Plot over date
grid.arrange(how,News,what,price,games,recipes, nrow = 2,
             top="Graphs of Ukrainian/Russian Ratios over date")


#Ideas have graph over time and map by oblast of ratio. Some sort of time series analysis to test if there are breaks in data. For regional analysis over time, could manually download
how_region <- readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/how2010_01_01_2025_05_01.RDS")[[3]]

#Table Ranking By Region
reg_ru <- how_region  %>%
  filter(keyword=="Как") %>%
  arrange(location) %>% select(hits) 

reg_ua <- corona_region  %>%
  filter(keyword=="Як") %>%
  arrange(location) %>% select(location,hits) 

#Creating separte columns for two diff search terms
reg_ua <- rename(reg_ua,hits_ua=hits)
reg_ru <- rename(reg_ru,hits_ru=hits)
regions_tab <- cbind(reg_ua,reg_ru)
#Setting Russian to numeric and sorting by it. Could be better to look at rus realtive to ukr as ratio?
regions_tab$hits_ua <-  ifelse(regions_tab$hits_ua == "<1",0,regions_tab$hits_ua)
regions_tab$hits_ua <-  as.numeric(ifelse(regions_tab$hits_ua == "",0,regions_tab$hits_ua))
regions_tab$hits_ru <- as.numeric(regions_tab$hits_ru)

regions_tab$rat <- regions_tab$hits_ua / regions_tab$hits_ru

regions_tab <-  arrange(regions_tab, -regions_tab$rat)
names(regions_tab) <- c("Oblast", "Hits UA", "Hits RU", "Ratio UA/RU")
stargazer(regions_tab,summary=FALSE,
          title= "Ukrainian Oblasts and Cities Ranked by Ukrainian/Russian Ratio", notes = "Crimea and Sevastapol had insignificant Ukrainian results",header =FALSE)



regions_tab %>%
  gt() %>%
  tab_header(
    title = "Ukrainian Oblasts and Cities Ranked by Ukrainian-to-Russian Search Ratio",
    subtitle = "Search term: 'how' ('Як' vs 'Как') from Google Trends"
  ) %>%
  fmt_number(columns = c(`Hits (UA)`, `Hits (RU)`, `UA/RU Ratio`), decimals = 2) %>%
  tab_footnote(
    footnote = "Crimea and Sevastopol may have suppressed or missing data.",
    locations = cells_title(groups = "title")
  ) %>%
  cols_label(
    `Hits (UA)` = "Hits (UA)",
    `Hits (RU)` = "Hits (RU)",
    `UA/RU Ratio` = "UA/RU Ratio"
  ) %>%
  opt_table_outline()