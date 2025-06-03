#do event study or interrupted time series analysis? 

#date for queries if not run 
date <- "2010-01-01 2025-05-01"
# trends_dat(what,"what","що таке","что такое","UA",date,what_t,what_r,what_c)
# trends_dat(how,"how","Як","Как","UA",date,what_t,what_r,what_c)
# trends_dat(news,"news","новини","новости","UA",date,time,region,city)
# trends_dat(games,"games","ігри","игры","UA",date,time,region,city)
# trends_dat(recipes,"recipes","рецепти","рецепты","UA",date,time,region,city)
# trends_dat(price,"price","ціна","цена","UA",date,time,region,city)
# trends_dat(why,"price","чому","почему","UA",date,time,region,city)

trends_dat(where,"where","де","где","UA",date,time,region,city)
trends_dat(translate,"translate","переклад","перевод","UA",date,time,region,city)
trends_dat(money,"money","гроші","деньги","UA",date,time,region,city)
trends_dat(war,"war","війна","война","UA",date,time,region,city)


trends_dat(composite,"composite","що таке+новини+ігри+рецепти+Як+ціна+чому+гроші","что такое+Как+новости+игры+рецепты+цена+почему+деньги","UA",date,time,region,city)


#If data saved and want to load
how <-  time_plot(wide_dat(process_g_trends(readRDS("C:/Language Analysis/Trend Queries/how2010_01_01_2025_05_01.RDS"))[[1]]), "How: Як/как")
News <-  time_plot(wide_dat(process_g_trends(readRDS("C:/Language Analysis/Trend Queries/news2010_01_01_2025_05_01.rds"))[[1]]), "News: новини/новости")
what <-  time_plot(wide_dat(process_g_trends(readRDS("C:/Language Analysis/Trend Queries/what2010_01_01_2025_05_01.rds"))[[1]]), "What is: Що таке/Что такое")
price <-  time_plot(wide_dat(process_g_trends(readRDS("C:/Language Analysis/Trend Queries/recipes2010_01_01_2025_05_01.RDS"))[[1]]), "Price: Ціна/Цена")
games <-  time_plot(wide_dat(process_g_trends(readRDS("C:/Language Analysis/Trend Queries/games2010_01_01_2025_05_01.RDS"))[[1]]), "Games: Ігри/Игри")
recipes <-  time_plot(wide_dat(process_g_trends(readRDS("C:/Language Analysis/Trend Queries/recipes2010_01_01_2025_05_01.RDS"))[[1]]), "Recipes: Рецепти/Рецепты")

#Plot over Time
grid.arrange(how,News,what,price,games,recipes, nrow = 2,
             top="Graphs of Ukrainian/Russian Ratios over Time")



#Plot for each of the regions if needed
g <- list(1)
for(i in 1:27) {
  g[[i]] <-  time_plot(y[[i]],y[[i]]$Oblast[1])
}

grid.arrange(g[[1]],g[[2]],g[[3]],g[[4]],g[[5]],g[[6]],g[[7]],g[[8]],g[[9]],g[[10]],g[[11]],g[[12]],g[[13]],
             g[[14]],g[[15]],g[[16]],g[[17]],g[[18]],g[[19]],g[[20]],g[[21]],g[[22]],g[[23]],g[[24]],g[[25]],
             g[[26]],g[[27]],
             top="Graphs of Ukrainian/Russian Ratios over Time",nrow = 9)

how <-  wide_dat(process_g_trends(readRDS("C:/Language Analysis/Trend Queries/how2010_01_01_2025_05_01.RDS"))[[1]])


how <-  process_g_trends(readRDS("C:/Language Analysis/Trend Queries/how2010_01_01_2025_05_01.RDS"))


