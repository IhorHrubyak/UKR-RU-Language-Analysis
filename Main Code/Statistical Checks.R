library(changepoint)
library(strucchange)

comp <-  wide_dat(process_g_trends(readRDS("C:/UKR-RU-Language-Analysis/Trend Queries/how2010_01_01_2025_05_01.RDS"))[[1]])

#Basic T test
comp$after_2022 <- comp$date >= as.POSIXct("2022-02-24")
# Compare Ukrainian/Russian ratio before and after Feb 24, 2022
t.test(ratio ~ after_2022, comp)


cpt <- cpt.mean(comp$ratio, method = "PELT")
plot(cpt)
cpts(cpt) 

model <- breakpoints(comp$ratio ~ comp$date)
summary(model)
plot(model)
lines(comp$date, fitted(model, breaks = 2), col = "red")


#Chow Test
comp$date_f <- as.Date(comp$date)

breakpoint_date <- which(comp$date_f ==  as.Date("2022-02-24") )
model_full <- lm(ratio ~ date_f, comp)
sctest(model_full, type = "Chow", point = breakpoint_date)