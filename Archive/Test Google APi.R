library(gtrendsAPI)

test <- getGraph(C("війна","Война"),geo = "UA" api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4")
test <- getGraph(C("apple"),geo = "UA" api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4")


head(getGraph("apple",geo = "UA", api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4"),1)
head(getGraph("Яблуко",geo = "UA", api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4"),1)
head(getGraph("Яблоко",geo = "UA", api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4"),1)

 getGraph(C("війна","Война"),geo = "UA", api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4")

gtrends(keywords, geo = geo, time = time
        
е <- getRegions("яблоко",geo="UA",property = "news", api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4")
        
t <-         getRegions("війна", geo = "US", property = "news",
                   category = "Food & Drink", startDate = "2014-01", endDate = "2014-12",
                   api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4")        

Sys.setlocale("LC_CTYPE", "ukrainian")

getRegions("Война", geo = "UA", property = "news", startDate = "2014-01", endDate = "2014-12",
           api.key = "AIzaSyC6ZybJxswErRSit9hA0zfpLYwxdOMgDE4")        
