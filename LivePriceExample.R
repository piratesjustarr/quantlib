##Example pulling live price down from Google Finance

library(curl)
library(jsonlite)
a<-curl("https://finance.google.com/finance/info?client=ig&q=LON:AMC")
readLines(a)
b

#To-Do Grep this shit out
b[2]=""
b[21]=""

quote<-fromJSON((b))

quote
