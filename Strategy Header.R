#Setup Code
source("Quantlib.R")
loadLibraries()
loadLocalData()
MySymbols<-LoadedSymbols
symbols<-ls(MySymbols)
library(chron)
suppressMessages(library(quantmod))
