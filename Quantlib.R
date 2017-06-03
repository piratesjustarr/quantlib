
# Installs basic BraveRock packages from git source alongside dependencies
installQuantPackages<-function()
{
  #install.packages("roxygen2")
  install.packages("devtools")
  require(devtools)
  install_github("braverock/FinancialInstrument")
  install_github("joshuaulrich/xts")
  install_github("braverock/blotter")
  install_github("braverock/quantstrat")
  install_github("braverock/PerformanceAnalytics")
}

#Loads basic libraries and sets up required environments for Quantstrat
loadLibraries<-function()
{
  require(quantmod)
  require(quantstrat)
  library(readr)
  
  #Quantstrat boilerplate
  
  #TODO - review estra boilerplate from Harry Georgakopolous' code
  .blotter<-new.env()
  .strategy<-new.env()
}

#Returns list of known AIM tickers from AimIndex.csv
#Full CSV returned from function call if needed
# 2 Variants of symbols list (either as LON:XXX or XXX.L) returned to parent.env
loadAIMSymbols<-function()
{
  setwd("./")
  AimIndex <- read_csv("./AimIndex.csv")
  LonSymbols<-AimIndex$Symbol
  LSymbols<-AimIndex$Symbol2
  return(AimIndex)
}


