
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

loadLibraries<-function()
{
  require(quantmod)
  require(quantstrat)
  library(readr)
  
  #Quantstrat boilerplate
  .blotter<-new.env()
  .strategy<-new.env()
}