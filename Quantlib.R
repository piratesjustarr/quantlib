
# Installs basic BraveRock packages from git source alongside dependencies
installQuantPackages<-function()
{
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
  .blotter<-new.env()
  .strategy<-new.env()
  currency("GBP")
  Sys.setenv(TZ="GMT")
}

#Returns list of known AIM tickers from AimIndex.csv
#Full CSV returned from function call if needed
# 2 Variants of symbols list (either as LON:XXX or XXX.L) returned to parent.env
loadAIMSymbols<-function()
{
  setwd("./")
  if(file.exists("AimIndex.csv"))
  {
  AimIndex <- read_csv("./AimIndex.csv")
  LonSymbols<<-AimIndex$Symbol
  LSymbols<<-AimIndex$Symbol2
  return(AimIndex)
  }
  else
  {
    print("AimIndex csv file not found! Check working directory.")
  }
}

getRemoteDataG<-function(startDate)
{
  #Make new environment and kick it up to parent
  LoadedSymbols<<-new.env()
  #Load data from Google
  getSymbols(as.character(LonSymbols), src='google', index.class=c("POSIXt","POSIXct"), env=LoadedSymbols, from=startDate)
}


getRemoteDataY<-function(startDate)
{
  #Make new environment and kick it up to parent
  LoadedSymbols<<-new.env()
  #Load data from Google
  getSymbols(as.character(LonSymbols), src='yahoo', index.class=c("POSIXt","POSIXct"), env=LoadedSymbols, from=startDate)
}

saveLocalData<-function()
{
  if(exists("LoadedSymbols"))
  {
  save(LoadedSymbols, file = "Symbols")
  }
  else
  {
    print("LoadedSymbols environment not found. Try loading some local or remote data first.")
  }
}

loadLocalData<-function()
{
  if(file.exists("Symbols"))
  {
    #Load previously acquired data from disk
    load(file = "Symbols")
    #Kick it up to parent environment
    LoadedSymbols<<-LoadedSymbols
  }
  else
  {
    print("'Symbols' file not found. Saving some data first.")
  }
}

sanityCheckSymbols<-function()
{
  if(exists("LoadedSymbols"))
  {
    #Make dataframe for output and fix column header
    SymbolCheck<-as.data.frame(ls(LoadedSymbols))
    colnames(SymbolCheck)="Symbol"
    
    #Setup Table
    SymbolCheck$HasVo<-FALSE
    SymbolCheck$BadVo<-FALSE
    SymbolCheck$HasOp<-FALSE
    SymbolCheck$HasCl<-FALSE
    for(symbol in ls(LoadedSymbols))
    {
      SymbolCheck[which(SymbolCheck$Symbol==symbol),]$HasVo=has.Vo(LoadedSymbols[[symbol]])
      SymbolCheck[which(SymbolCheck$Symbol==symbol),]$HasOp=has.Op(LoadedSymbols[[symbol]])
      SymbolCheck[which(SymbolCheck$Symbol==symbol),]$HasCl=has.Cl(LoadedSymbols[[symbol]])
      SymbolCheck[which(SymbolCheck$Symbol==symbol),]$BadVo=sum(Vo(LoadedSymbols[[symbol]]))==0
    }
    SymbolCheck<<-SymbolCheck
  }
  else
  {
    print("LoadedSymbols environment not found. Try loading some local or remote data first.")
  }
}

hasDateSymbols<-function()
{
  if(exists("LoadedSymbols"))
  {
    #Make dataframe for output and fix column header
    DateCheck<-as.data.frame(ls(LoadedSymbols))
    colnames(DateCheck)="Symbol"
    
    #Setup Table
    
    
    return(DateCheck)  
    }
  else
  {
    print("LoadedSymbols environment not found. Try loading some local or remote data first.")
  }
}
