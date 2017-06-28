# Refresh Data
##############

#
# Standalone script intended to be run by Cron job to keep Google Finance data fresh.
#


#Loads basic libraries and sets up required environments for Quantstrat
loadLibraries<-function()
{
  require(slackr)
  require(quantmod)
  require(quantstrat)
  require(readr)
  require(chron)
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

#Hassle GOOGLE for the OHLCV data for the symbols
getRemoteDataG<-function(startDate)
{
  #Make new environment and kick it up to parent
  LoadedSymbols<<-new.env()
  #Load data from Google
  getSymbols(as.character(LonSymbols), src='google', index.class=c("POSIXt","POSIXct"), env=LoadedSymbols, from=startDate)
}

#Hassle Yahoo for the OHLCV data for the symbols
getRemoteDataY<-function(startDate)
{
  #Make new environment and kick it up to parent
  LoadedSymbols<<-new.env()
  #Load data from Google
  getSymbols(as.character(LonSymbols), src='yahoo', index.class=c("POSIXt","POSIXct"), env=LoadedSymbols, from=startDate)
}


#Save out the LoadedSymbols environment
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

# Wrapper for functions above. Loads Symbols and pulls data from Google. Save to disk.
RefreshData<-function()
{
  print("Loading Symbols...")
  loadAIMSymbols()
  print("Getting data from Google...")
  getRemoteDataG("2015-01-01")
  print("Saving Data to disk...")
  saveLocalData()
  print("DONE.")
}


#Reload all AIM data and tell Slack
setwd("/home/raffles/Raffles/Data")
loadLibraries()
slackr_setup()
slackrBot("Refreshing all Aim data:")
message<-"Something terrible has occurred!"

if(is.weekend(Sys.Date())==TRUE)
{
  message<-"Nope - it's the weekend."  
}

if(is.weekend(Sys.Date())==FALSE)
{
  timeTaken=system.time(RefreshData())
  message<-paste("Got updates for",length(ls(LoadedSymbols)),"symbols in ",timeTaken)
}
slackrBot(message)
