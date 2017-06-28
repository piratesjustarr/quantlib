source("Quantlib.R")
loadLocalData()
loadLibraries()
loadLocalData()

RunDate="2017-04-01"
FutureDate="2017-05-10"

#Parallelising code
library(foreach)
library(doParallel)
        
# Calculate the number of core
no_cores <- detectCores() - 1

# Initiate cluster
cl<-makeCluster(no_cores)
registerDoParallel(cl)


stripData<-function(FakeToday)
{
  StrippedSymbols<<-new.env()
  
  #COPY Symbols for ROC analysis
  for(n in ls(LoadedSymbols, all.names=TRUE)) assign(n, get(n, LoadedSymbols), StrippedSymbols)
  
  #Remove data past start point to avoid lookahead bias
  for (symbol in ls(StrippedSymbols))
  {
    print(paste("Stripping future data from",symbol))
    StrippedSymbols[[symbol]]<-StrippedSymbols[[symbol]][paste("::",FakeToday,sep="")]
    #print(last(StrippedSymbols[[symbol]]))
  }
}


medianClose<-function(data,days)
{
  library(quantmod)
  closingPrice<-Cl(data)
  closingPrice<-last(closingPrice,paste(days,"days",sep=" "))
  return(median(closingPrice))
}

changeClose<-function(data,days)
{
  library(quantmod)
  library(xts)
  closingPrice<-Cl(data)
  last(closingPrice)/last(lag(closingPrice,days))
}

lastClose<-function(data)
{
  require(xts)
  require(quantmod)
  as.numeric(last(Cl(LoadedSymbols[[symbol]])))
}

#Strip data to valid date range
stripData(RunDate)

#Generate up to date symbol list
filteredSymbols<-c()
for(sym in ls(StrippedSymbols))
{
    if(nrow(StrippedSymbols[[sym]])<30)
    {
      print(paste("skipping ",sym,": <30 days data"))
    }
    else
    {
    filteredSymbols<-c(filteredSymbols,sym)
    }
}
symbolList<-as.data.frame(filteredSymbols,stringsAsFactors=FALSE)

#Populate some stats

#Get Last Close Price
symbolList$currentPrice<-foreach(symbol = filteredSymbols, .combine = rbind)  %dopar% lastClose(StrippedSymbols[[symbol]])

#Get median prices

# Last 5 days
symbolList$FiveDayMedianClose<-foreach(symbol = filteredSymbols, .combine = rbind)  %dopar% medianClose(StrippedSymbols[[symbol]],5)


# Last 15 days
symbolList$FifteenDayMedianClose<-foreach(symbol = filteredSymbols, .combine = rbind)  %dopar% medianClose(StrippedSymbols[[symbol]],15)


# Last 25 days
symbolList$TwentyFiveDayMedianClose<-foreach(symbol = filteredSymbols, .combine = rbind)  %dopar% medianClose(StrippedSymbols[[symbol]],25)

#Get ROC for

# Last 5 Days
symbolList$FiveDayChangeClose<-foreach(symbol = filteredSymbols, .combine = rbind)  %dopar% changeClose(StrippedSymbols[[symbol]],5)


# Last 15 Days
symbolList$FifteenDayChangeClose<-foreach(symbol = filteredSymbols, .combine = rbind)  %dopar% changeClose(StrippedSymbols[[symbol]],15)


# Last 25 Days
symbolList$TwentyFiveDayChangeClose<-foreach(symbol = filteredSymbols, .combine = rbind)  %dopar% changeClose(StrippedSymbols[[symbol]],25)


stopImplicitCluster()

PCA<-prcomp(symbolList[,-1],center = TRUE,scale. = TRUE)

Results<-cbind(symbolList,PCA$x)

loadLocalData()
FuturePrice=c()
for(sym in filteredSymbols)
{
  Price<-as.numeric(Cl(LoadedSymbols[[sym]][FutureDate]))
  if(length(Price)==0)
  { 
    print(paste("Future price for",sym,"is invalid",Price))
    Price=-1
  }
  FuturePrice<-c(FuturePrice,Price)
}

Results$FuturePrice<-FuturePrice
Results$Gain=Results$FuturePrice/Results$currentPrice

library(plotly)
plot_ly(x=Results$PC1,y=Results$PC2,z=Results$PC3,size=Results$Gain, hoverinfo="Results$filteredSymbols+x+y+z")

