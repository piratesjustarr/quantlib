

# Variables go here

testStartDate = "2016-03-01"

# Functions

runStrategy <- function(theDate)
{
  loadLocalData()
  loadLibraries()
  library(chron)
  
  #createLedger()
  

  
    theDate<-as.Date(theDate)
  
  #Share Select
  
  
  #Make a list
  shift<-list()
  
  #Loop through known symbols
  for (symbol in ls(LoadedSymbols))
  {
    delta=30
    data<-Cl(LoadedSymbols[[symbol]])
    #Only use data we have access to.
    data<-data[paste("::",theDate,sep="")]
    
    #If we have enough data
    if(nrow(data)>delta)
    {
      #Get ROC vs delta periods ago
      val=median(ROC(data,n = delta),na.rm = TRUE)
    }
    else
    {
      #Else shove it to bottom of the pile
      val=0
    }
    shift[symbol]<-val
  }
  shift<<-shift
  #Transpose and sort
  res<-(t(as.data.frame(shift)))
  res1<<-res
  res<-res[order(res,decreasing = TRUE),]
  
  # ONe from the top please carol
  ChosenSymbol<<-names(res)[1]
  #Fix name after previous transforms
  ChosenSymbol<-gsub(ChosenSymbol,pattern = "\\.",replacement = ":")
  print(ChosenSymbol)
  
  #Get Entry
  BuyPrice<-as.numeric(Cl(LoadedSymbols[[ChosenSymbol]][theDate]))
  
  #Get Exit
  n=21
  lastDate<-as.Date(index(last(Cl(LoadedSymbols[[ChosenSymbol]]))))
  maxDate<-as.Date(theDate)+n
  
  #Don't try and go into the FUTURE!
  if(maxDate>lastDate){maxDate<-lastDate}
  
  HoldData<-Cl(LoadedSymbols[[ChosenSymbol]][paste(theDate,"::",maxDate,sep="")])
  print(HoldData)
  if(nrow(HoldData>0))
  {
    # First 5% loss close
    StopLoss<-head(which(HoldData/BuyPrice<0.8),1)
    StopGain<-head(which(HoldData/BuyPrice>1.1),1)
    print(StopLoss)
    StopLoss<<-StopLoss
    StopGain<<-StopGain
    
    SellDate<-maxDate
    Comment<-"Dropthrough"
    
    if(length(StopLoss)==0 && length(StopGain)==0)
    {
      SellDate<-maxDate
      Comment<-"Sell on hold"
    }
    
    if(length(StopLoss)==0 && length(StopGain)!=0)
    {
      SellDate<-index(HoldData[StopGain,])
      Comment<-"StopGain"
    }
    
    if(length(StopLoss)!=0 && length(StopGain)==0)
    {
      SellDate<-index(HoldData[StopLoss,])
      Comment<-"Stop Loss"
    }
    
    
    #while(is.weekend(SellDate)==TRUE){SellDate=SellDate+1}
    #while(is.holiday(SellDate)==TRUE){SellDate=SellDate+1}    
    
    # if(SellDate>as.Date(index(last(Cl(LoadedSymbols[[ChosenSymbol]])))))
    #   {
    #    SellDate<-as.Date(index(last(Cl(LoadedSymbols[[ChosenSymbol]]))))
    # }
    
    SellPrice<-as.numeric(Cl(LoadedSymbols[[ChosenSymbol]][SellDate]))
    
    LedgerData<-c(ChosenSymbol,as.character.Date(theDate),BuyPrice,as.character.Date(SellDate),SellPrice,Comment)
    return(LedgerData)
  }
  else
  {
    return(c(NA,NA,NA,NA,NA,NA))
  }
}



# Environment Setup

options(stringsAsFactors = FALSE)



#Let's get some stuff in here
source("Quantlib.R")

loadLibraries()
loadLocalData()

#Get date loop up and running
timeIndex <- as.Date(index(Cl(LoadedSymbols[[ls(LoadedSymbols)[1]]])))
dataStartDate <- first(timeIndex)

#Check testStart is (sufficiently) after dataStart
as.Date(testStartDate)-as.Date(dataStartDate)

startOffset <- which(timeIndex==as.Date(testStartDate))

runStrategy(testStartDate)

