source("Quantlib.R")
library(chron)

#Run for saved data
loadLibraries()
loadLocalData()
MySymbols<-LoadedSymbols
symbols<-ls(MySymbols)


IdiotTrader<-function(TheDate)
{
  #Random Select a Symbol
  ChosenSymbol<-sample(symbols,1)
  BuyPrice<-as.numeric(Cl(MySymbols[[ChosenSymbol]][FakeToday]))
  n=21
  SellDate=as.Date(FakeToday)+sample(1:n,1)   
  #Random Hold for up to n days
  while(is.weekend(SellDate))
  {
    SellDate=as.Date(FakeToday)+sample(1:n,1)        
  }
  SellPrice<-as.numeric(Cl(MySymbols[[ChosenSymbol]][SellDate]))
  print(paste("Bought",ChosenSymbol,"at",BuyPrice,"and sold on",as.character(SellDate),"for",SellPrice))
  result<-c(ChosenSymbol,BuyPrice,SellDate,SellPrice)
  return(result) 
}

MakeTrades<-function()
{
  Trades<-as.data.frame(IdiotTrader())
  for(i in 1:4)
  {
    Trades<-cbind(Trades,IdiotTrader())
  }
  Trades<-t(Trades)
  Trades<-as.data.frame(Trades)
  colnames(Trades)<-c("Symbol","BuyPrice","SellDate","SellPrice")
  rownames(Trades)<-c(1:5)  
  Trades<<-Trades
}

MakeTrades()
Trades$SellPrice<-as.numeric(as.character(Trades$SellPrice))
Trades$BuyPrice<-as.numeric(as.character(Trades$BuyPrice))
Trades$Gain<-Trades$SellPrice/Trades$BuyPrice
median(Trades$Gain)
summary(Trades$Gain)