source("Strategy Header.R")

Strategy2<-function(FakeToday,pMargin,slMargin)
{
  ROCSymbols<-new.env()
  
  #COPY Symbols for ROC analysis
  for(n in ls(MySymbols, all.names=TRUE)) assign(n, get(n, MySymbols), ROCSymbols)
  
  #Remove data past start point to avoid lookahead bias
  for (symbol in ls(ROCSymbols))
  {
    #print(paste("Stripping future data from",symbol))
    ROCSymbols[[symbol]]<-ROCSymbols[[symbol]][paste("::",FakeToday,sep="")]
    #print(last(ROCSymbols[[symbol]]))
  }
  
  #Convert to monthly and drop all columns except Adjusted Close
  for(symbol in symbols) {
    x <- get(symbol,envir = ROCSymbols)
    if(nrow(x)>0)
    {
      x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
      indexFormat(x) <- '%Y-%m-%d'
      colnames(x) <- gsub("x",symbol,colnames(x))
      x <- x[,4] #drops all columns except Adjusted Close which is 6th column
      assign(symbol,x,envir = ROCSymbols)
    }
    else
    {
      print(paste("Skipping",symbol,"no data",sep=" "))
    }
  }
  
  #Attach ROC Symbols to search path
  attach(ROCSymbols)
  #merge the symbols into a single object with just the close prices
  symbols_close <- do.call(merge, lapply(as.character(symbols), get))
  
  #xts object of the 3 period ROC of each column in the close object
  #The 3 period ROC will be used as the ranking factor
  roc <- ROC(symbols_close, n = 3, type = "discrete")
  
  #xts object with ranks
  #symbol with a rank of 1 has the highest ROC
  r <- as.xts(t(apply(-roc, 1, rank)))
  
  meanranks<-as.data.frame(colMeans(r))
  meanranks$name<-rownames(meanranks)
  meanranks<-meanranks[order(meanranks),]
  nametemp<-substr(meanranks$name,start = 5,stop=(nchar(meanranks$name)-6))
  meanranks$name<-paste("LON",nametemp,sep=":")
  
  Top5<-head(meanranks,5)
  print(Top5)
  
  detach(ROCSymbols)
  
  events <- xts("FakeToday", as.Date(c(FakeToday)))
  
  Limits<-list()
  for(topper in Top5$name)
  {
    clROC<-ROC(Cl(MySymbols[[topper]][paste("::",FakeToday,sep="")]))
    #limit to last 2 months only
    clROC<-last(clROC,"2 months")
    shortSMA<-SMA(clROC,5)
    longSMA<-SMA(clROC,20)
    print(paste(topper,"Current shortSMAROC",last(shortSMA),"Current longSMAROC",last(longSMA)),sep="/")
  }
  
  
  # for(topper in Top5$name)
  # {
  #   barChart(MySymbols[[topper]],name = topper)
  #   addRSI()
  #   zoomChart(paste(FakeToday,"::",sep=""))
  # }
  
  Top5$BuyPrice=0
  Top5$SellPrice=0
  Top5$BuyDate=""
  Top5$SellDate=""
  Top5$Event=""
  
  for(topper in Top5$name)
  {
    #Flag which RSI events cross limit using which
    limSMAROC<-SMA(ROC(Cl(MySymbols[[topper]])),20)>SMA(ROC(Cl(MySymbols[[topper]])),5)
    plot(SMA(ROC(Cl(MySymbols[[topper]])),20))
    plot(SMA(ROC(Cl(MySymbols[[topper]])),5))
    #Now checking only the future
    limSMAROC<-limSMAROC[paste(FakeToday,"::",sep="")]
    
    #Check if any buy signal occurred
    if(nrow(limSMAROC[which(limSMAROC==TRUE)])>0)
    {
      #Ptherwise go with first signal generated
      buy=index(first(limSMAROC[which(limSMAROC==TRUE)]))
      
      #Use day closing price as nominal buy price
      bprice=as.numeric(Cl(MySymbols[[topper]][buy]))
      print(paste(topper,"buy signal at",buy,"at",bprice))
      
      #Record buy price and date
      Top5[Top5$name==topper,]$BuyPrice=bprice
      Top5[Top5$name==topper,]$BuyDate=as.character(buy)
      
      #Look ahead for days where Hi exceeds the profit margin.
      followingHighs<-Hi(MySymbols[[topper]][paste(buy,"::",sep="")])
      sells<-followingHighs[which(followingHighs>as.numeric(bprice*pmargin))]
      
      #Oh No! no sell signal was seen, better bail out!
      if(nrow(sells)==0)
      {
        followingLows<-Lo(MySymbols[[topper]][paste(buy,"::",sep="")])
        stoploss<-followingLows[which(followingHighs<as.numeric(bprice*slmargin))]
        sprice=as.numeric(first(stoploss))
        print(paste("No profit target met. Stop loss on",index(first(stoploss)),"for",as.numeric(first(stoploss))))
        index(first(stoploss))-buy
        
        #Record stop loss triggered
        Top5[Top5$name==topper,]$SellPrice=as.numeric(bprice*slmargin)
        #Top5[Top5$name==topper,]$SellDate=index(first(stoploss))
        Top5[Top5$name==topper,]$Event="STOP LOSS"
        
      }
      else
      {
        print(paste("Profit target met. Sell on",index(first(sells)),"for",as.numeric(first(sells))))
        print(index(first(sells))-buy)
        
        #Record successful sell
        Top5[Top5$name==topper,]$SellDate=as.character(index(first(sells)))
        Top5[Top5$name==topper,]$SellPrice=bprice*pMargin#as.numeric(first(sells))
        Top5[Top5$name==topper,]$Event="Exited Long"
      }
    }
    else
    {
      #No Buy Signal!
      print(paste("No opportunity to buy",topper))
      Top5[Top5$name==topper,]$BuyPrice=NA
      Top5[Top5$name==topper,]$SellPrice=NA
      Top5[Top5$name==topper,]$BuyDate=NA
      Top5[Top5$name==topper,]$SellDate=NA
      Top5[Top5$name==topper,]$Event="Failed to open Long"
    }
  }
  Top5$Gain<-Top5$SellPrice/Top5$BuyPrice
  StratTrades<<-Top5
}  

nextWeek<-function()
{
  FakeToday<<-as.Date(FakeToday)+7
  print(FakeToday)
  Strategy2(FakeToday = FakeToday,pMargin = pmargin, slMargin = slmargin)
  StratTrades$HoldTime=as.Date(StratTrades$SellDate)-as.Date(StratTrades$BuyDate)
  print(StratTrades)
}

nextDate<-function(newDate)
{
  FakeToday<<-newDate
  print(FakeToday)
  Strategy2(FakeToday = FakeToday,pMargin = pmargin, slMargin = slmargin)
  StratTrades$HoldTime=as.Date(StratTrades$SellDate)-as.Date(StratTrades$BuyDate)
  print(newDate)
  print(StratTrades)
}

## Strategy Output

FakeToday="2017-03-01"
print(paste("Checking: ",FakeToday))
pmargin=1.1
slmargin=0.95

system.time(Strategy2(FakeToday = FakeToday,pMargin = pmargin, slMargin = slmargin))
#StratTrades$HoldTime=as.Date(StratTrades$SellDate)-as.Date(StratTrades$BuyDate)
#print(StratTrades)

