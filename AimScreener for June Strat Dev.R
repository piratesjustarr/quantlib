library(quantmod)
AimIndex <- read.csv("./AimIndex.csv")
#Ticker<-getSymbols(AimIndex$Symbol[1],auto.assign = FALSE,src="google",from="2017-03-01")
#roc <- ROC(Cl(Ticker), n = 3, type = "discrete")
MySymbols<-new.env()
ROCSymbols<-new.env()
getSymbols(as.character(symbols), src='google', index.class=c("POSIXt","POSIXct"), from='2015-01-01',env = MySymbols)

require(quantstrat)

currency("GBP")
symbols = AimIndex$Symbol
stock(as.character(symbols), currency="GBP",multiplier=1)

#Run for saved data
loadLibraries()
loadLocalData()
MySymbols<-LoadedSymbols
symbols<-ls(MySymbols)

#Re run from here!!

FakeToday="2017-05-08"
margin=1.1
slmargin=0.95

ROCSymbols<-new.env()

#COPY Symbols for ROC analysis
for(n in ls(MySymbols, all.names=TRUE)) assign(n, get(n, MySymbols), ROCSymbols)

#Remove data past start point to avoid lookahead bias
for (symbol in ls(ROCSymbols))
{
print(paste("Stripping",symbol))
ROCSymbols[[symbol]]<-ROCSymbols[[symbol]][paste("::",FakeToday,sep="")]
print(last(ROCSymbols[[symbol]]))
}

#Attach ROC Symbols to search path
attach(ROCSymbols)

#Convert to monthly and drop all columns except Adjusted Close
for(symbol in symbols) {
  x <- get(symbol)
  if(nrow(x)>0)
  {
  x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x",symbol,colnames(x))
  x <- x[,4] #drops all columns except Adjusted Close which is 6th column
  assign(symbol,x)
  }
  else
  {
    print(paste("Skipping",symbol,"no data",sep=" "))
  }
}

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
# nametemp<-unlist(strsplit(meanranks$name,split = "[.]"))
# nametemp<-nametemp[-which(nametemp=="LON")]
# nametemp<-nametemp[-which(nametemp=="Close")]
# nametemp<-nametemp[-which(nametemp=="1")]
# nametemp<-na.exclude(nametemp)
meanranks$name<-paste("LON",nametemp,sep=":")

Top5<-head(meanranks,5)
print(Top5)

detach(ROCSymbols)

events <- xts("FakeToday", as.Date(c(FakeToday)))

Limits<-list()
for(topper in Top5$name)
{
  print(topper)
#barChart(MySymbols[[topper]][paste("::",FakeToday,sep="")],name = topper)
#addRSI()
clRSI<-RSI(Cl(MySymbols[[topper]][paste("::",FakeToday,sep="")]))
#limit to last 20 days only
clRSI<-last(clRSI,"20 days")
sdRSI<-round(sd(clRSI,na.rm = TRUE),2)
medianRSI<-round(median(clRSI,na.rm = TRUE),2)
print(paste(topper,"Current RSI",last(clRSI),"Limit RSI",medianRSI-(sdRSI*2),sep="/"))
Limits[[topper]]=medianRSI-(sdRSI*2)
# zoomChart(paste(FakeToday,"::",sep=""))
  # data<-(Cl(MySymbols[[topper]][paste(FakeToday,"::",sep="")]))
  # print(paste(topper,first(data),max(data),(max(data)/first(data)*100),sep=" "))
}


for(topper in Top5$name)
{
  barChart(MySymbols[[topper]],name = topper)
  addRSI()
  zoomChart(paste(FakeToday,"::",sep=""))
  #data<-(Cl(MySymbols[[topper]][paste(FakeToday,"::",sep="")]))
  #print(paste(topper,first(data),max(data),(max(data)/first(data)*100),sep=" "))
}

#TIME TRAVEL!!

for(topper in Top5$name)
{
  limRSI<-RSI(Cl(MySymbols[[topper]]))<Limits[[topper]]
  limRSI<-limRSI[paste(FakeToday,"::",sep="")]
  if(nrow(limRSI[which(limRSI==TRUE)])>0)
  {
  buy=index(first(limRSI[which(limRSI==TRUE)]))
  
  bprice=Cl(MySymbols[[topper]][buy])
  print(paste(topper,"buy signal at",buy,"at",bprice))
  followingHighs<-Hi(MySymbols[[topper]][paste(buy,"::",sep="")])
  sells<-followingHighs[which(followingHighs>as.numeric(bprice*margin))]
  if(nrow(sells)==0)
  {
    followingLows<-Lo(MySymbols[[topper]][paste(buy,"::",sep="")])
    stoploss<-followingLows[which(followingHighs<as.numeric(bprice*slmargin))]
    sprice=as.numeric(first(stoploss))
    print(paste("No profit target met. Stop loss on",index(first(stoploss)),"for",as.numeric(first(stoploss))))
    index(first(stoploss))-buy
  }
  else
  {
    print(paste("Profit target met. Sell on",index(first(sells)),"for",as.numeric(first(sells))))
    print(index(first(sells))-buy)
  }
  }
  else
  {
    print(paste("No opportunity to buy",topper))
  }
}


topper=Top5$name[1]
print(topper)
plot(Cl(MySymbols[[topper]]))
#par(mfrow=c(1,2))
addEventLines(events,srt=90,pos=2)
plot(Cl(MySymbols[[topper]][paste(FakeToday,"::",sep="")]))

topper=Top5$name[2]
print(topper)
plot(Cl(MySymbols[[topper]]))
#par(mfrow=c(1,2))
addEventLines(events,srt=90,pos=2)
plot(Cl(MySymbols[[topper]][paste(FakeToday,"::",sep="")]))  

topper=Top5$name[3]
print(topper)
plot(Cl(MySymbols[[topper]]))
#par(mfrow=c(1,2))
addEventLines(events,srt=90,pos=2)
plot(Cl(MySymbols[[topper]][paste(FakeToday,"::",sep="")]))

topper=Top5$name[4]
print(topper)
plot(Cl(MySymbols[[topper]]))
#par(mfrow=c(1,2))
addEventLines(events,srt=90,pos=2)
plot(Cl(MySymbols[[topper]][paste(FakeToday,"::",sep="")]))

topper=Top5$name[5]
print(topper)
plot(Cl(MySymbols[[topper]]))
#par(mfrow=c(1,2))
addEventLines(events,srt=90,pos=2)
plot(Cl(MySymbols[[topper]][paste(FakeToday,"::",sep="")]))

#write.csv(r,"Aimrank.csv")
