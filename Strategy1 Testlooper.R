TradeRecords<-StratTrades

nextWeek<-function()
{
  FakeToday<<-as.Date(FakeToday)+7
  print(FakeToday)
  Strategy1(FakeToday = FakeToday,pMargin = pmargin, slMargin = slmargin)
  StratTrades$HoldTime=as.Date(StratTrades$SellDate)-as.Date(StratTrades$BuyDate)
  print(StratTrades)
}

nextDate<-function(newDate)
{
  FakeToday<<-newDate
  print(FakeToday)
  Strategy1(FakeToday = FakeToday,pMargin = pmargin, slMargin = slmargin)
  StratTrades$HoldTime=as.Date(StratTrades$SellDate)-as.Date(StratTrades$BuyDate)
  print(newDate)
  print(StratTrades)
}

testloop<-function()
{
  for(i in 1:20)
  {
  nextWeek()
  TradeRecords<-rbind(TradeRecords,StratTrades)
  }
  summary(TradeRecords)
}