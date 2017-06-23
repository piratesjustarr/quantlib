nextDay<-function()
{
  require(chron)
  FakeToday<<-as.Date(FakeToday)+1
  while(is.weekend(FakeToday))
  {
  FakeToday<<-as.Date(FakeToday)+1  
  }
  print(FakeToday)
  Strategy1(FakeToday = FakeToday,pMargin = pmargin, slMargin = slmargin)
  StratTrades$HoldTime=as.Date(StratTrades$SellDate)-as.Date(StratTrades$BuyDate)
  print(StratTrades)
}


nextWeek<-function()
{
  FakeToday<<-as.Date(FakeToday)+7
  Strategy1(FakeToday = FakeToday,pMargin = pmargin, slMargin = slmargin)
  StratTrades$HoldTime=as.Date(StratTrades$SellDate)-as.Date(StratTrades$BuyDate)
  print(FakeToday)
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