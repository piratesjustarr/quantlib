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
  print(StratTrades)
}