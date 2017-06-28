createLedger<-function()
{
  Ledger<-data.frame(matrix(ncol=4,nrow=1),stringsAsFactors = FALSE)
  colnames(Ledger)<-c("Date","Symbol","Price","Buy/Sell")
  Ledger<<-Ledger
}

addBuyTransaction<-function(Date,Symbol,Price)
{
  Ledger<-rbind(Ledger,c(Date,Symbol,Price,"Buy"))
  Ledger<<-na.omit(Ledger)
}


addSellTransaction<-function(Date,Symbol,Price)
{
  Ledger<<-rbind(Ledger,c(Date,Symbol,Price,"Sell"))
  Ledger<<-na.omit(Ledger)
}