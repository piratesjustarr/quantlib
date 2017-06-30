# A very simple transaction ledger

#Make a ledger as a dataframe
createLedger<-function()
{
  Ledger<-data.frame(matrix(ncol=4,nrow=0),stringsAsFactors = FALSE)
  colnames(Ledger)<-c("Date","Symbol","Price","Buy/Sell")
  Ledger<<-Ledger
}


# Add a simple buy transaction
addBuyTransaction<-function(Date,Symbol,Price)
{
  Ledger<-rbind(Ledger,c(Date,Symbol,Price,"Buy"))
  #Ledger<<-na.omit(Ledger)
}

# Add a simple sell transaction
addSellTransaction<-function(Date,Symbol,Price)
{
  Ledger<-rbind(Ledger,c(Date,Symbol,Price,"Sell"))
  #Ledger<<-na.omit(Ledger)
}