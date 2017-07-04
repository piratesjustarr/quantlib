shift<-list()
for (symbol in ls(LoadedSymbols))
{
  delta=30
  data<-Cl(LoadedSymbols[[symbol]])
  if(nrow(data)>delta)
  {
  val=median(ROC(data,n = delta),na.rm = TRUE)
  }
  else
  {
    val=0
  }
  shift[symbol]<-val
}
res<-(t(as.data.frame(shift)))

res<-res[order(res,decreasing = TRUE),]

#barChart(LoadedSymbols$`LON:CPT`)
#plot(ROC(Cl(LoadedSymbols$`LON:CPT`),n=30))