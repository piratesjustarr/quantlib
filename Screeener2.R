shift<-list()
for (symbol in ls(LoadedSymbols))
{
  val=median(ROC(Cl(LoadedSymbols[[symbol]]),n = 30),na.rm = TRUE)
  shift[symbol]<-val
}
res<-(t(as.data.frame(shift)))

res[order(res,decreasing = FALSE),]

#barChart(LoadedSymbols$`LON:CPT`)
#plot(ROC(Cl(LoadedSymbols$`LON:CPT`),n=30))