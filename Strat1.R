source("Quantlib.R")

# Suppresses warnings
options("getSymbols.warning4.0" = FALSE)

# Do some house cleaning
rm(list = ls(.blotter), envir = .blotter)

# Set the currency and the timezone
currency('GBP')
Sys.setenv(TZ = "UTC")

# Define symbols of interest
symbols <- c("LON:IQE","LON:JAY",
  "LON:SOLG","LON:CHAR","LON:CGH"
  ,"LON:SQZ","LON:CPP","LON:AEN",
  "LON:THR","LON:PURP"
)

#Load all locally stored symbol data
loadLocalData()

#Get copies of what we care about and attach to the global environment
StratSym<-new.env()
for (symbol in symbols)
{
  StratSym[[symbol]]<-LoadedSymbols[[symbol]]
}

attach(StratSym)

from="2016-01-01"
to="2017-05-05"

# Trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize * length(symbols)


# Define the instrument type
stock(symbols, currency = "GBP", multiplier = 1)


# Set up the strategy, portfolio, and order book

rm(list = ls(.blotter), envir = .blotter)
initDate = '1990-01-01'
#initEq = 10000

strategy.st <- "Strat"
portfolio.st <- "Strat"
account.st <- "Strat"

rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = 'GBP')
initAcct(account.st, portfolios = portfolio.st,initDate = initDate, currency = 'GBP', initEq = initEq)
initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)


#Add two indicators. Fast and Slow MA at 50 and 20
add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 20), label = "qsma")

add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 50), label = "ssma")


#Add a signal, is the fast SMA bigger than the slow SMA??

add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("SMA.qsma", "SMA.ssma"),
                            relationship = "gt"), label = "qsma.gt.ssma")

#Or the other way around?
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("SMA.qsma", "SMA.ssma"),
                                                                 relationship = "lt"), label = "qsma.lt.ssma")


add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "qsma.gt.ssma", sigval = TRUE,
                          ordertype = "market", orderside  ="long", replace = FALSE,
                          prefer = "Open", tradeSize = tradeSize
                          ), type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "qsma.lt.ssma", sigval = TRUE,
                          ordertype = "market", orderside  ="short", replace = FALSE,
                          prefer = "Open", tradeSize = tradeSize
         ), type = "enter", path.dep = TRUE)

# Apply Strategy
t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st,
                     portfolios = portfolio.st)
t2 <- Sys.time()
print(t2 - t1)

# Set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)