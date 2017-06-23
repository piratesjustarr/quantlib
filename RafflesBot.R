library(slackr)

testMessage<-function()
{
  slackrSetup()
  slackrBot("Testing, testing, 1,2,3.")
}

echoExternalIP<-function()
{
  slackrSetup()
  slackrBot(timestamp())
  myIP<-system(intern=TRUE, command="curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'  ")
  a<-(paste("I can be found at http://",myIP,":8787",sep=""))
  slackrBot(a)
}

testMessage()
echoExternalIP()
