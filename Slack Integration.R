#Slack integration stuff

install.packages(slackr)
library(slackr)

#### ~./slackr config for user required also.

slackrSetup()

slackrBot("Raffles here.")
