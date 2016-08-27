##############################
# TOTVS Data Challenge       #
# Rodolpho Talaisys Bernabel #
# August 26, 2016            #
##############################

# The data was converted to csv using a free online service
# After that, some cleaning was made manually to get rid of 
# sparse and useless portions of the data matrix

rm(list=ls())

library(chron)
library(caret)

convertcsv <- read.csv("C:/Users/Rodolpho/Dropbox/convertcsvCopy.csv")
attach(convertcsv)
summary(convertcsv) #get a sense of the data

# Data munging to get days and times separately
str(ide.dhEmi..date)
aux <- as.character(ide.dhEmi..date)
aux <- strsplit(aux,'T')
date <- unlist(aux)[2*(1:length(aux))-1]
time <- unlist(aux)[2*(1:length(aux))]
time <- substr(time,1,8)
time <- chron(time=time)
aux <- time
aux <- ifelse(aux<12:00:00,'morning',ifelse(aux<18:00:00,'afternoon','evening'))
time <- as.factor(aux)
date <- as.factor(date)
convertcsv <- data.frame(convertcsv,date,time)
convertcsv <- subset(convertcsv,select=c(complemento.valorTotal,dets.0.prod.qCom,dets.0.prod.uCom,dets.0.prod.vProd,dets.0.prod.xProd,dets.1.prod.qCom,dets.1.prod.uCom,dets.1.prod.vProd,dets.1.prod.vUnCom,date,time))

summary(convertcsv)

lm.1 <- lm(complemento.valorTotal~.,data=convertcsv)
summary(lm.1)
#The best predictors, in economic and statistical significance are:
# dets.0.prod.uCom
# dets.0.prod.xProd
# dets.1.prod.uComUN
# A customer will spend more if he orders an expresso, sashimi, suco, whisky, or yakissoba
# A customer will spend less if he orders harumaki, sake, or temaki.

#Next week's sales forecast
day <- as.Date(date)

plot(day,complemento.valorTotal)
plot(day,complemento.valorTotal,ylim=c(0,150)) # a more meaningful plot
# No pattern apparent

lm.2 <- lm(complemento.valorTotal~day)
summary(lm.2) #OLS doesn't show anything

smooth.fit <- smooth.spline(day,complemento.valorTotal)
smooth.fit
lines(smooth.fit) #There is no clear upward or downward trend
# Any previous week's sales would be a good prior for next week's

# Therefore, taking the last week's record, one can estimate next week's 
# sales to be BRZ481.96, as given by the summation below:
sum(complemento.valorTotal[1629:1635])





