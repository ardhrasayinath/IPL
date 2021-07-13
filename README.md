# IPLhttps://github.com/ardhrasayinath/IPL/blob/main/IPL%20Ball-by-Ball%202008-2020.csv
getwd()
setwd("F:\\SCMA\\assignment1")
dir()
cric=read.csv( "IPL Ball-by-Ball 2008-2020.csv"  )
View(cric)
unique(cric$batsman)
names(cric)
dim(cric)
##subsetting batsman
library(dplyr)
runs=cric %>%
group_by(batsman,id)%>%  
summarize(score=sum(batsman_runs))  
View(runs)
runs$round = substr(runs$id, start = 1, stop = 2)
View(runs)
str(runs$round)
##subsetting bowlers
wickets  = cric %>%
group_by(bowler,id)%>%
summarize(wicket = sum(is_wicket))
View(wickets)
wickets$round = substr(wickets$id, start = 1, stop = 2)
View(wickets)
unique(wickets$round)
#Subsetting the assigned batsman and plotting his graph
hopes = runs[runs$batsman == 'JR Hopes',]
View(hopes)
dim(hopes)
sum(hopes$score)
library(MASS)
library(fitdistrplus)
hist(hopes$score,6)
descdist(hopes$score)
#subsetting the assigned bowler and plotting his graph
nar=wickets[wickets$bowler=='Y Nagar',]
View(nar)
dim(nar)
sum(nar$wicket)
hist(nar$wicket,6)


#Fitting a poisson distribution for Y Nagar 
library(vcd) # For discrete data
gf.nar <- goodfit(nar$wicket, type = "poisson", par = NULL)
summary(gf.nar)
plot(gf.nar, main= 'Y Nagar wicket distribution')

#fitting continuous distribution for JR Hopes 
library(MASS)
library(fitdistrplus)
ho<-fitdistr(hopes$score+0.00001,"weibull",lower=0.001)
ho$estimate
hopewei = rweibull(1000, shape=0.7520317, scale =19.7698839 )
plot(density(hopewei))
ks.test(hopes$score,"pweibull",scale=19.7698839,shape=0.7520317)
hist(hopes$score, prob=TRUE,col='light blue')
curve(dweibull(x,0.7520317,19.7698839),col="red",lwd=2,add=T)

          





