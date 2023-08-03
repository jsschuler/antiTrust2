setwd("/home/jsschuler/Research Code/antiTrustData")
read.csv("output2023-08-03T18:46:43.107-600-4361-1.csv",header = FALSE)-> runDat
names(runDat) <- c("key","tick","agtNum","engine","dickTick","vpnTick","deletionTick","sharingTick")
library(ggplot2)
library(dplyr)

runDat %>% transform(googPct=engine=="google") %>% group_by(tick) %>% summarise(googPct=mean(googPct)) -> googTime

ggplot() + geom_line(aes(x=googTime$tick,y=googTime$googPct))
