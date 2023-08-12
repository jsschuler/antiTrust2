library(ggplot2)
library(dplyr)
library(rlist)

setwd("/home/jsschuler/Research Code/antiTrustData")

# get control file

#read.csv("ctrl.csv",header=TRUE) -> control


list.files("/home/jsschuler/Research Code/antiTrustData") -> allFi
allFi[allFi!="ctrl.csv"] -> allFi
fiList=list()
for (fi in allFi){
  read.csv(fi,header = FALSE)-> fiList[[length(fiList)+1]] -> df 
  df$key <- fi
}
list.rbind(fiList) -> runDat


names(runDat) <- c("key","tick","agtNum","engine","dickTick","vpnTick","deletionTick","sharingTick")

#control[control$key==runDat$key[1],]

runDat %>% transform(googPct=(engine=="google")) %>% group_by(key,tick) %>% summarise(googPct=mean(googPct)) -> googTime
data.frame(googTime) -> googTime
unique(googTime$key) -> allKeys
for (j in 1:length(allKeys)){
ggplot() + geom_line(aes(x=1:500,y=c(googTime[googTime$key==allKeys[[j]],"googPct"]))) + xlab("Tick") + ylab("Google Share") + xlim(0,500) + ylim(0,1.1)
ggsave(paste0(allKeys[[j]],".png"))
}


ggplot()+ ggtitle("Google Vs Duck Duck Go") +  xlab("Tick") + ylab("Google Share") +
 xlim(0,500) + ylim(0,1.1) + 
  geom_line(aes(x=1:500,y=c(googTime[googTime$key=="JTESTDuck10","googPct"])),color="red") + geom_vline(xintercept = 10,linetype="dashed",color="red") + 
  geom_line(aes(x=1:500,y=c(googTime[googTime$key=="JTESTDuck100","googPct"])),color="blue") + geom_vline(xintercept = 100,linetype="dashed",color="blue") +
  geom_line(aes(x=1:500,y=c(googTime[googTime$key=="JTESTDuck200","googPct"])),color="green") + geom_vline(xintercept = 200,linetype="dashed",color="green") +
  geom_line(aes(x=1:500,y=c(googTime[googTime$key=="JTESTDuck300","googPct"])),color="purple") + geom_vline(xintercept = 300,linetype="dashed",color="purple") +
  geom_line(aes(x=1:500,y=c(googTime[googTime$key=="JTESTDuck400","googPct"])),color="orange") + geom_vline(xintercept = 400,linetype="dashed",color="orange")


# now get min ever googPct
googTime %>% group_by(key) %>% summarise(mnGoog=min(googPct)) -> minGoog 
# get final googPct
googTime[googTime$tick==max(googTime$tick),] -> lastGoog
names(lastGoog)[[3]] <- "finGoog"
ggplot() + geom_histogram(aes(x=minGoog$mnGoog))

merge(lastGoog,minGoog,by="key") -> jointGoog
merge(jointGoog,control,by="key") -> jointGoog
ggplot() + geom_point(aes(x=jointGoog$mnGoog,y=jointGoog$googPct,color=as.numeric(jointGoog$privacyVal)))
ggplot() + geom_point(aes(x=as.numeric(jointGoog$privacyVal),y=jointGoog$mnGoog))

jointGoog %>% transform(privacyVal=as.numeric(privacyVal)) %>% group_by(privacyVal) %>% summarise(mnGoog=mean(finGoog),medGoog=quantile(finGoog,.5),q25=quantile(finGoog,.25),q75=quantile(finGoog,.75)) -> qReport

jointGoog %>% transform(switchPct=as.numeric(switchPct)) %>% group_by(switchPct) %>% summarise(mnGoog=mean(mnGoog),medGoog=quantile(mnGoog,.5),q25=quantile(mnGoog,.25),q75=quantile(mnGoog,.75)) -> qReport2

jointGoog %>% group_by(vpnTick,duckTick) %>% summarise(mnGoog=mean(finGoog),medGoog=quantile(finGoog,.5),q25=quantile(finGoog,.25),q75=quantile(finGoog,.75)) -> qReport3

quantile(googTime$googPct,c(0,.05,.25,.5,.75,.95,1))

ggplot() + geom_line(aes(x=googTime$tick,y=googTime$googPct))

ggplot() + geom_point(aes(x=as.numeric(control$switchPct),y=as.numeric(control$privacyVal)))
