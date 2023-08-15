library(ggplot2)
library(dplyr)
library(rlist)

setwd("~/ResearchCode/antiTrustData")

list.files("~/ResearchCode/antiTrustData") -> allFi

outList <- list()
searchList <- list()
beforeList <- list()
afterList <- list()
agentList <- list()
for (fi in allFi){
  if (grepl("output",fi)){read.csv(fi,header = FALSE)-> outList[[length(outList)+1]]  }
  if (grepl("before",fi)){read.csv(fi,header = FALSE)-> beforeList[[length(beforeList)+1]]  }
  if (grepl("search",fi)){read.csv(fi,header = FALSE)-> searchList[[length(searchList)+1]]  }
  if (grepl("after",fi)){read.csv(fi,header = FALSE)-> afterList[[length(afterList)+1]]  }
  if (grepl("agent",fi)){read.csv(fi,header = FALSE)-> agentList[[length(agentList)+1]]  }
}
list.rbind(outList) -> outDat
names(outDat) <- c("key","tick","agent","currEngine","duckTick","vpnTick","delTick","shareTick","optOut")

list.rbind(beforeList) -> beforeDat
names(beforeDat) <- c("key","tick","agent","beforeAct","beforeActEngine")

list.rbind(searchList) -> searchDat
names(searchDat) <- c("key","tick","agent","searchEngine","optOut","waitTime","utility")

list.rbind(afterList) -> afterDat
names(afterDat) <- c("key","tick","agent","afterAct","afterActEngine","result")

list.rbind(agentList) -> agentDat
names(agentDat) <- c("key","agent","blissPoint","selfExp","unifExp")

# step 1: merge before and after acts

merge(beforeDat,afterDat,by=c("key","tick","agent")) -> jointActs

# now merge in search information
merge(agentDat,searchDat,by=c("key","agent")) -> agentSearch
agentSearch %>% arrange(agent,tick) -> agentSearch

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
